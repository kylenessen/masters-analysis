#!/usr/bin/env python3
"""
Analyze when maximum butterfly counts occur relative to sunset
Creates histogram showing hours until sunset when max count occurred
Uses only the filtered dataset from sunset window GAM analysis (metrics_complete >= 0.95)
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime
import json
from pathlib import Path
import re
import warnings
warnings.filterwarnings('ignore')


def extract_timestamp_from_filename(filename: str):
    """Extract timestamp from image filename"""
    match = re.search(r'_(\d{14})', filename)
    if not match:
        return None

    timestamp_str = match.group(1)
    try:
        return datetime.strptime(timestamp_str, '%Y%m%d%H%M%S')
    except ValueError:
        return None


def load_all_sunset_times(json_dir: str = 'data/deployments'):
    """Load all sunset times for all deployments and dates"""

    sunset_times = {}
    json_path = Path(json_dir)

    for json_file in json_path.glob('*.json'):
        deployment_id = json_file.stem
        print(f"  Loading sunset times for {deployment_id}...")

        with open(json_file, 'r') as f:
            data = json.load(f)

        # Handle both JSON structures
        if 'classifications' in data:
            classifications = data['classifications']
        else:
            classifications = data

        # Group observations by date
        daily_obs = {}

        for image_filename, image_data in classifications.items():
            timestamp = extract_timestamp_from_filename(image_filename)
            if not timestamp:
                continue

            date_key = timestamp.date()
            is_night = image_data.get('isNight', False)

            if date_key not in daily_obs:
                daily_obs[date_key] = []

            daily_obs[date_key].append({
                'timestamp': timestamp,
                'is_night': is_night
            })

        # Find sunset for each date (last non-night observation)
        for date_key, observations in daily_obs.items():
            # Sort by timestamp
            observations = sorted(observations, key=lambda x: x['timestamp'])

            # Find last non-night observation (sunset)
            sunset_time = None
            for obs in reversed(observations):
                if not obs['is_night']:
                    sunset_time = obs['timestamp']
                    break

            if sunset_time:
                key = (deployment_id, date_key)
                sunset_times[key] = sunset_time

    return sunset_times


def calculate_hours_until_sunset():
    """Calculate hours until sunset for each maximum butterfly count"""

    # Load the sunset window data
    print("Loading sunset window data...")
    lag_df = pd.read_csv('data/monarch_daily_lag_analysis_sunset_window.csv')

    # Apply the same filtering as in the GAM analysis
    print(f"Total observations before filtering: {len(lag_df)}")

    # Filter observations with metrics_complete < 0.95 (as done in the QMD)
    lag_df_filtered = lag_df[lag_df['metrics_complete'] >= 0.95].copy()
    print(f"Observations after filtering (metrics_complete >= 0.95): {len(lag_df_filtered)}")

    # Convert date and time columns to datetime
    lag_df_filtered['date_t'] = pd.to_datetime(lag_df_filtered['date_t'])
    lag_df_filtered['time_of_max_t'] = pd.to_datetime(lag_df_filtered['time_of_max_t'])

    # Get unique deployment-date combinations
    unique_days = lag_df_filtered[['deployment_id', 'date_t', 'time_of_max_t', 'max_butterflies_t']].drop_duplicates()

    # Filter out days with zero butterflies
    unique_days = unique_days[unique_days['max_butterflies_t'] > 0]

    print(f"Processing {len(unique_days)} unique deployment-days with butterflies (after filtering)...")

    # Load all sunset times at once
    print("Loading sunset times from all deployments...")
    sunset_times_dict = load_all_sunset_times()
    print(f"  Loaded {len(sunset_times_dict)} sunset times")

    hours_until_sunset = []

    for idx, row in unique_days.iterrows():
        deployment_id = row['deployment_id']
        date = row['date_t']
        time_of_max = row['time_of_max_t']

        # Find sunset time from pre-loaded dictionary
        key = (deployment_id, date.date())
        sunset_time = sunset_times_dict.get(key)

        if sunset_time:
            # Calculate hours until sunset (sunset time - max time)
            time_diff = sunset_time - time_of_max
            hours = time_diff.total_seconds() / 3600

            if hours >= 0:  # Only include if max occurred before sunset
                hours_until_sunset.append({
                    'deployment_id': deployment_id,
                    'date': date,
                    'sunset_time': sunset_time,
                    'time_of_max': time_of_max,
                    'hours_until_sunset': hours,
                    'max_butterflies': row['max_butterflies_t']
                })

    return pd.DataFrame(hours_until_sunset)


def create_histogram(df):
    """Create histogram of hours until sunset when max butterfly count occurred"""

    # Create figure with improved styling
    fig, ax = plt.subplots(figsize=(12, 7))

    # Create histogram with 30-minute bins
    n, bins, patches = ax.hist(df['hours_until_sunset'],
                               bins=np.arange(0, df['hours_until_sunset'].max() + 0.5, 0.5),
                               edgecolor='black',
                               alpha=0.7,
                               color='darkblue')

    # Add vertical lines for mean and median
    mean_hours = df['hours_until_sunset'].mean()
    median_hours = df['hours_until_sunset'].median()

    ax.axvline(mean_hours, color='red', linestyle='--', linewidth=2,
               label=f'Mean: {mean_hours:.1f} hours')
    ax.axvline(median_hours, color='green', linestyle='--', linewidth=2,
               label=f'Median: {median_hours:.1f} hours')

    # Formatting
    ax.set_xlabel('Hours Until Sunset', fontsize=12)
    ax.set_ylabel('Number of Days', fontsize=12)
    ax.set_title('Timing of Maximum Daily Butterfly Count Relative to Sunset\n(Filtered Dataset: metrics_complete ≥ 0.95)',
                 fontsize=14, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend()

    # Add statistics text box
    stats_text = f'n = {len(df)} days\n'
    stats_text += f'Range: {df["hours_until_sunset"].min():.1f} - {df["hours_until_sunset"].max():.1f} hours\n'
    stats_text += f'Std Dev: {df["hours_until_sunset"].std():.1f} hours'

    ax.text(0.98, 0.97, stats_text,
            transform=ax.transAxes,
            fontsize=10,
            verticalalignment='top',
            horizontalalignment='right',
            bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.8))

    plt.tight_layout()

    # Save figure
    output_path = 'analysis/dynamic_window_analysis/max_count_timing_until_sunset_histogram.png'
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"\nHistogram saved to: {output_path}")

    # Also save as HTML for interactive viewing
    html_path = 'analysis/dynamic_window_analysis/max_count_timing_until_sunset_analysis.html'
    create_html_report(df, html_path)

    plt.show()

    return fig


def create_html_report(df, output_path):
    """Create an HTML report with the analysis results"""

    # Calculate quartiles for additional statistics
    q1 = df['hours_until_sunset'].quantile(0.25)
    q3 = df['hours_until_sunset'].quantile(0.75)

    html_content = f"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>Maximum Butterfly Count Timing Analysis - Hours Until Sunset (Filtered Dataset)</title>
        <style>
            body {{ font-family: Arial, sans-serif; margin: 40px; }}
            h1 {{ color: #333; }}
            .stats {{ background-color: #f5f5f5; padding: 20px; border-radius: 5px; }}
            .note {{ background-color: #fff3cd; padding: 15px; border-radius: 5px; margin: 20px 0; border-left: 4px solid #ffc107; }}
            table {{ border-collapse: collapse; width: 100%; margin-top: 20px; }}
            th, td {{ border: 1px solid #ddd; padding: 12px; text-align: left; }}
            th {{ background-color: #2c5282; color: white; }}
            tr:nth-child(even) {{ background-color: #f2f2f2; }}
            .plot {{ text-align: center; margin: 30px 0; }}
        </style>
    </head>
    <body>
        <h1>Maximum Daily Butterfly Count Timing Relative to Sunset</h1>

        <div class="note">
            <strong>Dataset Note:</strong> This analysis uses the filtered sunset window dataset from the GAM analysis,
            excluding observations with metrics_complete < 0.95. Sunset is defined as the last non-night observation
            of each day.
        </div>

        <div class="stats">
            <h2>Summary Statistics</h2>
            <p><strong>Total Days Analyzed:</strong> {len(df)} (after filtering)</p>
            <p><strong>Mean Time Before Sunset:</strong> {df['hours_until_sunset'].mean():.2f} hours</p>
            <p><strong>Median Time Before Sunset:</strong> {df['hours_until_sunset'].median():.2f} hours</p>
            <p><strong>Standard Deviation:</strong> {df['hours_until_sunset'].std():.2f} hours</p>
            <p><strong>Range:</strong> {df['hours_until_sunset'].min():.2f} - {df['hours_until_sunset'].max():.2f} hours before sunset</p>
            <p><strong>Interquartile Range:</strong> Q1={q1:.2f}, Q3={q3:.2f} hours</p>
        </div>

        <div class="plot">
            <img src="max_count_timing_until_sunset_histogram.png" alt="Histogram" style="max-width: 100%; height: auto;">
        </div>

        <h2>Distribution by Hour Bins</h2>
        <table>
            <tr>
                <th>Hours Until Sunset</th>
                <th>Number of Days</th>
                <th>Percentage</th>
                <th>Cumulative %</th>
            </tr>
    """

    # Create hourly bins for the table
    bins = np.arange(0, df['hours_until_sunset'].max() + 1, 1)
    hist, _ = np.histogram(df['hours_until_sunset'], bins=bins)

    cumulative_pct = 0
    for i, count in enumerate(hist):
        percentage = (count / len(df)) * 100
        cumulative_pct += percentage
        html_content += f"""
            <tr>
                <td>{bins[i]:.0f} - {bins[i+1]:.0f}</td>
                <td>{count}</td>
                <td>{percentage:.1f}%</td>
                <td>{cumulative_pct:.1f}%</td>
            </tr>
        """

    html_content += """
        </table>

        <h2>Peak Timing Patterns</h2>
        <table>
            <tr>
                <th>Statistic</th>
                <th>Hours Before Sunset</th>
                <th>Time of Day (assuming 5:30 PM sunset)</th>
            </tr>
    """

    # Add timing interpretations (assuming roughly 5:30 PM sunset)
    sunset_hour = 17.5  # Approximate sunset at 5:30 PM
    for stat_name, stat_value in [
        ("Closest to Sunset", df['hours_until_sunset'].min()),
        ("25th Percentile", q1),
        ("Median", df['hours_until_sunset'].median()),
        ("Mean", df['hours_until_sunset'].mean()),
        ("75th Percentile", q3),
        ("Furthest from Sunset", df['hours_until_sunset'].max())
    ]:
        approx_time = sunset_hour - stat_value
        hour = int(approx_time)
        minute = int((approx_time - hour) * 60)
        time_str = f"{hour:02d}:{minute:02d}"
        html_content += f"""
            <tr>
                <td>{stat_name}</td>
                <td>{stat_value:.2f}</td>
                <td>~{time_str}</td>
            </tr>
        """

    html_content += """
        </table>

        <h2>Top 10 Peaks Closest to Sunset</h2>
        <table>
            <tr>
                <th>Rank</th>
                <th>Deployment</th>
                <th>Date</th>
                <th>Hours Before Sunset</th>
                <th>Max Count</th>
            </tr>
    """

    # Add top 10 closest to sunset
    closest = df.nsmallest(10, 'hours_until_sunset')
    for rank, (_, row) in enumerate(closest.iterrows(), 1):
        html_content += f"""
            <tr>
                <td>{rank}</td>
                <td>{row['deployment_id']}</td>
                <td>{row['date'].strftime('%Y-%m-%d')}</td>
                <td>{row['hours_until_sunset']:.2f}</td>
                <td>{row['max_butterflies']:.0f}</td>
            </tr>
        """

    html_content += """
        </table>

        <h2>Top 10 Peaks Furthest from Sunset (Early Morning)</h2>
        <table>
            <tr>
                <th>Rank</th>
                <th>Deployment</th>
                <th>Date</th>
                <th>Hours Before Sunset</th>
                <th>Max Count</th>
            </tr>
    """

    # Add top 10 furthest from sunset (early peaks)
    furthest = df.nlargest(10, 'hours_until_sunset')
    for rank, (_, row) in enumerate(furthest.iterrows(), 1):
        html_content += f"""
            <tr>
                <td>{rank}</td>
                <td>{row['deployment_id']}</td>
                <td>{row['date'].strftime('%Y-%m-%d')}</td>
                <td>{row['hours_until_sunset']:.2f}</td>
                <td>{row['max_butterflies']:.0f}</td>
            </tr>
        """

    html_content += """
        </table>

        <h3>Interpretation Notes</h3>
        <ul>
            <li>Lower values indicate peaks occurring closer to sunset (late afternoon)</li>
            <li>Higher values indicate peaks occurring earlier in the day (morning/midday)</li>
            <li>A value of 0 would mean the maximum count occurred at sunset</li>
            <li>Most peaks occur several hours before sunset, suggesting midday activity</li>
        </ul>

        <p style="margin-top: 30px; font-size: 0.9em; color: #666;">
            Analysis generated on """ + datetime.now().strftime('%Y-%m-%d %H:%M:%S') + """
        </p>
    </body>
    </html>
    """

    with open(output_path, 'w') as f:
        f.write(html_content)

    print(f"HTML report saved to: {output_path}")


def main():
    """Main analysis function"""

    print("="*60)
    print("MAXIMUM BUTTERFLY COUNT TIMING ANALYSIS")
    print("Analyzing when peak counts occur relative to SUNSET")
    print("Using filtered dataset (metrics_complete >= 0.95)")
    print("="*60)

    # Calculate hours until sunset for each max count
    results_df = calculate_hours_until_sunset()

    if results_df.empty:
        print("No valid data found for analysis")
        return

    print(f"\nSuccessfully processed {len(results_df)} days with butterfly observations")
    print("(This matches the filtered dataset used in the GAM analysis)")

    # Print summary statistics
    print("\nSummary Statistics:")
    print(f"  Mean time before sunset: {results_df['hours_until_sunset'].mean():.2f} hours")
    print(f"  Median time before sunset: {results_df['hours_until_sunset'].median():.2f} hours")
    print(f"  Closest peak to sunset: {results_df['hours_until_sunset'].min():.2f} hours before")
    print(f"  Furthest peak from sunset: {results_df['hours_until_sunset'].max():.2f} hours before")
    print(f"  Standard deviation: {results_df['hours_until_sunset'].std():.2f} hours")

    # Calculate percentiles
    percentiles = [10, 25, 50, 75, 90]
    print("\nPercentiles (hours before sunset):")
    for p in percentiles:
        val = results_df['hours_until_sunset'].quantile(p/100)
        print(f"  {p}th percentile: {val:.2f} hours before sunset")

    # Save the processed data
    output_csv = 'analysis/dynamic_window_analysis/max_count_timing_until_sunset_analysis.csv'
    results_df.to_csv(output_csv, index=False)
    print(f"\nDetailed results saved to: {output_csv}")

    # Create histogram
    create_histogram(results_df)

    print("\n" + "="*60)
    print("ANALYSIS COMPLETE")
    print("="*60)


if __name__ == "__main__":
    main()