#!/usr/bin/env python3
"""
Analyze when maximum butterfly counts occur relative to sunrise
Creates histogram showing hours since sunrise when max count occurred
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from datetime import datetime, timedelta
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


def load_all_sunrise_times(json_dir: str = 'data/deployments'):
    """Load all sunrise times for all deployments and dates"""

    sunrise_times = {}
    json_path = Path(json_dir)

    for json_file in json_path.glob('*.json'):
        deployment_id = json_file.stem
        print(f"  Loading sunrise times for {deployment_id}...")

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

        # Find sunrise for each date
        for date_key, observations in daily_obs.items():
            # Sort by timestamp
            observations = sorted(observations, key=lambda x: x['timestamp'])

            # Find first non-night observation (sunrise)
            for obs in observations:
                if not obs['is_night']:
                    key = (deployment_id, date_key)
                    sunrise_times[key] = obs['timestamp']
                    break

    return sunrise_times


def calculate_hours_since_sunrise():
    """Calculate hours since sunrise for each maximum butterfly count"""

    # Load the sunset window data
    print("Loading sunset window data...")
    lag_df = pd.read_csv('data/monarch_daily_lag_analysis_sunset_window.csv')

    # Convert date and time columns to datetime
    lag_df['date_t'] = pd.to_datetime(lag_df['date_t'])
    lag_df['time_of_max_t'] = pd.to_datetime(lag_df['time_of_max_t'])

    # Get unique deployment-date combinations
    unique_days = lag_df[['deployment_id', 'date_t', 'time_of_max_t', 'max_butterflies_t']].drop_duplicates()

    # Filter out days with zero butterflies
    unique_days = unique_days[unique_days['max_butterflies_t'] > 0]

    print(f"Processing {len(unique_days)} unique deployment-days with butterflies...")

    # Load all sunrise times at once
    print("Loading sunrise times from all deployments...")
    sunrise_times_dict = load_all_sunrise_times()
    print(f"  Loaded {len(sunrise_times_dict)} sunrise times")

    hours_since_sunrise = []

    for idx, row in unique_days.iterrows():
        deployment_id = row['deployment_id']
        date = row['date_t']
        time_of_max = row['time_of_max_t']

        # Find sunrise time from pre-loaded dictionary
        key = (deployment_id, date.date())
        sunrise_time = sunrise_times_dict.get(key)

        if sunrise_time:
            # Calculate hours since sunrise
            time_diff = time_of_max - sunrise_time
            hours = time_diff.total_seconds() / 3600

            if hours >= 0:  # Only include if max occurred after sunrise
                hours_since_sunrise.append({
                    'deployment_id': deployment_id,
                    'date': date,
                    'sunrise_time': sunrise_time,
                    'time_of_max': time_of_max,
                    'hours_since_sunrise': hours,
                    'max_butterflies': row['max_butterflies_t']
                })

    return pd.DataFrame(hours_since_sunrise)


def create_histogram(df):
    """Create histogram of hours since sunrise when max butterfly count occurred"""

    # Create figure with improved styling
    fig, ax = plt.subplots(figsize=(12, 7))

    # Create histogram
    n, bins, patches = ax.hist(df['hours_since_sunrise'],
                               bins=np.arange(0, df['hours_since_sunrise'].max() + 0.5, 0.5),
                               edgecolor='black',
                               alpha=0.7,
                               color='orange')

    # Add vertical lines for mean and median
    mean_hours = df['hours_since_sunrise'].mean()
    median_hours = df['hours_since_sunrise'].median()

    ax.axvline(mean_hours, color='red', linestyle='--', linewidth=2,
               label=f'Mean: {mean_hours:.1f} hours')
    ax.axvline(median_hours, color='blue', linestyle='--', linewidth=2,
               label=f'Median: {median_hours:.1f} hours')

    # Formatting
    ax.set_xlabel('Hours Since Sunrise', fontsize=12)
    ax.set_ylabel('Number of Days', fontsize=12)
    ax.set_title('Timing of Maximum Daily Butterfly Count Relative to Sunrise',
                 fontsize=14, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend()

    # Add statistics text box
    stats_text = f'n = {len(df)} days\n'
    stats_text += f'Range: {df["hours_since_sunrise"].min():.1f} - {df["hours_since_sunrise"].max():.1f} hours\n'
    stats_text += f'Std Dev: {df["hours_since_sunrise"].std():.1f} hours'

    ax.text(0.98, 0.97, stats_text,
            transform=ax.transAxes,
            fontsize=10,
            verticalalignment='top',
            horizontalalignment='right',
            bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.8))

    plt.tight_layout()

    # Save figure
    output_path = 'analysis/reports/max_count_timing_histogram.png'
    plt.savefig(output_path, dpi=300, bbox_inches='tight')
    print(f"\nHistogram saved to: {output_path}")

    # Also save as HTML for interactive viewing
    html_path = 'analysis/reports/max_count_timing_analysis.html'
    create_html_report(df, html_path)

    plt.show()

    return fig


def create_html_report(df, output_path):
    """Create an HTML report with the analysis results"""

    html_content = f"""
    <!DOCTYPE html>
    <html>
    <head>
        <title>Maximum Butterfly Count Timing Analysis</title>
        <style>
            body {{ font-family: Arial, sans-serif; margin: 40px; }}
            h1 {{ color: #333; }}
            .stats {{ background-color: #f5f5f5; padding: 20px; border-radius: 5px; }}
            table {{ border-collapse: collapse; width: 100%; margin-top: 20px; }}
            th, td {{ border: 1px solid #ddd; padding: 12px; text-align: left; }}
            th {{ background-color: #4CAF50; color: white; }}
            tr:nth-child(even) {{ background-color: #f2f2f2; }}
            .plot {{ text-align: center; margin: 30px 0; }}
        </style>
    </head>
    <body>
        <h1>Maximum Daily Butterfly Count Timing Relative to Sunrise</h1>

        <div class="stats">
            <h2>Summary Statistics</h2>
            <p><strong>Total Days Analyzed:</strong> {len(df)}</p>
            <p><strong>Mean Time After Sunrise:</strong> {df['hours_since_sunrise'].mean():.2f} hours</p>
            <p><strong>Median Time After Sunrise:</strong> {df['hours_since_sunrise'].median():.2f} hours</p>
            <p><strong>Standard Deviation:</strong> {df['hours_since_sunrise'].std():.2f} hours</p>
            <p><strong>Range:</strong> {df['hours_since_sunrise'].min():.2f} - {df['hours_since_sunrise'].max():.2f} hours</p>
        </div>

        <div class="plot">
            <img src="max_count_timing_histogram.png" alt="Histogram" style="max-width: 100%; height: auto;">
        </div>

        <h2>Distribution by Hour Bins</h2>
        <table>
            <tr>
                <th>Hours Since Sunrise</th>
                <th>Number of Days</th>
                <th>Percentage</th>
            </tr>
    """

    # Create hourly bins for the table
    bins = np.arange(0, df['hours_since_sunrise'].max() + 1, 1)
    hist, _ = np.histogram(df['hours_since_sunrise'], bins=bins)

    for i, count in enumerate(hist):
        percentage = (count / len(df)) * 100
        html_content += f"""
            <tr>
                <td>{bins[i]:.0f} - {bins[i+1]:.0f}</td>
                <td>{count}</td>
                <td>{percentage:.1f}%</td>
            </tr>
        """

    html_content += """
        </table>

        <h2>Top 10 Earliest Peak Times</h2>
        <table>
            <tr>
                <th>Deployment</th>
                <th>Date</th>
                <th>Hours After Sunrise</th>
                <th>Max Count</th>
            </tr>
    """

    # Add top 10 earliest peaks
    earliest = df.nsmallest(10, 'hours_since_sunrise')
    for _, row in earliest.iterrows():
        html_content += f"""
            <tr>
                <td>{row['deployment_id']}</td>
                <td>{row['date'].strftime('%Y-%m-%d')}</td>
                <td>{row['hours_since_sunrise']:.2f}</td>
                <td>{row['max_butterflies']:.0f}</td>
            </tr>
        """

    html_content += """
        </table>

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
    print("Analyzing when peak counts occur relative to sunrise")
    print("="*60)

    # Calculate hours since sunrise for each max count
    results_df = calculate_hours_since_sunrise()

    if results_df.empty:
        print("No valid data found for analysis")
        return

    print(f"\nSuccessfully processed {len(results_df)} days with butterfly observations")

    # Print summary statistics
    print("\nSummary Statistics:")
    print(f"  Mean time after sunrise: {results_df['hours_since_sunrise'].mean():.2f} hours")
    print(f"  Median time after sunrise: {results_df['hours_since_sunrise'].median():.2f} hours")
    print(f"  Earliest peak: {results_df['hours_since_sunrise'].min():.2f} hours after sunrise")
    print(f"  Latest peak: {results_df['hours_since_sunrise'].max():.2f} hours after sunrise")
    print(f"  Standard deviation: {results_df['hours_since_sunrise'].std():.2f} hours")

    # Save the processed data
    results_df.to_csv('data/max_count_timing_analysis.csv', index=False)
    print(f"\nDetailed results saved to: data/max_count_timing_analysis.csv")

    # Create histogram
    create_histogram(results_df)

    print("\n" + "="*60)
    print("ANALYSIS COMPLETE")
    print("="*60)


if __name__ == "__main__":
    main()