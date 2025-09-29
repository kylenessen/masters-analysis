#!/usr/bin/env python3
"""
Daily lag analysis data preprocessing for monarch butterfly study
Creates day-to-day comparisons with previous day's weather as predictors
"""

import pandas as pd
import numpy as np
import json
import re
import sqlite3
import argparse
import matplotlib.pyplot as plt
from datetime import datetime
from pathlib import Path
from typing import Dict, Tuple, Optional
import warnings
warnings.filterwarnings('ignore')

try:
    import seaborn as sns
    HAS_SEABORN = True
except ImportError:
    HAS_SEABORN = False


class DailyButterflyProcessor:
    """Process butterfly count data aggregated to daily level"""

    # Count mapping: text labels to numeric values (conservative minimums)
    COUNT_MAPPING = {
        '0': 0,
        '1-9': 1,
        '10-99': 10,
        '100-999': 100,
        '1000+': 1000
    }

    # Night periods for specific deployments (hardcoded as per R logic)
    NIGHT_PERIODS = {
        'SC1': [
            ('20231117174001', '20231118062001'),
            ('20231118172501', '20231119061501'),
            ('20231119171001', '20231120062001'),
            ('20231120172001', '20231121063001')
        ],
        'SC2': [
            ('20231117172501', '20231118062001'),
            ('20231118171501', '20231119061501')
        ]
    }

    # Downsampling rules (hardcoded as per R logic)
    DOWNSAMPLE_RULES = {
        'SC1': {'original_interval': 5, 'target_interval': 30},
        'SC2': {'original_interval': 5, 'target_interval': 30},
        'SC7': {'original_interval': 10, 'target_interval': 30},
        'SC12': {'original_interval': 10, 'target_interval': 30},
        'SC9': {'original_interval': 10, 'target_interval': 30},
        'SLC6_2': {'original_interval': 10, 'target_interval': 30}
    }

    def _map_count_to_number(self, count_text: str) -> float:
        """Map text count labels to numeric values"""
        if count_text is None:
            return 0.0

        count_str = str(count_text).strip()

        # Direct mapping
        if count_str in self.COUNT_MAPPING:
            return float(self.COUNT_MAPPING[count_str])

        # Range patterns
        range_match = re.match(r'^(\d+)-(\d+)$', count_str)
        if range_match:
            return float(range_match.group(1))  # Use minimum value

        # Plus patterns
        plus_match = re.match(r'^(\d+)\+$', count_str)
        if plus_match:
            return float(plus_match.group(1))

        # Try direct numeric conversion
        try:
            return float(count_str)
        except ValueError:
            raise ValueError(f"Cannot parse count value: '{count_text}'")

    def _extract_timestamp_from_filename(self, filename: str) -> Optional[datetime]:
        """Extract timestamp from image filename"""
        match = re.search(r'_(\d{14})', filename)
        if not match:
            return None

        timestamp_str = match.group(1)
        try:
            return datetime.strptime(timestamp_str, '%Y%m%d%H%M%S')
        except ValueError:
            return None

    def _is_night_image(self, deployment_id: str, timestamp: datetime) -> bool:
        """Check if image is during night period"""
        if deployment_id not in self.NIGHT_PERIODS:
            return False

        timestamp_str = timestamp.strftime('%Y%m%d%H%M%S')

        for start_str, end_str in self.NIGHT_PERIODS[deployment_id]:
            if start_str <= timestamp_str <= end_str:
                return True
        return False

    def _should_downsample(self, deployment_id: str, timestamp: datetime) -> bool:
        """Check if image should be excluded due to downsampling rules"""
        if deployment_id not in self.DOWNSAMPLE_RULES:
            return False

        rules = self.DOWNSAMPLE_RULES[deployment_id]
        target_interval = rules['target_interval']

        # Keep only images at target interval boundaries
        return timestamp.minute % target_interval != 0

    def _process_cells(self, cells_data: Dict) -> Tuple[float, float]:
        """Process cell data and return total counts and direct sun counts"""
        if not cells_data:
            return 0.0, 0.0

        total_butterflies = 0.0
        butterflies_direct_sun = 0.0

        for cell_data in cells_data.values():
            count_raw = cell_data.get('count', '0')
            direct_sun = cell_data.get('directSun', False)

            count_numeric = self._map_count_to_number(count_raw)
            total_butterflies += count_numeric

            if direct_sun:
                butterflies_direct_sun += count_numeric

        return total_butterflies, butterflies_direct_sun

    def process_deployments(self, json_dir: str = 'data/deployments') -> pd.DataFrame:
        """Process all deployment JSON files and return DataFrame"""
        json_path = Path(json_dir)
        json_files = list(json_path.glob('*.json'))

        if not json_files:
            raise RuntimeError(f"No JSON files found in {json_dir}")

        all_results = []

        for json_file in json_files:
            deployment_id = json_file.stem
            print(f"Processing {deployment_id}...")

            try:
                with open(json_file, 'r') as f:
                    data = json.load(f)
            except (json.JSONDecodeError, IOError) as e:
                raise RuntimeError(f"Failed to load JSON file {json_file}: {e}")

            # Handle both JSON structures
            if 'classifications' in data:
                classifications = data['classifications']
            else:
                classifications = data

            for image_filename, image_data in classifications.items():
                # Skip night images
                if image_data.get('isNight', False):
                    continue

                # Extract timestamp
                timestamp = self._extract_timestamp_from_filename(image_filename)
                if not timestamp:
                    continue

                # Apply night filtering
                if self._is_night_image(deployment_id, timestamp):
                    continue

                # Apply downsampling rules
                if self._should_downsample(deployment_id, timestamp):
                    continue

                # Process cell data
                cells_data = image_data.get('cells', {})
                total_butterflies, butterflies_direct_sun = self._process_cells(cells_data)

                all_results.append({
                    'deployment_id': deployment_id,
                    'image_filename': image_filename,
                    'timestamp': timestamp,
                    'date': timestamp.date(),
                    'total_butterflies': total_butterflies,
                    'butterflies_direct_sun': butterflies_direct_sun
                })

        if not all_results:
            print("Warning: No valid butterfly count data found")
            return pd.DataFrame()

        df = pd.DataFrame(all_results)
        df = df.sort_values(['deployment_id', 'timestamp']).reset_index(drop=True)

        print(f"Processed {len(df)} butterfly observations from {len(json_files)} deployments")
        return df


def add_temperature_data(butterfly_df: pd.DataFrame,
                         temp_file: str = 'data/temperature_data_2023.csv') -> pd.DataFrame:
    """Add temperature data to butterfly observations"""
    if butterfly_df.empty:
        raise ValueError("Cannot add temperature data to empty butterfly DataFrame")

    try:
        temp_df = pd.read_csv(temp_file, usecols=['filename', 'temperature'])
        print(f"Loaded {len(temp_df)} temperature records")
    except FileNotFoundError:
        raise FileNotFoundError(f"Temperature file not found: {temp_file}")

    # Perform left join
    merged_df = butterfly_df.merge(
        temp_df,
        left_on='image_filename',
        right_on='filename',
        how='left'
    ).drop('filename', axis=1)

    missing_temp = merged_df['temperature'].isna().sum()
    if missing_temp > 0:
        print(f"⚠️  Warning: {missing_temp} observations missing temperature data")

    print(f"✅ Temperature data joined successfully")
    return merged_df


def create_daily_aggregates(df: pd.DataFrame) -> pd.DataFrame:
    """Aggregate butterfly and temperature data to daily level"""
    if df.empty:
        raise ValueError("Cannot aggregate empty DataFrame")

    print("\n=== CREATING DAILY AGGREGATES ===")

    # Group by deployment and date
    grouped = df.groupby(['deployment_id', 'date'])

    daily_stats = []
    for (deployment_id, date), group in grouped:
        photo_count = len(group)

        # Calculate butterfly metrics
        total_butterflies = group['total_butterflies'].values
        butterflies_direct_sun = group['butterflies_direct_sun'].values

        # Maximum values
        max_butterflies = total_butterflies.max()
        # Sum all butterflies in direct sun throughout the day
        sum_butterflies_direct_sun = butterflies_direct_sun.sum()

        # 95th percentile
        butterflies_95th = np.percentile(total_butterflies, 95)

        # Top 3 mean
        top_3_values = np.sort(total_butterflies)[-3:]
        # Pad with zeros if fewer than 3 observations
        while len(top_3_values) < 3:
            top_3_values = np.append(top_3_values, 0)
        butterflies_top3_mean = top_3_values.mean()

        # Time of maximum count
        max_idx = total_butterflies.argmax()
        time_of_max = group.iloc[max_idx]['timestamp']
        temp_at_max = group.iloc[max_idx]['temperature']

        # Temperature metrics (daytime only - already filtered)
        temperatures = group['temperature'].dropna()
        if len(temperatures) > 0:
            temp_max = temperatures.max()
            temp_min = temperatures.min()
            temp_mean = temperatures.mean()

            # Hours above 15°C (assuming 30-min intervals, each observation = 0.5 hours)
            hours_above_15C = (temperatures >= 15).sum() * 0.5

            # Degree-hours above 15°C
            degree_hours_above_15C = ((temperatures[temperatures > 15] - 15) * 0.5).sum()
        else:
            temp_max = temp_min = temp_mean = temp_at_max = np.nan
            hours_above_15C = degree_hours_above_15C = 0

        daily_stats.append({
            'deployment_id': deployment_id,
            'date': date,
            'photo_count': photo_count,
            'max_butterflies': max_butterflies,
            'butterflies_95th_percentile': butterflies_95th,
            'butterflies_top3_mean': butterflies_top3_mean,
            'sum_butterflies_direct_sun': sum_butterflies_direct_sun,
            'time_of_max': time_of_max,
            'temp_max': temp_max,
            'temp_min': temp_min,
            'temp_mean': temp_mean,
            'temp_at_max_count': temp_at_max,
            'hours_above_15C': hours_above_15C,
            'degree_hours_above_15C': degree_hours_above_15C
        })

    daily_df = pd.DataFrame(daily_stats)

    # Calculate days since October 15th
    season_start = datetime(2023, 10, 15).date()
    daily_df['days_since_oct15'] = daily_df['date'].apply(lambda x: (x - season_start).days)

    print(f"Created {len(daily_df)} daily aggregates")

    # Summary statistics
    photo_counts = daily_df['photo_count'].value_counts().sort_index()
    print(f"\nPhoto counts per day distribution:")
    print(photo_counts.head(10))

    return daily_df


def add_daily_wind_metrics(daily_df: pd.DataFrame,
                          deployments_df: pd.DataFrame,
                          wind_db_dir: str = 'data/wind') -> pd.DataFrame:
    """Add aggregated daily wind metrics"""
    if daily_df.empty:
        raise ValueError("Cannot add wind data to empty DataFrame")

    print("\n=== ADDING DAILY WIND METRICS ===")

    # Create mapping from deployment_id to wind_meter_name
    wind_meter_map = deployments_df.set_index('deployment_id')['wind_meter_name'].to_dict()

    wind_db_path = Path(wind_db_dir)
    if not wind_db_path.exists():
        raise FileNotFoundError(f"Wind database directory not found: {wind_db_dir}")

    # Get available wind database files
    db_files = {
        db_path.stem: db_path
        for db_path in wind_db_path.glob('*.s3db')
    }

    print(f"Found {len(db_files)} wind databases")

    # Add wind metrics for each day
    wind_metrics_list = []

    for _, row in daily_df.iterrows():
        deployment_id = row['deployment_id']
        date = row['date']

        # Get wind meter name
        wind_meter_name = wind_meter_map.get(deployment_id)
        if not wind_meter_name:
            print(f"⚠️  No wind meter found for deployment {deployment_id}")
            wind_metrics = _get_empty_wind_metrics()
        else:
            db_path = db_files.get(wind_meter_name)
            if not db_path:
                print(f"⚠️  No wind database found for {wind_meter_name}")
                wind_metrics = _get_empty_wind_metrics()
            else:
                # Define day period (6am to 6pm as approximation)
                start_time = datetime.combine(date, datetime.min.time().replace(hour=6))
                end_time = datetime.combine(date, datetime.min.time().replace(hour=18))

                wind_metrics = _query_daily_wind_metrics(db_path, start_time, end_time)

        wind_metrics_list.append(wind_metrics)

    # Add wind metrics to dataframe
    wind_df = pd.DataFrame(wind_metrics_list)
    result_df = pd.concat([daily_df, wind_df], axis=1)

    print(f"✅ Wind metrics added successfully")

    # Summary of wind data coverage
    missing_wind = (result_df['wind_obs_count'] == 0).sum()
    if missing_wind > 0:
        print(f"⚠️  Days with missing wind data: {missing_wind}/{len(result_df)}")

    return result_df


def _get_empty_wind_metrics() -> Dict:
    """Return empty wind metrics dictionary"""
    return {
        'wind_avg_sustained': np.nan,
        'wind_max_gust': np.nan,
        'wind_gust_sum': np.nan,
        'wind_gust_sum_above_2ms': np.nan,
        'wind_gust_hours': np.nan,
        'wind_minutes_above_2ms': 0,
        'wind_gust_sd': np.nan,
        'wind_mode_gust': np.nan,
        'wind_obs_count': 0
    }


def _query_daily_wind_metrics(db_path: Path, start_time: datetime, end_time: datetime) -> Dict:
    """Query and calculate daily wind metrics"""
    try:
        with sqlite3.connect(str(db_path)) as conn:
            # Convert to string format
            start_str = start_time.strftime('%Y-%m-%d %H:%M:%S')
            end_str = end_time.strftime('%Y-%m-%d %H:%M:%S')

            query = """
            SELECT time, speed, gust
            FROM Wind
            WHERE time BETWEEN ? AND ?
            ORDER BY time
            """

            wind_data = pd.read_sql_query(query, conn, params=[start_str, end_str])

            if wind_data.empty:
                return _get_empty_wind_metrics()

            # Convert to numeric
            wind_data['speed'] = pd.to_numeric(wind_data['speed'].astype(str).str.strip(), errors='coerce')
            wind_data['gust'] = pd.to_numeric(wind_data['gust'].astype(str).str.strip(), errors='coerce')

            # Calculate metrics
            sustained_speeds = wind_data['speed'].dropna()
            gust_speeds = wind_data['gust'].dropna()

            if len(gust_speeds) == 0:
                return _get_empty_wind_metrics()

            # Basic statistics
            avg_sustained = sustained_speeds.mean() if len(sustained_speeds) > 0 else np.nan
            max_gust = gust_speeds.max()
            gust_sd = gust_speeds.std() if len(gust_speeds) > 1 else np.nan

            # Sum metrics
            gust_sum = gust_speeds.sum()
            gust_sum_above_2ms = gust_speeds[gust_speeds > 2].sum()

            # Integral approximation (trapezoid rule for gust-hours)
            # Assuming 1-minute intervals
            gust_hours = gust_speeds.sum() / 60  # Convert minutes to hours

            # Threshold metrics
            minutes_above_2ms = (gust_speeds >= 2).sum()

            # Mode (most frequent gust speed, rounded to 0.5 m/s bins)
            if len(gust_speeds) > 0:
                gust_binned = np.round(gust_speeds * 2) / 2  # Round to nearest 0.5
                mode_gust = gust_binned.mode().iloc[0] if len(gust_binned.mode()) > 0 else gust_binned.iloc[0]
            else:
                mode_gust = np.nan

            return {
                'wind_avg_sustained': avg_sustained,
                'wind_max_gust': max_gust,
                'wind_gust_sum': gust_sum,
                'wind_gust_sum_above_2ms': gust_sum_above_2ms,
                'wind_gust_hours': gust_hours,
                'wind_minutes_above_2ms': minutes_above_2ms,
                'wind_gust_sd': gust_sd,
                'wind_mode_gust': mode_gust,
                'wind_obs_count': len(wind_data)
            }

    except Exception as e:
        print(f"Error querying wind data: {e}")
        return _get_empty_wind_metrics()


def filter_valid_days(daily_df: pd.DataFrame,
                     min_photos: int = 15,
                     max_photos: int = 25) -> pd.DataFrame:
    """Filter days based on photo count criteria"""
    print(f"\n=== FILTERING VALID DAYS ===")
    print(f"Criteria: {min_photos} <= photo_count <= {max_photos}")

    # Apply photo count filter
    valid_mask = (daily_df['photo_count'] >= min_photos) & (daily_df['photo_count'] <= max_photos)
    valid_df = daily_df[valid_mask].copy()

    excluded_df = daily_df[~valid_mask]

    print(f"Days before filtering: {len(daily_df)}")
    print(f"Days after filtering: {len(valid_df)}")
    print(f"Days excluded: {len(excluded_df)}")

    if len(excluded_df) > 0:
        print(f"\nExcluded days by photo count:")
        excluded_counts = excluded_df['photo_count'].value_counts().sort_index()
        for count, n_days in excluded_counts.items():
            print(f"  {count} photos: {n_days} days")

    return valid_df


def create_lag_pairs(daily_df: pd.DataFrame,
                    deployments_df: pd.DataFrame) -> pd.DataFrame:
    """Create lag pairs comparing consecutive days within deployments"""
    print(f"\n=== CREATING LAG PAIRS ===")

    lag_pairs = []

    # Process each deployment separately
    for deployment_id in daily_df['deployment_id'].unique():
        deployment_days = daily_df[daily_df['deployment_id'] == deployment_id].sort_values('date')

        # Skip if deployment has fewer than 2 days
        if len(deployment_days) < 2:
            print(f"Skipping {deployment_id}: only {len(deployment_days)} day(s)")
            continue

        # Skip the first day (no previous day)
        for i in range(1, len(deployment_days)):
            current_day = deployment_days.iloc[i]
            previous_day = deployment_days.iloc[i-1]

            # Check if days are consecutive
            date_diff = (current_day['date'] - previous_day['date']).days
            if date_diff != 1:
                print(f"Warning: Non-consecutive days in {deployment_id}: {previous_day['date']} to {current_day['date']}")
                continue

            # Skip if both days have zero butterflies
            if current_day['max_butterflies'] == 0 and previous_day['max_butterflies'] == 0:
                continue

            # Create lag pair record
            lag_record = {
                'deployment_id': deployment_id,
                'deployment_day_id_t': f"{deployment_id}_{current_day['date'].strftime('%Y%m%d')}",
                'deployment_day_id_t_1': f"{deployment_id}_{previous_day['date'].strftime('%Y%m%d')}",
                'date_t': current_day['date'],
                'date_t_1': previous_day['date'],
                'observation_order_t': i + 1,

                # Current day butterfly metrics
                'max_butterflies_t': current_day['max_butterflies'],
                'butterflies_95th_percentile_t': current_day['butterflies_95th_percentile'],
                'butterflies_top3_mean_t': current_day['butterflies_top3_mean'],
                'sum_butterflies_direct_sun_t': current_day['sum_butterflies_direct_sun'],
                'time_of_max_t': current_day['time_of_max'],

                # Previous day butterfly metrics
                'max_butterflies_t_1': previous_day['max_butterflies'],
                'butterflies_95th_percentile_t_1': previous_day['butterflies_95th_percentile'],
                'butterflies_top3_mean_t_1': previous_day['butterflies_top3_mean'],
                'sum_butterflies_direct_sun_t_1': previous_day['sum_butterflies_direct_sun'],
                'time_of_max_t_1': previous_day['time_of_max'],

                # Previous day weather predictors
                'temp_max_t_1': previous_day['temp_max'],
                'temp_min_t_1': previous_day['temp_min'],
                'temp_mean_t_1': previous_day['temp_mean'],
                'temp_at_max_count_t_1': previous_day['temp_at_max_count'],
                'hours_above_15C_t_1': previous_day['hours_above_15C'],
                'degree_hours_above_15C_t_1': previous_day['degree_hours_above_15C'],

                # Previous day wind metrics
                'wind_avg_sustained_t_1': previous_day['wind_avg_sustained'],
                'wind_max_gust_t_1': previous_day['wind_max_gust'],
                'wind_gust_sum_t_1': previous_day['wind_gust_sum'],
                'wind_gust_sum_above_2ms_t_1': previous_day['wind_gust_sum_above_2ms'],
                'wind_gust_hours_t_1': previous_day['wind_gust_hours'],
                'wind_minutes_above_2ms_t_1': previous_day['wind_minutes_above_2ms'],
                'wind_gust_sd_t_1': previous_day['wind_gust_sd'],
                'wind_mode_gust_t_1': previous_day['wind_mode_gust'],

                # Temporal variables
                'days_since_oct15_t': current_day['days_since_oct15']
            }

            # Calculate response variables
            # Max butterflies differences
            lag_record['butterfly_diff'] = current_day['max_butterflies'] - previous_day['max_butterflies']
            lag_record['butterfly_diff_cbrt'] = np.sign(lag_record['butterfly_diff']) * np.power(abs(lag_record['butterfly_diff']), 1/3)
            lag_record['butterfly_diff_log'] = np.sign(lag_record['butterfly_diff']) * np.log(abs(lag_record['butterfly_diff']) + 1)

            # 95th percentile differences
            diff_95th = current_day['butterflies_95th_percentile'] - previous_day['butterflies_95th_percentile']
            lag_record['butterfly_diff_95th'] = diff_95th
            lag_record['butterfly_diff_95th_cbrt'] = np.sign(diff_95th) * np.power(abs(diff_95th), 1/3)
            lag_record['butterfly_diff_95th_log'] = np.sign(diff_95th) * np.log(abs(diff_95th) + 1)

            # Top 3 mean differences
            diff_top3 = current_day['butterflies_top3_mean'] - previous_day['butterflies_top3_mean']
            lag_record['butterfly_diff_top3'] = diff_top3
            lag_record['butterfly_diff_top3_cbrt'] = np.sign(diff_top3) * np.power(abs(diff_top3), 1/3)
            lag_record['butterfly_diff_top3_log'] = np.sign(diff_top3) * np.log(abs(diff_top3) + 1)

            lag_pairs.append(lag_record)

    if not lag_pairs:
        raise ValueError("No valid lag pairs created")

    lag_df = pd.DataFrame(lag_pairs)

    # Add deployment metadata
    metadata_cols = ['deployment_id', 'Observer', 'horizontal_dist_to_cluster_m', 'grove', 'view_id']

    # Check for missing columns
    missing_cols = [col for col in metadata_cols if col not in deployments_df.columns]
    if missing_cols:
        print(f"Warning: Missing metadata columns: {missing_cols}")
        available_cols = [col for col in metadata_cols if col in deployments_df.columns]
        deployment_metadata = deployments_df[available_cols].copy()
    else:
        deployment_metadata = deployments_df[metadata_cols].copy()

    # Join metadata
    lag_df = lag_df.merge(deployment_metadata, on='deployment_id', how='left')

    print(f"Created {len(lag_df)} lag pairs from {lag_df['deployment_id'].nunique()} deployments")

    # Summary statistics
    print(f"\nLag pairs per deployment:")
    pairs_per_deployment = lag_df['deployment_id'].value_counts()
    for dep_id, n_pairs in pairs_per_deployment.head(10).items():
        print(f"  {dep_id}: {n_pairs} pairs")

    return lag_df


def create_diagnostic_plots(lag_df: pd.DataFrame, daily_df: pd.DataFrame, output_dir: str = 'data/daily_lag_plots'):
    """Create diagnostic plots for data quality assessment"""
    print(f"\n=== CREATING DIAGNOSTIC PLOTS ===")

    # Create output directory
    output_path = Path(output_dir)
    output_path.mkdir(parents=True, exist_ok=True)

    # Set style
    if HAS_SEABORN:
        sns.set_style("whitegrid")
    else:
        plt.style.use('seaborn-v0_8-whitegrid' if 'seaborn-v0_8-whitegrid' in plt.style.available else 'ggplot')

    # 1. Photo counts distribution
    fig, axes = plt.subplots(1, 2, figsize=(12, 5))

    axes[0].hist(daily_df['photo_count'], bins=30, edgecolor='black', alpha=0.7)
    axes[0].axvline(x=15, color='red', linestyle='--', label='Min threshold')
    axes[0].axvline(x=25, color='red', linestyle='--', label='Max threshold')
    axes[0].set_xlabel('Photos per Day')
    axes[0].set_ylabel('Number of Days')
    axes[0].set_title('Distribution of Photo Counts per Day')
    axes[0].legend()

    # Photo counts by deployment
    deployment_photo_counts = daily_df.groupby('deployment_id')['photo_count'].mean().sort_values()
    axes[1].barh(range(len(deployment_photo_counts)), deployment_photo_counts.values)
    axes[1].set_yticks(range(len(deployment_photo_counts)))
    axes[1].set_yticklabels(deployment_photo_counts.index, fontsize=8)
    axes[1].set_xlabel('Mean Photos per Day')
    axes[1].set_title('Average Photo Count by Deployment')
    axes[1].axvline(x=20, color='green', linestyle='--', alpha=0.5)

    plt.tight_layout()
    plt.savefig(output_path / 'photo_counts_distribution.png', dpi=150, bbox_inches='tight')
    plt.close()

    # 2. Response variable distributions (3x3 grid)
    fig, axes = plt.subplots(3, 3, figsize=(15, 12))

    response_vars = [
        ('butterfly_diff', 'Max Butterflies Difference'),
        ('butterfly_diff_cbrt', 'Max Butterflies Diff (Cube Root)'),
        ('butterfly_diff_log', 'Max Butterflies Diff (Log)'),
        ('butterfly_diff_95th', '95th Percentile Difference'),
        ('butterfly_diff_95th_cbrt', '95th Percentile Diff (Cube Root)'),
        ('butterfly_diff_95th_log', '95th Percentile Diff (Log)'),
        ('butterfly_diff_top3', 'Top 3 Mean Difference'),
        ('butterfly_diff_top3_cbrt', 'Top 3 Mean Diff (Cube Root)'),
        ('butterfly_diff_top3_log', 'Top 3 Mean Diff (Log)')
    ]

    for idx, (var_name, var_label) in enumerate(response_vars):
        row = idx // 3
        col = idx % 3

        if var_name in lag_df.columns:
            data = lag_df[var_name].dropna()
            axes[row, col].hist(data, bins=30, edgecolor='black', alpha=0.7)
            axes[row, col].set_xlabel(var_label)
            axes[row, col].set_ylabel('Frequency')
            axes[row, col].axvline(x=0, color='red', linestyle='--', alpha=0.5)

            # Add statistics
            mean_val = data.mean()
            median_val = data.median()
            axes[row, col].text(0.05, 0.95, f'Mean: {mean_val:.2f}\nMedian: {median_val:.2f}',
                               transform=axes[row, col].transAxes,
                               verticalalignment='top',
                               bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))

    plt.suptitle('Distribution of Response Variables (All Transformations)', fontsize=14, y=1.02)
    plt.tight_layout()
    plt.savefig(output_path / 'response_variables_distributions.png', dpi=150, bbox_inches='tight')
    plt.close()

    # 3. Time series of daily max butterflies by deployment
    fig, ax = plt.subplots(figsize=(15, 8))

    for deployment_id in daily_df['deployment_id'].unique()[:10]:  # Show first 10 deployments
        dep_data = daily_df[daily_df['deployment_id'] == deployment_id].sort_values('date')
        ax.plot(dep_data['date'], dep_data['max_butterflies'],
               marker='o', label=deployment_id, alpha=0.7)

    ax.set_xlabel('Date')
    ax.set_ylabel('Maximum Butterfly Count')
    ax.set_title('Daily Maximum Butterfly Counts by Deployment')
    ax.legend(bbox_to_anchor=(1.05, 1), loc='upper left')
    ax.grid(True, alpha=0.3)

    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.savefig(output_path / 'time_series_by_deployment.png', dpi=150, bbox_inches='tight')
    plt.close()

    # 4. Correlation matrix of predictors
    predictor_cols = [col for col in lag_df.columns if col.endswith('_t_1') and
                     not col.startswith('butterfly') and not col.startswith('date')]

    if len(predictor_cols) > 0:
        fig, ax = plt.subplots(figsize=(12, 10))

        # Select numeric predictor columns
        predictor_data = lag_df[predictor_cols].select_dtypes(include=[np.number])

        if not predictor_data.empty:
            corr_matrix = predictor_data.corr()

            # Create heatmap
            im = ax.imshow(corr_matrix, cmap='coolwarm', vmin=-1, vmax=1, aspect='auto')

            # Set ticks and labels
            ax.set_xticks(np.arange(len(corr_matrix.columns)))
            ax.set_yticks(np.arange(len(corr_matrix.columns)))
            ax.set_xticklabels([col.replace('_t_1', '') for col in corr_matrix.columns], rotation=45, ha='right')
            ax.set_yticklabels([col.replace('_t_1', '') for col in corr_matrix.columns])

            # Add colorbar
            plt.colorbar(im, ax=ax)

            # Add correlation values
            for i in range(len(corr_matrix.columns)):
                for j in range(len(corr_matrix.columns)):
                    ax.text(j, i, f'{corr_matrix.iloc[i, j]:.2f}',
                            ha='center', va='center', color='black', fontsize=8)

            ax.set_title('Correlation Matrix of Previous Day Predictors')
            plt.tight_layout()
            plt.savefig(output_path / 'predictor_correlation_matrix.png', dpi=150, bbox_inches='tight')
            plt.close()

    # 5. Wind vs Butterfly Difference scatter plot
    _, axes = plt.subplots(1, 3, figsize=(18, 5))

    # Plot for each butterfly metric
    metrics = [
        ('butterfly_diff', 'Max Butterflies Difference'),
        ('butterfly_diff_95th', '95th Percentile Difference'),
        ('butterfly_diff_top3', 'Top 3 Mean Difference')
    ]

    for idx, (metric, title) in enumerate(metrics):
        ax = axes[idx]

        # Remove NaN values for plotting
        plot_data = lag_df[['wind_max_gust_t_1', metric]].dropna()

        if not plot_data.empty:
            # Scatter plot
            ax.scatter(plot_data['wind_max_gust_t_1'], plot_data[metric],
                      alpha=0.5, color='darkblue', s=30)

            # Add smooth line using polynomial fit
            if len(plot_data) > 3:
                # Sort for smooth line
                x_sorted = np.sort(plot_data['wind_max_gust_t_1'].values)

                # LOESS-like smoothing using polynomial
                from scipy.interpolate import UnivariateSpline
                try:
                    # Create spline for smoothing
                    spline = UnivariateSpline(plot_data['wind_max_gust_t_1'],
                                             plot_data[metric],
                                             s=len(plot_data)*5, k=3)
                    y_smooth = spline(x_sorted)
                    ax.plot(x_sorted, y_smooth, 'r-', linewidth=2, label='Smooth fit')
                except:
                    # Fallback to polynomial if spline fails
                    z = np.polyfit(plot_data['wind_max_gust_t_1'], plot_data[metric], 3)
                    p = np.poly1d(z)
                    y_smooth = p(x_sorted)
                    ax.plot(x_sorted, y_smooth, 'r-', linewidth=2, label='Polynomial fit')

            # Add reference lines
            ax.axhline(y=0, color='gray', linestyle='--', alpha=0.5)
            ax.axvline(x=2, color='orange', linestyle='--', alpha=0.5, label='2 m/s threshold')

            # Labels and title
            ax.set_xlabel('Previous Day Max Wind Gust (m/s)')
            ax.set_ylabel(title)
            ax.set_title(f'Wind Effect on {title}')
            ax.legend(loc='best', fontsize=8)
            ax.grid(True, alpha=0.3)

            # Add correlation coefficient
            corr = plot_data['wind_max_gust_t_1'].corr(plot_data[metric])
            ax.text(0.05, 0.95, f'r = {corr:.3f}',
                   transform=ax.transAxes,
                   bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5),
                   verticalalignment='top')

    plt.suptitle('Wind Speed vs Butterfly Change Analysis', fontsize=14, y=1.02)
    plt.tight_layout()
    plt.savefig(output_path / 'wind_vs_butterfly_change.png', dpi=150, bbox_inches='tight')
    plt.close()

    print(f"✅ Diagnostic plots saved to {output_path}")


def generate_summary_report(lag_df: pd.DataFrame, daily_df: pd.DataFrame,
                           output_file: str = 'data/daily_lag_analysis_summary.txt'):
    """Generate summary report of the analysis"""

    with open(output_file, 'w') as f:
        f.write("="*60 + "\n")
        f.write("DAILY LAG ANALYSIS SUMMARY REPORT\n")
        f.write(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
        f.write("="*60 + "\n\n")

        # Dataset overview
        f.write("DATASET OVERVIEW\n")
        f.write("-"*40 + "\n")
        f.write(f"Total deployments processed: {daily_df['deployment_id'].nunique()}\n")
        f.write(f"Total days analyzed: {len(daily_df)}\n")
        f.write(f"Total lag pairs created: {len(lag_df)}\n")
        f.write(f"Date range: {daily_df['date'].min()} to {daily_df['date'].max()}\n\n")

        # Photo count statistics
        f.write("PHOTO COUNT STATISTICS\n")
        f.write("-"*40 + "\n")
        photo_stats = daily_df['photo_count'].describe()
        f.write(f"Mean photos per day: {photo_stats['mean']:.1f}\n")
        f.write(f"Median photos per day: {photo_stats['50%']:.1f}\n")
        f.write(f"Range: {photo_stats['min']:.0f} - {photo_stats['max']:.0f}\n\n")

        # Butterfly count statistics
        f.write("BUTTERFLY COUNT STATISTICS (Daily Maximum)\n")
        f.write("-"*40 + "\n")
        butterfly_stats = daily_df['max_butterflies'].describe()
        f.write(f"Mean daily max: {butterfly_stats['mean']:.1f}\n")
        f.write(f"Median daily max: {butterfly_stats['50%']:.1f}\n")
        f.write(f"Range: {butterfly_stats['min']:.0f} - {butterfly_stats['max']:.0f}\n")
        f.write(f"Days with zero butterflies: {(daily_df['max_butterflies'] == 0).sum()}\n\n")

        # Response variable statistics
        f.write("RESPONSE VARIABLE STATISTICS\n")
        f.write("-"*40 + "\n")
        for var in ['butterfly_diff', 'butterfly_diff_95th', 'butterfly_diff_top3']:
            if var in lag_df.columns:
                var_stats = lag_df[var].describe()
                f.write(f"\n{var}:\n")
                f.write(f"  Mean: {var_stats['mean']:.2f}\n")
                f.write(f"  Std: {var_stats['std']:.2f}\n")
                f.write(f"  Range: {var_stats['min']:.2f} to {var_stats['max']:.2f}\n")

        # Deployments summary
        f.write("\n\nDEPLOYMENTS SUMMARY\n")
        f.write("-"*40 + "\n")
        pairs_per_deployment = lag_df['deployment_id'].value_counts()
        f.write(f"Deployments with lag pairs: {len(pairs_per_deployment)}\n")
        f.write(f"Mean pairs per deployment: {pairs_per_deployment.mean():.1f}\n")
        f.write(f"Max pairs in a deployment: {pairs_per_deployment.max()}\n\n")

        f.write("Pairs per deployment:\n")
        for dep_id, n_pairs in pairs_per_deployment.items():
            f.write(f"  {dep_id}: {n_pairs} pairs\n")

        # Missing data summary
        f.write("\n\nMISSING DATA SUMMARY\n")
        f.write("-"*40 + "\n")
        missing_counts = lag_df.isnull().sum()
        missing_counts = missing_counts[missing_counts > 0].sort_values(ascending=False)

        if len(missing_counts) > 0:
            for col, count in missing_counts.items():
                pct = count / len(lag_df) * 100
                f.write(f"{col}: {count} missing ({pct:.1f}%)\n")
        else:
            f.write("No missing data in final dataset\n")

        f.write("\n" + "="*60 + "\n")
        f.write("END OF REPORT\n")

    print(f"✅ Summary report saved to {output_file}")


def main():
    """Main function for daily lag analysis"""
    parser = argparse.ArgumentParser(
        description="Daily lag analysis data preprocessing for monarch butterflies",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    # Data paths
    parser.add_argument('--json-dir', default='data/deployments',
                       help='Directory containing JSON deployment files')
    parser.add_argument('--temp-file', default='data/temperature_data_2023.csv',
                       help='Temperature data file')
    parser.add_argument('--wind-db-dir', default='data/wind',
                       help='Wind database directory')
    parser.add_argument('--deployments-file', default='data/deployments.csv',
                       help='Deployments metadata file')

    # Filtering parameters
    parser.add_argument('--min-photos', type=int, default=15,
                       help='Minimum photos per day for valid day')
    parser.add_argument('--max-photos', type=int, default=25,
                       help='Maximum photos per day for valid day')

    # Output options
    parser.add_argument('--output-file', default='data/monarch_daily_lag_analysis.csv',
                       help='Output CSV filename')
    parser.add_argument('--daily-file', default='data/monarch_daily_summaries.csv',
                       help='Daily summaries output file')
    parser.add_argument('--plot-dir', default='data/daily_lag_plots',
                       help='Directory for diagnostic plots')

    args = parser.parse_args()

    print("="*60)
    print("MONARCH BUTTERFLY DAILY LAG ANALYSIS")
    print(f"Started: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print("="*60)

    try:
        # Load deployment metadata
        print(f"\nLoading deployment metadata from {args.deployments_file}...")
        deployments_df = pd.read_csv(args.deployments_file)
        print(f"Loaded {len(deployments_df)} deployments")

        # Process butterfly counts
        print(f"\nProcessing butterfly counts from {args.json_dir}...")
        processor = DailyButterflyProcessor()
        butterfly_df = processor.process_deployments(args.json_dir)

        if butterfly_df.empty:
            raise ValueError("No butterfly data processed")

        # Add temperature data
        print(f"\nAdding temperature data from {args.temp_file}...")
        butterfly_with_temp = add_temperature_data(butterfly_df, args.temp_file)

        # Create daily aggregates
        daily_df = create_daily_aggregates(butterfly_with_temp)

        # Add wind metrics
        daily_with_wind = add_daily_wind_metrics(daily_df, deployments_df, args.wind_db_dir)

        # Filter valid days
        valid_days = filter_valid_days(daily_with_wind, args.min_photos, args.max_photos)

        # Save daily summaries
        valid_days.to_csv(args.daily_file, index=False)
        print(f"\n✅ Daily summaries saved to {args.daily_file}")

        # Create lag pairs
        lag_df = create_lag_pairs(valid_days, deployments_df)

        # Save final dataset
        lag_df.to_csv(args.output_file, index=False)
        print(f"\n✅ Final lag analysis dataset saved to {args.output_file}")

        # Create diagnostic plots
        create_diagnostic_plots(lag_df, valid_days, args.plot_dir)

        # Generate summary report
        generate_summary_report(lag_df, valid_days)

        print("\n" + "="*60)
        print("ANALYSIS COMPLETED SUCCESSFULLY")
        print(f"Final dataset: {len(lag_df)} lag pairs")
        print(f"Output files:")
        print(f"  - {args.output_file}")
        print(f"  - {args.daily_file}")
        print(f"  - {args.plot_dir}/")
        print(f"Completed: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("="*60)

    except Exception as e:
        print(f"\n❌ ERROR: {e}")
        import traceback
        traceback.print_exc()
        return 1

    return 0


if __name__ == "__main__":
    exit(main())