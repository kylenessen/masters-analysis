#!/usr/bin/env python3
"""
Dynamic window analysis data preprocessing for monarch butterfly study
Creates day-to-day comparisons with weather metrics calculated over dynamic time windows:
1. 24-hour window: from time of max count (t-1) to +24 hours
2. Sunset window: from time of max count (t-1) to last observation on day t

Key difference from original: includes overnight temperature and wind data
"""

import pandas as pd
import numpy as np
import json
import re
import sqlite3
import argparse
import matplotlib.pyplot as plt
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, Tuple, Optional
import warnings
warnings.filterwarnings('ignore')


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
                # Check if labeled as night
                is_night = image_data.get('isNight', False)

                # Extract timestamp
                timestamp = self._extract_timestamp_from_filename(image_filename)
                if not timestamp:
                    continue

                # Apply night filtering
                if is_night or self._is_night_image(deployment_id, timestamp):
                    is_night = True

                # Apply downsampling rules (only to non-night images)
                if not is_night and self._should_downsample(deployment_id, timestamp):
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
                    'butterflies_direct_sun': butterflies_direct_sun,
                    'isNight': is_night
                })

        if not all_results:
            print("Warning: No valid butterfly count data found")
            return pd.DataFrame()

        df = pd.DataFrame(all_results)
        df = df.sort_values(['deployment_id', 'timestamp']).reset_index(drop=True)

        print(f"Processed {len(df)} butterfly observations from {len(json_files)} deployments")
        print(f"  - Daytime observations: {(~df['isNight']).sum()}")
        print(f"  - Night observations: {df['isNight'].sum()}")
        return df


def add_temperature_data(butterfly_df: pd.DataFrame,
                         temp_file: str = 'data/temperature_data_2023.csv') -> pd.DataFrame:
    """Add temperature data to butterfly observations and extract timestamps"""
    if butterfly_df.empty:
        raise ValueError("Cannot add temperature data to empty butterfly DataFrame")

    try:
        temp_df = pd.read_csv(temp_file, usecols=['filename', 'temperature'])
        print(f"Loaded {len(temp_df)} temperature records")
    except FileNotFoundError:
        raise FileNotFoundError(f"Temperature file not found: {temp_file}")

    # Extract deployment_id and timestamp from temperature filenames
    def extract_temp_info(filename):
        # Expected format: deploymentID_YYYYMMDDHHMMSS.jpg
        match = re.match(r'([^_]+)_(\d{14})', filename)
        if match:
            return match.group(1), match.group(2)
        return None, None

    temp_df['deployment_id'] = temp_df['filename'].apply(lambda x: extract_temp_info(x)[0])
    temp_df['timestamp_str'] = temp_df['filename'].apply(lambda x: extract_temp_info(x)[1])

    # Convert timestamp string to datetime
    temp_df['timestamp'] = pd.to_datetime(temp_df['timestamp_str'], format='%Y%m%d%H%M%S', errors='coerce')

    # Drop rows with invalid timestamps
    temp_df = temp_df.dropna(subset=['timestamp', 'deployment_id'])

    # Perform left join on butterfly data
    merged_df = butterfly_df.merge(
        temp_df[['filename', 'temperature']],
        left_on='image_filename',
        right_on='filename',
        how='left'
    ).drop('filename', axis=1)

    missing_temp = merged_df['temperature'].isna().sum()
    if missing_temp > 0:
        print(f"⚠️  Warning: {missing_temp} observations missing temperature data")

    print(f"✅ Temperature data joined successfully")

    # Also return the full temperature dataset with timestamps for 24/7 queries
    return merged_df, temp_df[['deployment_id', 'timestamp', 'temperature']]


def create_daily_aggregates_with_sunset(df: pd.DataFrame) -> pd.DataFrame:
    """
    Aggregate butterfly and temperature data to daily level
    Adds last_observation_time (functional sunset) for each day
    """
    if df.empty:
        raise ValueError("Cannot aggregate empty DataFrame")

    print("\n=== CREATING DAILY AGGREGATES ===")

    # Group by deployment and date
    grouped = df.groupby(['deployment_id', 'date'])

    daily_stats = []
    for (deployment_id, date), group in grouped:
        # Filter to daytime observations only
        daytime_obs = group[~group['isNight']]

        if len(daytime_obs) == 0:
            continue  # Skip days with no daytime observations

        photo_count = len(daytime_obs)

        # Calculate butterfly metrics (daytime only)
        total_butterflies = daytime_obs['total_butterflies'].values
        butterflies_direct_sun = daytime_obs['butterflies_direct_sun'].values

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
        time_of_max = daytime_obs.iloc[max_idx]['timestamp']
        temp_at_max = daytime_obs.iloc[max_idx]['temperature']

        # Last observation time (functional sunset)
        last_observation_time = daytime_obs['timestamp'].max()

        # Temperature metrics (daytime only for these stats, but we'll recalculate with 24/7 later)
        temperatures = daytime_obs['temperature'].dropna()
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
            'last_observation_time': last_observation_time,  # NEW: functional sunset
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

    # Add day sequence within each deployment (for AR1 correlation structure)
    daily_df = daily_df.sort_values(['deployment_id', 'date']).reset_index(drop=True)
    daily_df['day_sequence'] = daily_df.groupby('deployment_id').cumcount() + 1

    print(f"Created {len(daily_df)} daily aggregates")

    return daily_df


def calculate_dynamic_temperature_metrics(
    temp_df: pd.DataFrame,
    deployment_id: str,
    start_time: datetime,
    end_time: datetime
) -> Dict:
    """
    Calculate temperature metrics over a dynamic time window (includes 24/7 data)

    Args:
        temp_df: Full 24/7 temperature data with columns [deployment_id, timestamp, temperature]
        deployment_id: Deployment identifier
        start_time: Window start (typically time_of_max_t_1)
        end_time: Window end (either +24hrs or last_observation_time_t)

    Returns:
        Dictionary with temperature metrics and data coverage
    """
    # Filter to this deployment and time window
    window_temps = temp_df[
        (temp_df['deployment_id'] == deployment_id) &
        (temp_df['timestamp'] >= start_time) &
        (temp_df['timestamp'] <= end_time)
    ]['temperature'].dropna()

    if len(window_temps) == 0:
        return {
            'temp_max': np.nan,
            'temp_min': np.nan,
            'temp_mean': np.nan,
            'hours_above_15C': np.nan,
            'degree_hours_above_15C': np.nan,
            'temp_obs_count': 0,
            'temp_data_coverage': 0.0
        }

    # Calculate metrics
    temp_max = window_temps.max()
    temp_min = window_temps.min()
    temp_mean = window_temps.mean()

    # Hours above 15°C (assuming 30-min intervals)
    hours_above_15C = (window_temps >= 15).sum() * 0.5

    # Degree-hours above 15°C
    degree_hours_above_15C = ((window_temps[window_temps > 15] - 15) * 0.5).sum()

    # Data coverage
    window_duration_hours = (end_time - start_time).total_seconds() / 3600
    expected_obs = window_duration_hours * 2  # 2 observations per hour (30-min intervals)
    temp_data_coverage = min(1.0, len(window_temps) / expected_obs) if expected_obs > 0 else 0.0

    return {
        'temp_max': temp_max,
        'temp_min': temp_min,
        'temp_mean': temp_mean,
        'hours_above_15C': hours_above_15C,
        'degree_hours_above_15C': degree_hours_above_15C,
        'temp_obs_count': len(window_temps),
        'temp_data_coverage': temp_data_coverage
    }


def calculate_dynamic_wind_metrics(
    db_path: Path,
    start_time: datetime,
    end_time: datetime
) -> Dict:
    """
    Query and calculate wind metrics over a dynamic time window (includes 24/7 data)

    Args:
        db_path: Path to wind SQLite database
        start_time: Window start
        end_time: Window end

    Returns:
        Dictionary with wind metrics and data coverage
    """
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

            # Data coverage
            window_duration_hours = (end_time - start_time).total_seconds() / 3600
            expected_obs = window_duration_hours * 60  # 1 observation per minute
            wind_data_coverage = min(1.0, len(wind_data) / expected_obs) if expected_obs > 0 else 0.0

            return {
                'wind_avg_sustained': avg_sustained,
                'wind_max_gust': max_gust,
                'wind_gust_sum': gust_sum,
                'wind_gust_sum_above_2ms': gust_sum_above_2ms,
                'wind_gust_hours': gust_hours,
                'wind_minutes_above_2ms': minutes_above_2ms,
                'wind_gust_sd': gust_sd,
                'wind_mode_gust': mode_gust,
                'wind_obs_count': len(wind_data),
                'wind_data_coverage': wind_data_coverage
            }

    except Exception as e:
        print(f"Error querying wind data: {e}")
        return _get_empty_wind_metrics()


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
        'wind_obs_count': 0,
        'wind_data_coverage': 0.0
    }


def calculate_dynamic_sun_exposure(
    butterfly_df: pd.DataFrame,
    deployment_id: str,
    start_time: datetime,
    end_time: datetime
) -> Dict:
    """
    Calculate sun exposure over a dynamic time window (daylight observations only)

    Args:
        butterfly_df: Raw butterfly data with isNight flag
        deployment_id: Deployment identifier
        start_time: Window start
        end_time: Window end

    Returns:
        Dictionary with sun exposure metrics and data coverage
    """
    # Filter to this deployment, time window, and DAYTIME only
    daylight_obs = butterfly_df[
        (butterfly_df['deployment_id'] == deployment_id) &
        (butterfly_df['timestamp'] >= start_time) &
        (butterfly_df['timestamp'] <= end_time) &
        (~butterfly_df['isNight'])  # Only daytime observations
    ]

    if len(daylight_obs) == 0:
        return {
            'sum_butterflies_direct_sun': 0.0,
            'butterfly_obs_count': 0,
            'butterfly_data_coverage': 0.0
        }

    # Sum butterflies in direct sun
    sum_direct_sun = daylight_obs['butterflies_direct_sun'].sum()

    # Data coverage (estimate based on ~2 observations per daylight hour)
    # This is approximate - actual daylight hours vary by date
    window_duration_hours = (end_time - start_time).total_seconds() / 3600
    # Assume roughly 12 hours of daylight per day
    estimated_daylight_hours = min(window_duration_hours, window_duration_hours * (12/24))
    expected_obs = estimated_daylight_hours * 2  # 2 obs/hour
    butterfly_data_coverage = min(1.0, len(daylight_obs) / expected_obs) if expected_obs > 0 else 0.0

    return {
        'sum_butterflies_direct_sun': sum_direct_sun,
        'butterfly_obs_count': len(daylight_obs),
        'butterfly_data_coverage': butterfly_data_coverage
    }


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


def create_dynamic_lag_pairs(
    daily_df: pd.DataFrame,
    butterfly_df: pd.DataFrame,
    temp_24hr_df: pd.DataFrame,
    deployments_df: pd.DataFrame,
    wind_db_dir: str,
    window_type: str = '24hr'
) -> pd.DataFrame:
    """
    Create lag pairs with dynamic weather windows

    Args:
        daily_df: Daily aggregates with time_of_max and last_observation_time
        butterfly_df: Raw 30-min butterfly data
        temp_24hr_df: 24/7 temperature data
        deployments_df: Deployment metadata
        wind_db_dir: Directory containing wind databases
        window_type: '24hr' or 'sunset'

    Returns:
        DataFrame with lag pairs and dynamic weather metrics
    """
    print(f"\n=== CREATING {window_type.upper()} WINDOW LAG PAIRS ===")

    # Create mapping from deployment_id to wind_meter_name
    wind_meter_map = deployments_df.set_index('deployment_id')['wind_meter_name'].to_dict()

    # Get available wind database files
    wind_db_path = Path(wind_db_dir)
    db_files = {
        db_path.stem: db_path
        for db_path in wind_db_path.glob('*.s3db')
    }

    lag_pairs = []

    # Process each deployment separately
    for deployment_id in daily_df['deployment_id'].unique():
        deployment_days = daily_df[daily_df['deployment_id'] == deployment_id].sort_values('date')

        # Skip if deployment has fewer than 2 days
        if len(deployment_days) < 2:
            print(f"Skipping {deployment_id}: only {len(deployment_days)} day(s)")
            continue

        # Get wind database for this deployment
        wind_meter_name = wind_meter_map.get(deployment_id)
        if wind_meter_name and wind_meter_name in db_files:
            wind_db = db_files[wind_meter_name]
        else:
            wind_db = None
            print(f"⚠️  No wind database for {deployment_id}")

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

            # Define dynamic time window
            window_start = previous_day['time_of_max']

            if window_type == '24hr':
                window_end = window_start + timedelta(hours=24)
            elif window_type == 'sunset':
                window_end = current_day['last_observation_time']
            else:
                raise ValueError(f"Unknown window_type: {window_type}")

            # Calculate window duration
            lag_duration_hours = (window_end - window_start).total_seconds() / 3600

            # Calculate dynamic temperature metrics (includes overnight)
            temp_metrics = calculate_dynamic_temperature_metrics(
                temp_24hr_df, deployment_id, window_start, window_end
            )

            # Calculate dynamic wind metrics (includes overnight)
            if wind_db:
                wind_metrics = calculate_dynamic_wind_metrics(wind_db, window_start, window_end)
            else:
                wind_metrics = _get_empty_wind_metrics()

            # Calculate dynamic sun exposure (daylight only)
            sun_metrics = calculate_dynamic_sun_exposure(
                butterfly_df, deployment_id, window_start, window_end
            )

            # Calculate overall metrics completeness
            metrics_complete = np.power(
                temp_metrics['temp_data_coverage'] *
                wind_metrics['wind_data_coverage'] *
                sun_metrics['butterfly_data_coverage'],
                1/3  # Geometric mean
            )

            # Create lag pair record
            lag_record = {
                'deployment_id': deployment_id,
                'deployment_day_id_t': f"{deployment_id}_{current_day['date'].strftime('%Y%m%d')}",
                'deployment_day_id_t_1': f"{deployment_id}_{previous_day['date'].strftime('%Y%m%d')}",
                'date_t': current_day['date'],
                'date_t_1': previous_day['date'],
                'observation_order_t': i + 1,
                'day_sequence': current_day['day_sequence'],

                # Window metadata
                'window_start': window_start,
                'window_end': window_end,
                'lag_duration_hours': lag_duration_hours,
                'metrics_complete': metrics_complete,
                'temp_data_coverage': temp_metrics['temp_data_coverage'],
                'wind_data_coverage': wind_metrics['wind_data_coverage'],
                'butterfly_data_coverage': sun_metrics['butterfly_data_coverage'],

                # Current day butterfly metrics
                'max_butterflies_t': current_day['max_butterflies'],
                'butterflies_95th_percentile_t': current_day['butterflies_95th_percentile'],
                'butterflies_top3_mean_t': current_day['butterflies_top3_mean'],
                'sum_butterflies_direct_sun_t': current_day['sum_butterflies_direct_sun'],
                'time_of_max_t': current_day['time_of_max'],

                # Previous day butterfly metrics (baseline)
                'max_butterflies_t_1': previous_day['max_butterflies'],
                'butterflies_95th_percentile_t_1': previous_day['butterflies_95th_percentile'],
                'butterflies_top3_mean_t_1': previous_day['butterflies_top3_mean'],
                'time_of_max_t_1': previous_day['time_of_max'],

                # Dynamic weather predictors (from window_start to window_end)
                'temp_max': temp_metrics['temp_max'],
                'temp_min': temp_metrics['temp_min'],
                'temp_mean': temp_metrics['temp_mean'],
                'temp_at_max_count_t_1': previous_day['temp_at_max_count'],  # Single point
                'hours_above_15C': temp_metrics['hours_above_15C'],
                'degree_hours_above_15C': temp_metrics['degree_hours_above_15C'],

                # Dynamic wind metrics (from window_start to window_end)
                'wind_avg_sustained': wind_metrics['wind_avg_sustained'],
                'wind_max_gust': wind_metrics['wind_max_gust'],
                'wind_gust_sum': wind_metrics['wind_gust_sum'],
                'wind_gust_sum_above_2ms': wind_metrics['wind_gust_sum_above_2ms'],
                'wind_gust_hours': wind_metrics['wind_gust_hours'],
                'wind_minutes_above_2ms': wind_metrics['wind_minutes_above_2ms'],
                'wind_gust_sd': wind_metrics['wind_gust_sd'],
                'wind_mode_gust': wind_metrics['wind_mode_gust'],

                # Dynamic sun exposure (from window_start to window_end, daylight only)
                # Note: spans entire lag window regardless of calendar day
                'sum_butterflies_direct_sun': sun_metrics['sum_butterflies_direct_sun'],

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
    print(f"Mean lag duration: {lag_df['lag_duration_hours'].mean():.1f} hours")
    print(f"Median metrics completeness: {lag_df['metrics_complete'].median():.2f}")

    # Summary statistics
    print(f"\nLag pairs per deployment:")
    pairs_per_deployment = lag_df['deployment_id'].value_counts()
    for dep_id, n_pairs in pairs_per_deployment.head(10).items():
        print(f"  {dep_id}: {n_pairs} pairs")

    return lag_df


def main():
    """Main function for dynamic window analysis"""
    parser = argparse.ArgumentParser(
        description="Dynamic window analysis for monarch butterflies (includes overnight weather)",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )

    # Data paths
    parser.add_argument('--json-dir', default='data/deployments',
                       help='Directory containing JSON deployment files')
    parser.add_argument('--temp-file', default='data/temperature_data_2023.csv',
                       help='Temperature data file (24/7 coverage)')
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
    parser.add_argument('--output-24hr', default='data/monarch_daily_lag_analysis_24hr_window.csv',
                       help='Output CSV for 24-hour window analysis')
    parser.add_argument('--output-sunset', default='data/monarch_daily_lag_analysis_sunset_window.csv',
                       help='Output CSV for sunset window analysis')

    args = parser.parse_args()

    print("="*60)
    print("MONARCH BUTTERFLY DYNAMIC WINDOW ANALYSIS")
    print("Includes overnight temperature and wind data")
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
        butterfly_with_temp, temp_24hr_df = add_temperature_data(butterfly_df, args.temp_file)

        print(f"24/7 temperature dataset: {len(temp_24hr_df)} observations")

        # Create daily aggregates (with functional sunset)
        daily_df = create_daily_aggregates_with_sunset(butterfly_with_temp)

        # Filter valid days
        valid_days = filter_valid_days(daily_df, args.min_photos, args.max_photos)

        # Create 24-hour window lag pairs
        print("\n" + "="*60)
        lag_df_24hr = create_dynamic_lag_pairs(
            valid_days, butterfly_with_temp, temp_24hr_df, deployments_df,
            args.wind_db_dir, window_type='24hr'
        )

        # Save 24-hour window dataset
        lag_df_24hr.to_csv(args.output_24hr, index=False)
        print(f"\n✅ 24-hour window dataset saved to {args.output_24hr}")

        # Create sunset window lag pairs
        print("\n" + "="*60)
        lag_df_sunset = create_dynamic_lag_pairs(
            valid_days, butterfly_with_temp, temp_24hr_df, deployments_df,
            args.wind_db_dir, window_type='sunset'
        )

        # Save sunset window dataset
        lag_df_sunset.to_csv(args.output_sunset, index=False)
        print(f"\n✅ Sunset window dataset saved to {args.output_sunset}")

        # Summary comparison
        print("\n" + "="*60)
        print("ANALYSIS COMPLETED SUCCESSFULLY")
        print("="*60)
        print(f"\n24-Hour Window Analysis:")
        print(f"  - Lag pairs: {len(lag_df_24hr)}")
        print(f"  - Mean duration: {lag_df_24hr['lag_duration_hours'].mean():.2f} hours")
        print(f"  - Median completeness: {lag_df_24hr['metrics_complete'].median():.3f}")

        print(f"\nSunset Window Analysis:")
        print(f"  - Lag pairs: {len(lag_df_sunset)}")
        print(f"  - Mean duration: {lag_df_sunset['lag_duration_hours'].mean():.2f} hours")
        print(f"  - Duration range: {lag_df_sunset['lag_duration_hours'].min():.1f} - {lag_df_sunset['lag_duration_hours'].max():.1f} hours")
        print(f"  - Median completeness: {lag_df_sunset['metrics_complete'].median():.3f}")

        print(f"\nOutput files:")
        print(f"  - {args.output_24hr}")
        print(f"  - {args.output_sunset}")
        print(f"\nCompleted: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print("="*60)

    except Exception as e:
        print(f"\n❌ ERROR: {e}")
        import traceback
        traceback.print_exc()
        return 1

    return 0


if __name__ == "__main__":
    exit(main())
