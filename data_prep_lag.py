#!/usr/bin/env python3
"""
Data preprocessing script with configurable lag analysis
"""

import pandas as pd
import json
import re
import sqlite3
import numpy as np
import argparse
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Tuple, Optional


class ButterflyCountProcessor:
    """Process butterfly count data from deployment JSON files"""
    
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
        
        # Range patterns (e.g., "1-9" -> 1, "10-99" -> 10)
        range_match = re.match(r'^(\d+)-(\d+)$', count_str)
        if range_match:
            return float(range_match.group(1))  # Use minimum value
        
        # Plus patterns (e.g., "1000+" -> 1000)
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
        # Pattern: filename_YYYYMMDDHHMMSS.JPG
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
    
    def _process_json_file(self, json_path: Path) -> List[Dict]:
        """Process a single JSON deployment file"""
        deployment_id = json_path.stem
        
        try:
            with open(json_path, 'r') as f:
                data = json.load(f)
        except (json.JSONDecodeError, IOError) as e:
            raise RuntimeError(f"Failed to load JSON file {json_path}: {e}")
        
        # Handle both JSON structures
        if 'classifications' in data:
            # Structure 2: Nested format  
            classifications = data['classifications']
        else:
            # Structure 1: Flat format
            classifications = data
            
        results = []
        
        for image_filename, image_data in classifications.items():
            # Skip night images (if marked in JSON)
            if image_data.get('isNight', False):
                continue
                
            # Extract timestamp
            timestamp = self._extract_timestamp_from_filename(image_filename)
            if not timestamp:
                continue  # Skip images without valid timestamps
                
            # Apply night filtering (hardcoded periods)
            if self._is_night_image(deployment_id, timestamp):
                continue
                
            # Apply downsampling rules
            if self._should_downsample(deployment_id, timestamp):
                continue
                
            # Process cell data
            cells_data = image_data.get('cells', {})
            total_butterflies, butterflies_direct_sun = self._process_cells(cells_data)
            
            results.append({
                'deployment_id': deployment_id,
                'image_filename': image_filename, 
                'timestamp': timestamp,
                'total_butterflies': total_butterflies,
                'butterflies_direct_sun': butterflies_direct_sun
            })
            
        return results
    
    def process_deployments(self, json_dir: str = 'data/deployments') -> pd.DataFrame:
        """Process all deployment JSON files and return DataFrame"""
        json_path = Path(json_dir)
        json_files = list(json_path.glob('*.json'))
        
        if not json_files:
            raise RuntimeError(f"No JSON files found in {json_dir}")
            
        all_results = []
        
        for json_file in json_files:
            print(f"Processing {json_file.name}...")
            try:
                file_results = self._process_json_file(json_file)
                all_results.extend(file_results)
            except Exception as e:
                raise RuntimeError(f"Error processing {json_file}: {e}")
                
        if not all_results:
            print("Warning: No valid butterfly count data found")
            return pd.DataFrame()
            
        df = pd.DataFrame(all_results)
        df = df.sort_values(['deployment_id', 'timestamp']).reset_index(drop=True)
        
        print(f"Processed {len(df)} butterfly observations from {len(json_files)} deployments")
        return df
    
    def validate_intervals(self, df: pd.DataFrame) -> pd.DataFrame:
        """Validate time intervals between consecutive photos within each deployment-day"""
        if df.empty:
            print("No data to validate")
            return pd.DataFrame()
        
        results = []
        
        # Group by deployment and date 
        df_sorted = df.sort_values(['deployment_id', 'timestamp'])
        df_sorted['date'] = df_sorted['timestamp'].dt.date
        
        for (deployment_id, date), group in df_sorted.groupby(['deployment_id', 'date']):
            if len(group) < 2:
                continue  # Need at least 2 photos to calculate intervals
                
            group_sorted = group.sort_values('timestamp')
            intervals_minutes = group_sorted['timestamp'].diff().dt.total_seconds() / 60
            intervals_minutes = intervals_minutes.dropna()  # Remove first NaN
            
            if len(intervals_minutes) > 0:
                results.append({
                    'deployment_id': deployment_id,
                    'date': date,
                    'n_intervals': len(intervals_minutes),
                    'min_interval': intervals_minutes.min(),
                    'max_interval': intervals_minutes.max(), 
                    'mean_interval': intervals_minutes.mean(),
                    'std_interval': intervals_minutes.std()
                })
        
        if not results:
            print("No valid intervals found")
            return pd.DataFrame()
            
        interval_df = pd.DataFrame(results)
        
        # Overall summary
        all_intervals = []
        for (deployment_id, date), group in df_sorted.groupby(['deployment_id', 'date']):
            if len(group) >= 2:
                group_sorted = group.sort_values('timestamp') 
                intervals = group_sorted['timestamp'].diff().dt.total_seconds() / 60
                all_intervals.extend(intervals.dropna().tolist())
        
        print(f"\n=== INTERVAL VALIDATION SUMMARY ===")
        print(f"Total deployment-days analyzed: {len(interval_df)}")
        print(f"Total intervals measured: {sum(interval_df['n_intervals'])}")
        print(f"Overall min interval: {min(all_intervals):.1f} minutes")
        print(f"Overall max interval: {max(all_intervals):.1f} minutes") 
        print(f"Overall mean interval: {sum(all_intervals)/len(all_intervals):.1f} minutes")
        
        # Flag problematic deployment-days (outside 27-33 minute range)
        problematic = interval_df[(interval_df['mean_interval'] > 33) | (interval_df['mean_interval'] < 27)]
        if not problematic.empty:
            print(f"\n⚠️ PROBLEMATIC INTERVALS (outside 27-33 min range):")
            print(f"Found {len(problematic)} deployment-days with concerning intervals:")
            for _, row in problematic.iterrows():
                print(f"  {row['deployment_id']} {row['date']}: mean={row['mean_interval']:.1f}min, range={row['min_interval']:.1f}-{row['max_interval']:.1f}min, n={row['n_intervals']}")
        else:
            print(f"\n✅ All deployment-days have intervals within 27-33 minute range")
        
        return interval_df


def add_temperature_data(butterfly_df: pd.DataFrame, temp_file: str = 'data/temperature_data_2023.csv') -> pd.DataFrame:
    """Add temperature data to butterfly observations by joining on image filename"""
    if butterfly_df.empty:
        raise ValueError("Cannot add temperature data to empty butterfly DataFrame")
    
    try:
        # Load only needed columns to save memory
        temp_df = pd.read_csv(temp_file, usecols=['filename', 'temperature'])
        print(f"Loaded {len(temp_df)} temperature records")
    except FileNotFoundError:
        raise FileNotFoundError(f"Temperature file not found: {temp_file}")
    except Exception as e:
        raise RuntimeError(f"Failed to load temperature data: {e}")
    
    # Check for duplicates in temperature data
    duplicates = temp_df['filename'].duplicated().sum()
    if duplicates > 0:
        raise ValueError(f"Found {duplicates} duplicate filenames in temperature data - this will cause join issues")
    
    # Perform left join on filename
    original_count = len(butterfly_df)
    merged_df = butterfly_df.merge(
        temp_df, 
        left_on='image_filename', 
        right_on='filename', 
        how='left'
    ).drop('filename', axis=1)  # Remove redundant filename column
    
    # Validation checks
    if len(merged_df) != original_count:
        raise RuntimeError(f"Join changed row count: {original_count} → {len(merged_df)}. This indicates duplicate temperature records.")
    
    missing_temp = merged_df['temperature'].isna().sum()
    if missing_temp > 0:
        print(f"⚠️ Warning: {missing_temp} butterfly observations missing temperature data ({missing_temp/len(merged_df)*100:.1f}%)")
        
        # Show some examples of missing temperature data
        missing_examples = merged_df[merged_df['temperature'].isna()]['image_filename'].head(5).tolist()
        print(f"   Examples of missing files: {missing_examples}")
    
    successful_joins = len(merged_df) - missing_temp
    print(f"✅ Successfully joined temperature for {successful_joins}/{len(merged_df)} observations")
    
    return merged_df


def create_lag_analysis(df: pd.DataFrame, 
                       lag_minutes: int = 30,
                       remove_zero_pairs: bool = True,
                       tolerance_minutes: int = 5) -> pd.DataFrame:
    """Create lag analysis comparing observations at time t with t-lag"""
    if df.empty:
        raise ValueError("Cannot create lag analysis from empty DataFrame")
    
    # Create deployment_day column for grouping and random effects
    df_with_day = df.copy()
    df_with_day['deployment_day'] = (
        df_with_day['deployment_id'] + '_' + 
        df_with_day['timestamp'].dt.strftime('%Y%m%d')
    )
    
    results = []
    lag_delta = pd.Timedelta(minutes=lag_minutes)
    tolerance_delta = pd.Timedelta(minutes=tolerance_minutes)
    
    # Process each deployment-day independently
    for deployment_day, group in df_with_day.groupby('deployment_day'):
        group_sorted = group.sort_values('timestamp').reset_index(drop=True)
        
        for _, current_obs in group_sorted.iterrows():
            current_time = current_obs['timestamp']
            target_lag_time = current_time - lag_delta
            
            # Find observation within tolerance window
            time_diffs = (group_sorted['timestamp'] - target_lag_time).abs()
            within_tolerance = time_diffs <= tolerance_delta
            
            if not within_tolerance.any():
                continue  # Skip if no lagged observation found
            
            # Get closest observation within tolerance
            closest_idx = time_diffs[within_tolerance].idxmin()
            lag_obs = group_sorted.loc[closest_idx]
            
            # Apply zero-pair filter if requested
            if remove_zero_pairs:
                if (current_obs['total_butterflies'] == 0 and 
                    lag_obs['total_butterflies'] == 0):
                    continue
            
            # Create lag analysis record
            lag_record = {
                'deployment_day': deployment_day,
                'deployment_id': current_obs['deployment_id'],
                
                # Current time (t) data
                'timestamp_t': current_obs['timestamp'],
                'image_filename_t': current_obs['image_filename'],
                'total_butterflies_t': current_obs['total_butterflies'],
                'butterflies_direct_sun_t': current_obs['butterflies_direct_sun'],
                'temperature_t': current_obs['temperature'],
                
                # Lagged time (t-lag) data  
                'timestamp_t_lag': lag_obs['timestamp'],
                'image_filename_t_lag': lag_obs['image_filename'],
                'total_butterflies_t_lag': lag_obs['total_butterflies'],
                'butterflies_direct_sun_t_lag': lag_obs['butterflies_direct_sun'],
                'temperature_t_lag': lag_obs['temperature'],
                
                # Derived metrics
                'actual_lag_minutes': (current_time - lag_obs['timestamp']).total_seconds() / 60,
                'temperature_avg': (current_obs['temperature'] + lag_obs['temperature']) / 2,
                'butterfly_difference': current_obs['total_butterflies'] - lag_obs['total_butterflies'],
                'butterfly_difference_cbrt': np.sign(current_obs['total_butterflies'] - lag_obs['total_butterflies']) * np.power(np.abs(current_obs['total_butterflies'] - lag_obs['total_butterflies']), 1/3),
                'butterfly_difference_log': np.sign(current_obs['total_butterflies'] - lag_obs['total_butterflies']) * np.log(np.maximum(np.abs(current_obs['total_butterflies'] - lag_obs['total_butterflies']), 0.1)),
            }
            
            results.append(lag_record)
    
    if not results:
        print("No valid lag pairs found")
        return pd.DataFrame()
    
    lag_df = pd.DataFrame(results)
    
    # Summary statistics
    print(f"\n=== LAG ANALYSIS SUMMARY ===")
    print(f"Original observations: {len(df)}")
    print(f"Valid lag pairs created: {len(lag_df)}")
    print(f"Lag period: {lag_minutes} ± {tolerance_minutes} minutes")
    print(f"Actual lag range: {lag_df['actual_lag_minutes'].min():.1f} to {lag_df['actual_lag_minutes'].max():.1f} minutes")
    print(f"Mean actual lag: {lag_df['actual_lag_minutes'].mean():.1f} minutes")
    
    if remove_zero_pairs:
        total_possible_pairs = sum(len(group) - 1 for _, group in df_with_day.groupby('deployment_day') if len(group) > 1)
        zero_pairs_filtered = total_possible_pairs - len(lag_df)
        print(f"Zero-pairs filtered out: ~{zero_pairs_filtered}")
    
    unique_deployment_days = lag_df['deployment_day'].nunique()
    print(f"Deployment-days with lag pairs: {unique_deployment_days}")
    
    return lag_df


def add_wind_data(lag_df: pd.DataFrame, 
                 deployments_df: pd.DataFrame,
                 wind_db_dir: str = 'data/wind',
                 lag_minutes: int = 30) -> pd.DataFrame:
    """Add wind metrics to lag analysis data by querying SQLite databases"""
    if lag_df.empty:
        raise ValueError("Cannot add wind data to empty lag DataFrame")
    
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
    
    print(f"Found {len(db_files)} wind databases: {list(db_files.keys())}")
    
    results = []
    diagnostic_issues = []
    
    for _, row in lag_df.iterrows():
        deployment_id = row['deployment_id']
        
        # Get wind meter name for this deployment
        wind_meter_name = wind_meter_map.get(deployment_id)
        if not wind_meter_name:
            raise ValueError(f"No wind meter found for deployment {deployment_id}")
        
        # Find corresponding database
        db_path = db_files.get(wind_meter_name)
        if not db_path:
            print(f"⚠️ Warning: No wind database found for {wind_meter_name} (deployment {deployment_id})")
            # Add row with NaN wind values
            wind_metrics = {
                'avg_sustained': np.nan,
                'max_gust': np.nan, 
                'mode_gust': np.nan,
                'gust_sd': np.nan,
                'wind_obs_count': 0
            }
        else:
            # Query wind data for the lag period
            wind_metrics = _query_wind_metrics(
                db_path, 
                row['timestamp_t_lag'], 
                row['timestamp_t']
            )
            
            # Check for diagnostic issues - calculate expected range based on lag period
            # Wind data is typically recorded every minute, so expect ~lag_minutes observations
            # Allow ±5 minute tolerance around the expected count
            expected_min = lag_minutes - 5
            expected_max = lag_minutes + 5
            obs_count = wind_metrics['wind_obs_count']
            if obs_count < expected_min or obs_count > expected_max:
                diagnostic_issues.append({
                    'deployment_day': row['deployment_day'],
                    'timestamp_t': row['timestamp_t'],
                    'wind_meter': wind_meter_name,
                    'obs_count': obs_count,
                    'expected_range': f'{expected_min}-{expected_max}'
                })
        
        # Combine original row with wind metrics
        result_row = row.to_dict()
        result_row.update(wind_metrics)
        results.append(result_row)
    
    # Create final DataFrame
    wind_df = pd.DataFrame(results)
    
    # Print diagnostics
    print(f"\n=== WIND DATA INTEGRATION SUMMARY ===")
    print(f"Lag pairs processed: {len(wind_df)}")
    missing_wind = wind_df['wind_obs_count'].eq(0).sum()
    if missing_wind > 0:
        print(f"⚠️ Pairs with missing wind data: {missing_wind}")
    
    valid_wind = len(wind_df) - missing_wind
    print(f"✅ Pairs with wind data: {valid_wind}")
    
    if valid_wind > 0:
        obs_stats = wind_df[wind_df['wind_obs_count'] > 0]['wind_obs_count']
        print(f"Wind observations per lag period: {obs_stats.min():.0f}-{obs_stats.max():.0f} (mean: {obs_stats.mean():.1f})")
    
    # Report and filter out diagnostic issues
    expected_min = lag_minutes - 5
    expected_max = lag_minutes + 5
    if diagnostic_issues:
        print(f"\n⚠️ FILTERING OUT {len(diagnostic_issues)} pairs with unusual wind observation counts:")
        # Show first 10 examples
        for issue in diagnostic_issues[:10]:
            print(f"  {issue['deployment_day']} at {issue['timestamp_t']}: {issue['obs_count']} obs (expected: {issue['expected_range']})")
        if len(diagnostic_issues) > 10:
            print(f"  ... and {len(diagnostic_issues) - 10} more")
        
        # Filter out problematic pairs
        problematic_indices = set()
        for issue in diagnostic_issues:
            mask = ((wind_df['deployment_day'] == issue['deployment_day']) & 
                   (wind_df['timestamp_t'] == issue['timestamp_t']))
            problematic_indices.update(wind_df[mask].index.tolist())
        
        wind_df_filtered = wind_df.drop(index=list(problematic_indices)).reset_index(drop=True)
        
        print(f"\n📊 FILTERING SUMMARY:")
        print(f"Original lag pairs: {len(wind_df)}")
        print(f"Filtered out: {len(problematic_indices)}")
        print(f"Final clean dataset: {len(wind_df_filtered)}")
        
        return wind_df_filtered
    else:
        print(f"✅ All wind observation counts within expected range ({expected_min}-{expected_max})")
        return wind_df


def _query_wind_metrics(db_path: Path, start_time: datetime, end_time: datetime) -> Dict:
    """Query wind metrics from SQLite database for given time period"""
    try:
        with sqlite3.connect(str(db_path)) as conn:
            # Convert timestamps to strings in local time format
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
                return {
                    'avg_sustained': np.nan,
                    'max_gust': np.nan,
                    'mode_gust': np.nan, 
                    'gust_sd': np.nan,
                    'wind_obs_count': 0
                }
            
            # Convert string columns to numeric, handling whitespace
            wind_data['speed'] = pd.to_numeric(wind_data['speed'].astype(str).str.strip(), errors='coerce')
            wind_data['gust'] = pd.to_numeric(wind_data['gust'].astype(str).str.strip(), errors='coerce')
            
            # Calculate wind metrics
            sustained_speeds = wind_data['speed'].dropna()
            gust_speeds = wind_data['gust'].dropna()
            
            # Average sustained wind speed
            avg_sustained = sustained_speeds.mean() if len(sustained_speeds) > 0 else np.nan
            
            # Maximum gust speed
            max_gust = gust_speeds.max() if len(gust_speeds) > 0 else np.nan
            
            # Mode (most frequent) gust speed using pandas
            if len(gust_speeds) > 0:
                mode_gust = gust_speeds.mode().iloc[0] if len(gust_speeds.mode()) > 0 else gust_speeds.iloc[0]
            else:
                mode_gust = np.nan
            
            # Standard deviation of gust speeds
            gust_sd = gust_speeds.std() if len(gust_speeds) > 1 else np.nan
            
            return {
                'avg_sustained': avg_sustained,
                'max_gust': max_gust,
                'mode_gust': mode_gust,
                'gust_sd': gust_sd,
                'wind_obs_count': len(wind_data)
            }
            
    except Exception as e:
        raise RuntimeError(f"Failed to query wind data from {db_path}: {e}")


def export_final_dataset(df: pd.DataFrame, output_path: str = 'data/monarch_lag_analysis_final.csv') -> str:
    """Export final research dataset to CSV"""
    if df.empty:
        raise ValueError("Cannot export empty dataset")
    
    # Create output directory if it doesn't exist
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    
    # Export to CSV
    df.to_csv(output_path, index=False)
    
    print(f"\n=== DATASET EXPORT SUMMARY ===")
    print(f"✅ Exported {len(df)} observations to: {output_path}")
    print(f"📊 Dataset shape: {df.shape}")
    print(f"📁 File size: {output_path.stat().st_size / 1024:.1f} KB")
    
    # Show column summary
    print(f"\n📋 FINAL DATASET COLUMNS ({len(df.columns)}):")
    for i, col in enumerate(df.columns, 1):
        print(f"  {i:2d}. {col}")
    
    # Data completeness check
    missing_data = df.isnull().sum()
    if missing_data.any():
        print(f"\n⚠️ Missing data summary:")
        for col, count in missing_data[missing_data > 0].items():
            pct = count / len(df) * 100
            print(f"  {col}: {count} missing ({pct:.1f}%)")
    else:
        print(f"\n✅ Complete dataset - no missing values!")
    
    return str(output_path)


def add_deployment_metadata(df: pd.DataFrame, deployments_df: pd.DataFrame) -> pd.DataFrame:
    """Add deployment metadata fields for analysis"""
    if df.empty:
        raise ValueError("Cannot add metadata to empty DataFrame")
    
    # Select the metadata columns we want
    metadata_cols = ['deployment_id', 'Observer', 'horizontal_dist_to_cluster_m', 'grove', 'view_id']
    
    # Check that all required columns exist
    missing_cols = [col for col in metadata_cols if col not in deployments_df.columns]
    if missing_cols:
        raise ValueError(f"Missing columns in deployments data: {missing_cols}")
    
    deployment_metadata = deployments_df[metadata_cols].copy()
    
    # Join metadata to the main dataset
    original_count = len(df)
    merged_df = df.merge(
        deployment_metadata,
        on='deployment_id',
        how='left'
    )
    
    # Validation
    if len(merged_df) != original_count:
        raise RuntimeError(f"Metadata join changed row count: {original_count} → {len(merged_df)}")
    
    # Check for missing metadata
    metadata_check_cols = ['Observer', 'horizontal_dist_to_cluster_m', 'grove', 'view_id']
    missing_metadata = merged_df[metadata_check_cols].isnull().sum()
    
    if missing_metadata.any():
        print(f"⚠️ Missing deployment metadata:")
        for col, count in missing_metadata[missing_metadata > 0].items():
            pct = count / len(merged_df) * 100
            print(f"  {col}: {count} missing ({pct:.1f}%)")
    else:
        print(f"✅ All deployment metadata successfully joined")
    
    print(f"📋 Added deployment metadata: Observer, horizontal_dist_to_cluster_m, grove, view_id")
    
    return merged_df




def parse_arguments():
    """Parse command line arguments"""
    parser = argparse.ArgumentParser(
        description="Data preprocessing script with configurable lag analysis",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter
    )
    
    # Core parameters
    parser.add_argument('--lag-minutes', type=int, default=30,
                        help='Lag period for analysis in minutes')
    parser.add_argument('--tolerance-minutes', type=int, default=5,
                        help='Tolerance window for finding lagged observations in minutes')
    parser.add_argument('--keep-zero-pairs', action='store_true',
                        help='Keep zero-zero butterfly pairs (default is to remove them)')
    
    # Data paths
    parser.add_argument('--json-dir', default='data/deployments',
                        help='Directory containing JSON deployment files')
    parser.add_argument('--temp-file', default='data/temperature_data_2023.csv',
                        help='Temperature data file')
    parser.add_argument('--wind-db-dir', default='data/wind',
                        help='Wind database directory')
    parser.add_argument('--deployments-file', default='data/deployments.csv',
                        help='Deployments metadata file')
    
    # Output options
    parser.add_argument('--output-file', 
                        help='Output CSV filename (auto-generated if not specified)')
    parser.add_argument('--output-dir', default='data',
                        help='Output directory')
    
    return parser.parse_args()


def generate_output_filename(args):
    """Generate output filename based on arguments"""
    if args.output_file:
        return args.output_file
    
    # Base filename with lag minutes
    filename = f"monarch_analysis_lag{args.lag_minutes}min"
    
    # Add zero pairs flag if keeping them
    if args.keep_zero_pairs:
        filename += "_withzeros"
    
    filename += ".csv"
    
    return str(Path(args.output_dir) / filename)


def load_deployments(deployments_file):
    """Load deployment data"""
    deployments = pd.read_csv(deployments_file)
    print(f"Loaded {len(deployments)} deployments")
    print(f"Columns: {list(deployments.columns)}")
    return deployments


def main():
    """Main function for data preprocessing"""
    args = parse_arguments()
    
    print(f"Starting data preprocessing with {args.lag_minutes}-minute lag...")
    print(f"Configuration:")
    print(f"  Lag minutes: {args.lag_minutes}")
    print(f"  Tolerance: ±{args.tolerance_minutes} minutes")
    print(f"  Zero pairs: {'kept' if args.keep_zero_pairs else 'removed'}")
    print(f"  JSON directory: {args.json_dir}")
    print(f"  Temperature file: {args.temp_file}")
    print(f"  Wind database directory: {args.wind_db_dir}")
    print(f"  Deployments file: {args.deployments_file}")
    
    # Generate output filename
    output_path = generate_output_filename(args)
    print(f"  Output file: {output_path}")
    
    # Load deployment metadata
    deployments = load_deployments(args.deployments_file)
    print(f"\nDeployment overview:")
    print(deployments.head())
    
    # Process butterfly counts from JSON files
    print(f"\n" + "="*50)
    print("Processing butterfly count data...")
    processor = ButterflyCountProcessor()
    butterfly_counts = processor.process_deployments(args.json_dir)
    
    if not butterfly_counts.empty:
        print(f"\nButterfly count data overview:")
        print(f"Shape: {butterfly_counts.shape}")
        print(f"Date range: {butterfly_counts['timestamp'].min()} to {butterfly_counts['timestamp'].max()}")
        print(f"\nFirst 10 rows:")
        print(butterfly_counts.head(10))
        
        # Validate timestamp intervals
        print(f"\n" + "="*50)
        print("Validating timestamp intervals...")
        interval_validation = processor.validate_intervals(butterfly_counts)
        
        if not interval_validation.empty:
            print(f"\nDetailed breakdown by deployment-day:")
            print(interval_validation.round(1))
        
        # Add temperature data
        print(f"\n" + "="*50)
        print("Adding temperature data...")
        try:
            final_data = add_temperature_data(butterfly_counts, args.temp_file)
            print(f"\nFinal dataset overview:")
            print(f"Shape: {final_data.shape}")
            print(f"Columns: {list(final_data.columns)}")
            print(f"\nTemperature stats:")
            temp_stats = final_data['temperature'].describe()
            print(f"  Count: {temp_stats['count']:.0f}")
            print(f"  Mean: {temp_stats['mean']:.1f}°C")
            print(f"  Range: {temp_stats['min']:.1f}°C to {temp_stats['max']:.1f}°C")
            
            print(f"\nFirst 5 rows with temperature:")
            print(final_data[['deployment_id', 'image_filename', 'timestamp', 'total_butterflies', 'temperature']].head())
            
        except Exception as e:
            print(f"❌ Failed to add temperature data: {e}")
            final_data = butterfly_counts
        
        # Create lag analysis
        print(f"\n" + "="*50)
        print("Creating lag analysis...")
        try:
            lag_data = create_lag_analysis(
                final_data, 
                lag_minutes=args.lag_minutes, 
                remove_zero_pairs=not args.keep_zero_pairs,
                tolerance_minutes=args.tolerance_minutes
            )
            
            if not lag_data.empty:
                print(f"\nLag analysis overview:")
                print(f"Shape: {lag_data.shape}")
                print(f"Columns: {list(lag_data.columns)}")
                
                print(f"\nFirst 3 lag pairs:")
                display_cols = ['deployment_day', 'timestamp_t', 'total_butterflies_t', 'total_butterflies_t_lag', 'actual_lag_minutes', 'temperature_avg']
                print(lag_data[display_cols].head(3))
                
        except Exception as e:
            print(f"❌ Failed to create lag analysis: {e}")
            return
        
        # Add wind data
        print(f"\n" + "="*50)
        print("Adding wind data...")
        try:
            final_data_with_wind = add_wind_data(lag_data, deployments, wind_db_dir=args.wind_db_dir, lag_minutes=args.lag_minutes)
            
            if not final_data_with_wind.empty:
                print(f"\nFinal dataset with wind:")
                print(f"Shape: {final_data_with_wind.shape}")
                
                # Show wind statistics
                wind_cols = ['avg_sustained', 'max_gust', 'mode_gust', 'gust_sd']
                wind_stats = final_data_with_wind[wind_cols].describe()
                print(f"\nWind metrics summary:")
                print(wind_stats.round(2))
                
                print(f"\nSample with all metrics:")
                sample_cols = ['deployment_day', 'total_butterflies_t', 'total_butterflies_t_lag', 
                             'temperature_avg', 'avg_sustained', 'max_gust', 'wind_obs_count']
                print(final_data_with_wind[sample_cols].head(3))
                
                # Add deployment metadata
                print(f"\n" + "="*50)
                print("Adding deployment metadata...")
                try:
                    final_dataset = add_deployment_metadata(final_data_with_wind, deployments)
                    
                    print(f"\nFinal dataset with metadata:")
                    print(f"Shape: {final_dataset.shape}")
                    
                    # Show metadata preview
                    metadata_cols = ['deployment_id', 'Observer', 'horizontal_dist_to_cluster_m', 'grove', 'view_id']
                    print(f"\nMetadata preview:")
                    print(final_dataset[metadata_cols].head(3))
                    
                except Exception as e:
                    print(f"❌ Failed to add deployment metadata: {e}")
                    final_dataset = final_data_with_wind
                
                # Export final dataset
                print(f"\n" + "="*50)
                print("Exporting final research dataset...")
                try:
                    export_path = export_final_dataset(final_dataset, output_path)
                    print(f"\n🎉 Data preprocessing pipeline completed successfully!")
                    print(f"📊 Final research dataset ready at: {export_path}")
                except Exception as e:
                    print(f"❌ Failed to export dataset: {e}")
                
        except Exception as e:
            print(f"❌ Failed to add wind data: {e}")
    else:
        print("No butterfly count data processed.")


if __name__ == "__main__":
    main()
