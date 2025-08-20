#!/usr/bin/env python3
"""
Data preprocessing script with 30-min lag analysis
"""

import pandas as pd
import json
import re
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


def load_deployments():
    """Load deployment data"""
    deployments = pd.read_csv('data/deployments.csv')
    print(f"Loaded {len(deployments)} deployments")
    print(f"Columns: {list(deployments.columns)}")
    return deployments


def main():
    """Main function for data preprocessing"""
    print("Starting data preprocessing with 30-day lag...")
    
    # Load deployment metadata
    deployments = load_deployments()
    print(f"\nDeployment overview:")
    print(deployments.head())
    
    # Process butterfly counts from JSON files
    print(f"\n" + "="*50)
    print("Processing butterfly count data...")
    processor = ButterflyCountProcessor()
    butterfly_counts = processor.process_deployments()
    
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
            final_data = add_temperature_data(butterfly_counts)
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
    else:
        print("No butterfly count data processed.")


if __name__ == "__main__":
    main()
