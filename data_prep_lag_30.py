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
    else:
        print("No butterfly count data processed.")


if __name__ == "__main__":
    main()
