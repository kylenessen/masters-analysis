#!/usr/bin/env python3
"""
Test script to verify day sequence functionality in data_prep_daily_lag.py
"""

import pandas as pd
import numpy as np
from datetime import datetime, date
from data_prep_daily_lag import create_daily_aggregates

def test_day_sequence():
    """Test that day_sequence is created correctly within each deployment"""

    # Create mock data with multiple deployments
    mock_data = []

    # Deployment SC1 with 3 days
    for i in range(3):
        mock_data.append({
            'deployment_id': 'SC1',
            'date': date(2023, 11, 15 + i),
            'timestamp': datetime(2023, 11, 15 + i, 12, 0, 0),
            'total_butterflies': i * 10,
            'butterflies_direct_sun': i * 5,
            'temperature': 20.0 + i
        })

    # Deployment SC2 with 2 days
    for i in range(2):
        mock_data.append({
            'deployment_id': 'SC2',
            'date': date(2023, 11, 17 + i),
            'timestamp': datetime(2023, 11, 17 + i, 12, 0, 0),
            'total_butterflies': (i + 1) * 15,
            'butterflies_direct_sun': i * 7,
            'temperature': 18.0 + i
        })

    # Deployment SC3 with 4 days
    for i in range(4):
        mock_data.append({
            'deployment_id': 'SC3',
            'date': date(2023, 11, 16 + i),
            'timestamp': datetime(2023, 11, 16 + i, 12, 0, 0),
            'total_butterflies': i * 8,
            'butterflies_direct_sun': i * 3,
            'temperature': 22.0 + i
        })

    mock_df = pd.DataFrame(mock_data)

    # Test the function
    print("Testing day sequence functionality...")
    daily_df = create_daily_aggregates(mock_df)

    print("\nResult DataFrame:")
    print(daily_df[['deployment_id', 'date', 'day_sequence', 'max_butterflies']].to_string())

    # Verify day sequences
    print("\n=== VERIFICATION ===")
    for deployment_id in daily_df['deployment_id'].unique():
        dep_data = daily_df[daily_df['deployment_id'] == deployment_id].sort_values('date')
        sequences = dep_data['day_sequence'].tolist()
        expected = list(range(1, len(dep_data) + 1))

        print(f"{deployment_id}: day_sequence = {sequences}, expected = {expected}")
        assert sequences == expected, f"Day sequence mismatch for {deployment_id}"

    print("\n✅ All tests passed! Day sequence works correctly within each deployment.")

    return daily_df

if __name__ == "__main__":
    test_df = test_day_sequence()