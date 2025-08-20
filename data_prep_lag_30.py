#!/usr/bin/env python3
"""
Data preprocessing script with 30-min lag analysis
"""

import pandas as pd


def load_deployments():
    """Load deployment data"""
    deployments = pd.read_csv('data/deployments.csv')
    print(f"Loaded {len(deployments)} deployments")
    print(f"Columns: {list(deployments.columns)}")
    return deployments


def main():
    """Main function for data preprocessing"""
    print("Starting data preprocessing with 30-day lag...")
    
    # Load deployment data
    deployments = load_deployments()
    print(f"\nDeployment overview:")
    print(deployments.head())


if __name__ == "__main__":
    main()
