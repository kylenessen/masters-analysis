#!/usr/bin/env python3

import csv

def combine_deployment_data():
    """
    Combine deployment data from QGIS and label files.
    Matches on deployment ID and creates a merged dataset.
    """
    
    # Read QGIS data
    qgis_data = {}
    with open('data/deployments/deployments_QGIS.csv', 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            deployment_id = row['deployment_id']
            if deployment_id:
                qgis_data[deployment_id] = row
    
    # Read label data
    label_data = {}
    with open('data/deployments/deployments_label.csv', 'r', encoding='utf-8-sig') as f:
        reader = csv.DictReader(f)
        for row in reader:
            deployment_id = row['Deployment ID']
            if deployment_id:
                label_data[deployment_id] = row
    
    # Get all unique deployment IDs
    all_ids = set(qgis_data.keys()) | set(label_data.keys())
    
    # Create combined data
    combined_data = []
    
    # Define output columns (QGIS columns + label columns, avoiding duplicates)
    qgis_columns = ['camera_name', 'wind_meter_name', 'Deployed_time', 'Recovered_time', 
                    'notes', 'height_m', 'horizontal_dist_to_cluster_m', 'view_direction', 
                    'cluster_count', 'deployment_id', 'status', 'photo_interval_min', 
                    'monarchs_present', 'youtube_url', 'latitude', 'longitude']
    
    label_columns = ['Status', 'Percent Complete', 'Observer', 'Effort', 'Notes', 'Youtube Link']
    
    # Rename conflicting columns from label data
    label_column_mapping = {
        'Status': 'label_status',
        'Notes': 'label_notes', 
        'Youtube Link': 'label_youtube_url'
    }
    
    output_columns = qgis_columns + ['label_status', 'Percent Complete', 'Observer', 'Effort', 'label_notes', 'label_youtube_url']
    
    for deployment_id in sorted(all_ids):
        combined_row = {}
        
        # Add QGIS data
        if deployment_id in qgis_data:
            qgis_row = qgis_data[deployment_id]
            for col in qgis_columns:
                combined_row[col] = qgis_row.get(col, '')
        else:
            # Fill with empty values if not in QGIS
            for col in qgis_columns:
                combined_row[col] = ''
            combined_row['deployment_id'] = deployment_id
        
        # Add label data
        if deployment_id in label_data:
            label_row = label_data[deployment_id]
            for orig_col, new_col in label_column_mapping.items():
                combined_row[new_col] = label_row.get(orig_col, '')
            combined_row['Percent Complete'] = label_row.get('Percent Complete', '')
            combined_row['Observer'] = label_row.get('Observer', '')
            combined_row['Effort'] = label_row.get('Effort', '')
        else:
            # Fill with empty values if not in label data
            for new_col in ['label_status', 'Percent Complete', 'Observer', 'Effort', 'label_notes', 'label_youtube_url']:
                combined_row[new_col] = ''
        
        combined_data.append(combined_row)
    
    # Write combined data
    with open('data/deployments.csv', 'w', newline='') as f:
        writer = csv.DictWriter(f, fieldnames=output_columns)
        writer.writeheader()
        writer.writerows(combined_data)
    
    print(f"Combined {len(combined_data)} deployment records")
    print(f"QGIS records: {len(qgis_data)}")
    print(f"Label records: {len(label_data)}")
    print(f"Output written to: data/deployments.csv")
    
    # Show records that are in one file but not the other
    qgis_only = set(qgis_data.keys()) - set(label_data.keys())
    label_only = set(label_data.keys()) - set(qgis_data.keys())
    
    if qgis_only:
        print(f"Records only in QGIS file: {sorted(qgis_only)}")
    if label_only:
        print(f"Records only in label file: {sorted(label_only)}")

if __name__ == "__main__":
    combine_deployment_data()
