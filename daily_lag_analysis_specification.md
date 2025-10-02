# Daily Lag Analysis Specification for Monarch Butterfly Data

## Overview
Transform 30-minute interval butterfly count data into daily aggregations, then create lag pairs comparing consecutive days within deployments to analyze how previous day's weather conditions affect current day butterfly abundance.

## Input Data
- **Butterfly counts**: JSON files in `data/deployments/` containing labeled photo data with timestamps and butterfly counts
- **Temperature data**: `data/temperature_data_2023.csv` with per-image temperature readings
- **Wind data**: SQLite databases in `data/wind/` with minute-level wind measurements
- **Deployment metadata**: `data/deployments.csv` with deployment information

## Data Processing Pipeline

### Phase 1: Photo-Level Data Preparation
1. **Load and filter butterfly count data**:
   - Parse JSON deployment files
   - Extract timestamp from image filenames (format: `YYYYMMDDHHMMSS`)
   - Remove night images (using hardcoded night periods in `NIGHT_PERIODS` dict)
   - Apply downsampling rules where applicable (30-minute intervals)
   - Map count categories to numeric values (e.g., '1-9' → 1, '10-99' → 10)

2. **Join temperature data**:
   - Left join on image_filename
   - Validate no row count changes after join

### Phase 2: Daily Aggregation
1. **Define valid days**:
   - Group photos by deployment_id and date
   - Count photos per deployment-day
   - **Exclude days with <15 or >25 photos**
   - Skip the first day of each deployment (no previous day for comparison)

2. **Calculate daily butterfly metrics** (for each valid deployment-day):
   - `max_butterflies`: Maximum total_butterflies count
   - `butterflies_95th_percentile`: 95th percentile of total_butterflies
   - `butterflies_top3_mean`: Average of top 3 butterfly counts (use zeros if <3 observations)
   - `sum_butterflies_direct_sun`: Sum of all butterflies_direct_sun counts throughout the day
   - `time_of_max`: Timestamp when maximum count occurred

3. **Calculate daily temperature metrics** (daytime only):
   - `temp_max`: Maximum temperature
   - `temp_min`: Minimum temperature
   - `temp_mean`: Mean temperature
   - `temp_at_max_count`: Temperature at time of maximum butterfly count
   - `hours_above_15C`: Hours where temperature ≥ 15°C
   - `degree_hours_above_15C`: Sum of (temperature - 15) for all hours where temp > 15°C

4. **Calculate daily wind metrics** (daytime only):
   - Query wind database for each deployment-day using deployment's wind_meter_name
   - Calculate for the active period (sunrise to sunset):
     - `wind_avg_sustained`: Mean of all sustained wind speeds
     - `wind_max_gust`: Maximum gust speed
     - `wind_gust_sum`: Sum of all gust values
     - `wind_gust_sum_above_2ms`: Sum of gust values where gust > 2 m/s
     - `wind_gust_hours`: Integral of gust speeds over time (∫gust dt)
     - `wind_minutes_above_2ms`: Count of minutes where gust ≥ 2 m/s
     - `wind_gust_sd`: Standard deviation of gust speeds
     - `wind_mode_gust`: Most frequent gust speed

### Phase 3: Lag Pair Creation
1. **Create consecutive day pairs**:
   - For each deployment, identify all valid consecutive days
   - Create records with:
     - Current day (t) metrics
     - Previous day (t-1) metrics
   - **Exclude pairs where both days have max_butterflies = 0**

2. **Calculate response variables**:
   - `butterfly_diff`: max_butterflies_t - max_butterflies_t_1
   - `butterfly_diff_cbrt`: cube root transformation of difference (preserving sign)
   - `butterfly_diff_log`: log transformation of |difference| + 1 (preserving sign)
   - Do the same for 95th percentile and top3 mean versions:
     - `butterfly_diff_95th`: butterflies_95th_percentile_t - butterflies_95th_percentile_t_1
     - `butterfly_diff_95th_cbrt`: cube root transformation
     - `butterfly_diff_95th_log`: log transformation
     - `butterfly_diff_top3`: butterflies_top3_mean_t - butterflies_top3_mean_t_1
     - `butterfly_diff_top3_cbrt`: cube root transformation
     - `butterfly_diff_top3_log`: log transformation

3. **Add metadata and identifiers**:
   - `deployment_day_id`: Unique identifier combining deployment_id and date
   - `deployment_id`, `Observer`, `horizontal_dist_to_cluster_m`, `grove`, `view_id`
   - `days_since_oct15`: Days since October 15th (season start)
   - `observation_order`: Sequential day number within deployment

### Phase 4: Quality Checks and Export
1. **Validation outputs**:
   - Summary statistics for each deployment (days analyzed, days excluded, etc.)
   - Distribution plots of photo counts per day
   - Histograms of three response variable transformations for all three metrics (9 plots total)
   - Summary table of daily aggregations before lag pairing

2. **Final dataset structure**:
   ```
   Columns:
   - Identifiers:
     * deployment_day_id_t, deployment_day_id_t_1
     * deployment_id, date_t, date_t_1

   - Response variables (3 metrics × 3 transformations = 9 columns):
     * butterfly_diff, butterfly_diff_cbrt, butterfly_diff_log
     * butterfly_diff_95th, butterfly_diff_95th_cbrt, butterfly_diff_95th_log
     * butterfly_diff_top3, butterfly_diff_top3_cbrt, butterfly_diff_top3_log

   - Current day butterfly metrics (for reference):
     * max_butterflies_t, butterflies_95th_percentile_t, butterflies_top3_mean_t
     * sum_butterflies_direct_sun_t, time_of_max_t

   - Previous day butterfly metrics:
     * max_butterflies_t_1, butterflies_95th_percentile_t_1, butterflies_top3_mean_t_1
     * sum_butterflies_direct_sun_t_1, time_of_max_t_1

   - Previous day weather predictors:
     * temp_max_t_1, temp_min_t_1, temp_mean_t_1
     * temp_at_max_count_t_1, hours_above_15C_t_1, degree_hours_above_15C_t_1
     * wind_avg_sustained_t_1, wind_max_gust_t_1, wind_gust_sum_t_1
     * wind_gust_sum_above_2ms_t_1, wind_gust_hours_t_1
     * wind_minutes_above_2ms_t_1, wind_gust_sd_t_1, wind_mode_gust_t_1

   - Metadata:
     * Observer, horizontal_dist_to_cluster_m, grove, view_id
     * days_since_oct15_t, observation_order_t
   ```

3. **Export**:
   - Primary output: `data/monarch_daily_lag_analysis.csv`
   - Supplementary output: `data/monarch_daily_summaries.csv` (daily aggregations before lag pairing)
   - Summary report: `data/daily_lag_analysis_summary.txt`
   - Diagnostic plots directory: `data/daily_lag_plots/`

## Key Constraints
- **Within-deployment only**: Never compare days across different deployments
- **Skip first day**: Each deployment's first day has no previous day
- **Photo count filter**: 15-25 photos per day required for valid day
- **Zero-pair removal**: Exclude if both days have max_butterflies = 0
- **Daytime only**: All metrics calculated for active period only (non-night images)

## Expected Output
- Approximately 50-100 lag pairs (estimated from ~20 deployments × 3-5 days each)
- CSV file ready for GAM analysis in R
- Comprehensive diagnostic plots showing:
  - Photo count distributions per deployment-day
  - Response variable distributions (9 histograms)
  - Correlation matrix of predictors
  - Time series plots of butterfly counts by deployment

## Implementation Notes
1. **Code reuse**: Adapt existing functions from `data_prep_lag.py`:
   - `ButterflyCountProcessor` class methods for JSON parsing
   - `_map_count_to_number()` for count conversion
   - `_is_night_image()` for filtering night photos
   - `_query_wind_metrics()` for wind database queries

2. **Performance considerations**:
   - Cache wind database connections when processing multiple days
   - Vectorize daily aggregations where possible
   - Use pandas groupby operations efficiently

3. **Error handling**:
   - Log deployments with insufficient days
   - Handle missing wind data gracefully
   - Validate temperature data coverage

4. **Diagnostic outputs during processing**:
   - Print summary after each phase
   - Track number of days excluded and reasons
   - Report deployments with data quality issues

## Validation Checklist
- [ ] Verify photo counts per day across deployments
- [ ] Check that all lag pairs are within-deployment
- [ ] Confirm wind metrics are calculated for daytime only
- [ ] Validate that response variable distributions are reasonable
- [ ] Ensure no data leakage (t-1 predictors only)
- [ ] Verify temporal ordering within deployments
- [ ] Check for missing values in critical columns
- [ ] Confirm zero-zero pairs are excluded