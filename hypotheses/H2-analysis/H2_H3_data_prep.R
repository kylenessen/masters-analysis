# H2 & H3 Data Preparation Script
# Prepare data for testing wind as disruptive force (H2) and intensity scaling (H3)
# Author: Kyle Nessen
# Created: 2025-08-12

# Load required libraries
library(tidyverse)
library(here)

# Set up paths
data_path <- here("results", "H1_interval_30min_terms_prepared.rds")

# Check if prepared data exists
if (!file.exists(data_path)) {
  stop("Prepared data not found. Please run interval_30min_terms.qmd first.")
}

# Load the prepared dataset from H1 analysis
message("Loading prepared data from H1 analysis...")
df <- read_rds(data_path)

# Display data structure
message("Original data dimensions: ", nrow(df), " rows, ", ncol(df), " columns")
message("Date range: ", min(df$datetime_utc, na.rm = TRUE), " to ", max(df$datetime_utc, na.rm = TRUE))

# Enhanced data preparation for H2 and H3 analyses
df_enhanced <- df %>%
  # Remove missing environmental data
  filter(!is.na(ambient_temp) & !is.na(sunlight_exposure_prop)) %>%
  
  # H2: General wind metrics (not threshold-based)
  mutate(
    # Log-transformed lagged abundance (for autocorrelation control)
    log_lag_abundance = log(abundance_index_t_minus_1 + 1),
    
    # H2: Continuous wind metrics - test which matters most
    mean_sustained_wind = sustained_minutes_above_2ms / 30,  # Proportion of time windy
    mean_gust_wind = gust_minutes_above_2ms / 30,            # Proportion of time gusty
    wind_variability = abs(gust_minutes_above_2ms - sustained_minutes_above_2ms) / 30,
    
    # H3: Multiple intensity metrics for scaling analysis
    wind_intensity_low = pmin(sustained_minutes_above_2ms, 5),     # Capped at 5 min
    wind_intensity_medium = pmax(0, pmin(sustained_minutes_above_2ms - 5, 10)), # 5-15 min
    wind_intensity_high = pmax(0, sustained_minutes_above_2ms - 15),            # >15 min
    
    # Standardized predictors (for model stability and comparison)
    sustained_wind_std = scale(sustained_minutes_above_2ms)[,1],
    gust_wind_std = scale(gust_minutes_above_2ms)[,1],
    wind_variability_std = scale(wind_variability)[,1],
    temp_std = scale(ambient_temp)[,1],
    sun_std = scale(sunlight_exposure_prop)[,1],
    
    # Binary indicators for H2 (any wind effects)
    any_sustained_wind = sustained_minutes_above_2ms > 0,
    any_gust_wind = gust_minutes_above_2ms > 0,
    
    # H3: Polynomial terms for non-linear scaling
    sustained_wind_squared = sustained_minutes_above_2ms^2,
    sustained_wind_cubed = sustained_minutes_above_2ms^3,
    
    # Interaction terms for exploration
    wind_temp_interaction = sustained_wind_std * temp_std,
    wind_sun_interaction = sustained_wind_std * sun_std
  )

# Create alternative threshold datasets for H3 sensitivity analysis
thresholds <- c(1.0, 1.5, 2.0, 2.5, 3.0, 3.5)

# Note: Since we only have the 2 m/s threshold data, we'll create proxy metrics
# This is a limitation acknowledged from H1 - we don't have raw wind speed data
message("Creating proxy threshold metrics based on 2 m/s data...")

threshold_data <- map_dfr(thresholds, function(thr) {
  df_enhanced %>%
    mutate(
      threshold = thr,
      # Scale the existing 2 m/s data to approximate other thresholds
      # This is imperfect but allows sensitivity analysis
      sustained_minutes_above_threshold = sustained_minutes_above_2ms * (2.0 / thr),
      gust_minutes_above_threshold = gust_minutes_above_2ms * (2.0 / thr)
    ) %>%
    # Ensure values don't exceed 30 minutes
    mutate(
      sustained_minutes_above_threshold = pmin(sustained_minutes_above_threshold, 30),
      gust_minutes_above_threshold = pmin(gust_minutes_above_threshold, 30)
    )
})

# Data quality checks
message("\n=== Data Quality Summary ===")
message("Enhanced dataset dimensions: ", nrow(df_enhanced), " rows, ", ncol(df_enhanced), " columns")
message("Complete cases: ", sum(complete.cases(df_enhanced)))
message("Proportion with zero abundance at t: ", round(mean(df_enhanced$abundance_index_t == 0), 3))
message("Range of abundance at t: ", min(df_enhanced$abundance_index_t), " to ", max(df_enhanced$abundance_index_t))
message("Mean sustained wind minutes > 2ms: ", round(mean(df_enhanced$sustained_minutes_above_2ms), 2))
message("Proportion with any wind > 2ms: ", round(mean(df_enhanced$any_sustained_wind), 3))

# Check for extreme values that might cause issues
extreme_abundance <- df_enhanced %>%
  filter(abundance_index_t > quantile(abundance_index_t, 0.99, na.rm = TRUE))

if (nrow(extreme_abundance) > 0) {
  message("Note: ", nrow(extreme_abundance), " observations with very high abundance (>99th percentile)")
}

# Summary statistics by site and labeler
site_summary <- df_enhanced %>%
  group_by(view_id) %>%
  summarise(
    n_obs = n(),
    mean_abundance = round(mean(abundance_index_t), 1),
    mean_wind = round(mean(sustained_minutes_above_2ms), 1),
    .groups = "drop"
  )

labeler_summary <- df_enhanced %>%
  group_by(labeler) %>%
  summarise(
    n_obs = n(),
    mean_abundance = round(mean(abundance_index_t), 1),
    .groups = "drop"
  )

message("\n=== Site Summary ===")
print(site_summary)

message("\n=== Labeler Summary ===")
print(labeler_summary)

# Save enhanced datasets
output_path_enhanced <- here("results", "H2_H3_enhanced_data.rds")
output_path_thresholds <- here("results", "H3_threshold_sensitivity_data.rds")

write_rds(df_enhanced, output_path_enhanced)
write_rds(threshold_data, output_path_thresholds)

message("\n=== Data Saved ===")
message("Enhanced data saved to: ", output_path_enhanced)
message("Threshold data saved to: ", output_path_thresholds)

# Create correlation matrix for H2 wind metrics
wind_correlations <- df_enhanced %>%
  select(sustained_minutes_above_2ms, gust_minutes_above_2ms, wind_variability) %>%
  cor(use = "complete.obs") %>%
  round(3)

message("\n=== Wind Metric Correlations ===")
print(wind_correlations)

message("\n=== Data preparation complete! ===")
message("Ready for H2 and H3 analyses.")