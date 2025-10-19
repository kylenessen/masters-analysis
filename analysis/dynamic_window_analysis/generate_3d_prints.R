#!/usr/bin/env Rscript

# Generate 3D printable STL files for both sunset and 30-minute analyses

library(tidyverse)
library(mgcv)
library(nlme)
library(here)

# Source the 3D printing functions
source(here("analysis", "dynamic_window_analysis", "create_3d_printable_surface.R"))

# Create output directory for STL files
stl_dir <- here("thesis_exports", "3d_prints")
if (!dir.exists(stl_dir)) dir.create(stl_dir, recursive = TRUE)

# ----------------------------------------------------------------------------
# SUNSET ANALYSIS
# ----------------------------------------------------------------------------
cat("\n=== Generating Sunset Analysis 3D Print ===\n")

# Load data
data_file <- here("data", "monarch_daily_lag_analysis_sunset_window.csv")
daily_data <- readr::read_csv(data_file, show_col_types = FALSE)

daily_data <- daily_data %>%
  mutate(butterfly_diff_95th_sqrt = ifelse(butterfly_diff_95th >= 0,
    sqrt(butterfly_diff_95th), -sqrt(-butterfly_diff_95th)))

model_data_sunset <- daily_data %>%
  filter(metrics_complete >= 0.95) %>%
  arrange(deployment_id, observation_order_t) %>%
  mutate(
    deployment_id = factor(deployment_id),
    across(c(max_butterflies_t_1, lag_duration_hours,
      temp_min, temp_max, temp_at_max_count_t_1,
      wind_max_gust, sum_butterflies_direct_sun), as.numeric)) %>%
  filter(!is.na(butterfly_diff_95th_sqrt),
    !is.na(max_butterflies_t_1),
    !is.na(lag_duration_hours))

# Fit model
random_structure <- list(deployment_id = ~1)
ar1_cor <- corAR1(form = ~ observation_order_t | deployment_id)
best_formula <- as.formula('butterfly_diff_95th_sqrt ~ max_butterflies_t_1 + lag_duration_hours + ti(wind_max_gust, sum_butterflies_direct_sun)')

sunset_model <- gamm(best_formula, data = model_data_sunset, random = random_structure,
                    correlation = ar1_cor, method = 'REML')

# Generate STL
tryCatch({
  sunset_mesh <- create_3d_printable_surface_simple(
    gam_model = sunset_model$gam,
    x_var = "wind_max_gust",
    y_var = "sum_butterflies_direct_sun",
    data = model_data_sunset,
    output_file = file.path(stl_dir, "sunset_interaction_surface.stl"),
    n_grid = 100,
    print_width = 100,   # 100mm width
    print_depth = 100,   # 100mm depth
    print_height = 50,   # 50mm max height (larger range needs more height)
    base_height = 5      # 5mm base
  )
  cat("✓ Sunset STL created successfully\n")
}, error = function(e) {
  cat("✗ Error creating sunset STL:", e$message, "\n")
})

# ----------------------------------------------------------------------------
# 30-MINUTE ANALYSIS
# ----------------------------------------------------------------------------
cat("\n=== Generating 30-Minute Analysis 3D Print ===\n")

# Load data
data_file <- here("data", "monarch_analysis_lag30min.csv")
dat <- readr::read_csv(data_file, show_col_types = FALSE)

model_data_30min <- dat %>%
  filter(
    !is.na(butterfly_difference_cbrt),
    !is.na(total_butterflies_t_lag),
    !is.na(temperature_avg),
    !is.na(max_gust),
    !is.na(butterflies_direct_sun_t_lag),
    !is.na(observation_order_within_day_t),
    !is.na(deployment_day),
    !is.na(deployment_id),
    !is.na(Observer)
  )

# Fit best model (M50)
best_formula <- butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(time_within_day_t) + ti(max_gust, butterflies_direct_sun_t_lag)
random_structure <- list(deployment_id = ~1)
ar1_cor <- corAR1(form = ~ observation_order_within_day_t | deployment_day)

model_30min <- gamm(best_formula, data = model_data_30min, random = random_structure,
                   correlation = ar1_cor, method = 'REML')

# Generate STL
tryCatch({
  min30_mesh <- create_3d_printable_surface_simple(
    gam_model = model_30min$gam,
    x_var = "max_gust",
    y_var = "butterflies_direct_sun_t_lag",
    data = model_data_30min,
    output_file = file.path(stl_dir, "30min_interaction_surface.stl"),
    n_grid = 100,
    print_width = 100,   # 100mm width
    print_depth = 100,   # 100mm depth
    print_height = 40,   # 40mm max height (smaller range)
    base_height = 5      # 5mm base
  )
  cat("✓ 30-minute STL created successfully\n")
}, error = function(e) {
  cat("✗ Error creating 30-minute STL:", e$message, "\n")
})

# ----------------------------------------------------------------------------
# Create a comparison version with both surfaces
# ----------------------------------------------------------------------------
cat("\n=== Creating Scaled Comparison Prints ===\n")

# Create versions scaled to show relative effect sizes
tryCatch({
  # Sunset with proportional scaling
  sunset_comparison <- create_3d_printable_surface_simple(
    gam_model = sunset_model$gam,
    x_var = "wind_max_gust",
    y_var = "sum_butterflies_direct_sun",
    data = model_data_sunset,
    output_file = file.path(stl_dir, "sunset_scaled_for_comparison.stl"),
    n_grid = 80,
    print_width = 80,    # Slightly smaller for side-by-side printing
    print_depth = 80,
    print_height = 60,   # Taller to show larger effects
    base_height = 5
  )

  # 30-min with proportional scaling
  min30_comparison <- create_3d_printable_surface_simple(
    gam_model = model_30min$gam,
    x_var = "max_gust",
    y_var = "butterflies_direct_sun_t_lag",
    data = model_data_30min,
    output_file = file.path(stl_dir, "30min_scaled_for_comparison.stl"),
    n_grid = 80,
    print_width = 80,
    print_depth = 80,
    print_height = 35,   # Shorter to show smaller effects
    base_height = 5
  )

  cat("✓ Comparison STLs created successfully\n")
}, error = function(e) {
  cat("✗ Error creating comparison STLs:", e$message, "\n")
})

cat("\n=== 3D Print Generation Complete ===\n")
cat("STL files saved in:", stl_dir, "\n")
cat("\nRecommended print settings:\n")
cat("- Layer height: 0.2mm\n")
cat("- Infill: 20-30%\n")
cat("- Support: Not needed (base is flat)\n")
cat("- Scale in slicer if needed for your print bed\n")
cat("\nFiles created:\n")
list.files(stl_dir, pattern = "\\.stl$", full.names = FALSE)