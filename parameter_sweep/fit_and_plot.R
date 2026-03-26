#!/usr/bin/env Rscript
#
# Parameter sweep: fit models at every interval and plot R² vs interval.
#
# Reads:
#   parameter_sweep/within_day_pairs.csv
#   parameter_sweep/cross_day_pairs.csv
#
# Writes:
#   parameter_sweep/results.csv          (one row per interval × model)
#   parameter_sweep/best_model_curve.csv (one row per interval × anchor)
#   parameter_sweep/sweep_plot.png
#   parameter_sweep/sweep_plot.pdf

# Ensure library path is set for non-interactive sessions (e.g., nohup)
lib_paths <- c("/opt/homebrew/lib/R/4.5/site-library",
               "/opt/homebrew/Cellar/r/4.5.3/lib/R/library",
               .libPaths())
.libPaths(unique(lib_paths))

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(scales)
  library(mgcv)
  library(nlme)
})

# Set working directory to repo root when run via nohup
if (file.exists("parameter_sweep/fit_and_plot.R")) {
  out_dir <- "parameter_sweep"
} else if (file.exists("fit_and_plot.R")) {
  setwd("..")
  out_dir <- "parameter_sweep"
} else {
  out_dir <- "parameter_sweep"
}

# ============================================================================
# MODEL SPECIFICATIONS
# ============================================================================

# --- Within-day models (from monarch_gam_analysis.qmd, 48 models) ---
within_day_models <- list(
  "M1"  = "butterfly_difference_cbrt ~ total_butterflies_t_lag",
  "M2"  = "butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust",
  "M3"  = "butterfly_difference_cbrt ~ total_butterflies_t_lag + temperature_avg",
  "M4"  = "butterfly_difference_cbrt ~ total_butterflies_t_lag + butterflies_direct_sun_t_lag",
  "M5"  = "butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust + temperature_avg",
  "M6"  = "butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust + butterflies_direct_sun_t_lag",
  "M7"  = "butterfly_difference_cbrt ~ total_butterflies_t_lag + temperature_avg + butterflies_direct_sun_t_lag",
  "M8"  = "butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust + temperature_avg + butterflies_direct_sun_t_lag",
  "M9"  = "butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust * temperature_avg",
  "M10" = "butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust * butterflies_direct_sun_t_lag",
  "M11" = "butterfly_difference_cbrt ~ total_butterflies_t_lag + temperature_avg * butterflies_direct_sun_t_lag",
  "M12" = "butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust * temperature_avg + butterflies_direct_sun_t_lag",
  "M13" = "butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust * butterflies_direct_sun_t_lag + temperature_avg",
  "M14" = "butterfly_difference_cbrt ~ total_butterflies_t_lag + temperature_avg * butterflies_direct_sun_t_lag + max_gust",
  "M15" = "butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust * temperature_avg + max_gust * butterflies_direct_sun_t_lag + temperature_avg * butterflies_direct_sun_t_lag",
  "M16" = "butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust * temperature_avg * butterflies_direct_sun_t_lag",
  "M17" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(butterflies_direct_sun_t_lag)",
  "M18" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + temperature_avg + s(butterflies_direct_sun_t_lag)",
  "M19" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(max_gust) + temperature_avg + s(butterflies_direct_sun_t_lag)",
  "M20" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(butterflies_direct_sun_t_lag)",
  "M21" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(max_gust) + s(temperature_avg) + s(butterflies_direct_sun_t_lag)",
  "M22" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + temperature_avg + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)",
  "M23" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)",
  "M24" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(max_gust) + s(temperature_avg) + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)",
  "M25" = "butterfly_difference_cbrt ~ 1",
  "M26" = "butterfly_difference_cbrt ~ max_gust",
  "M27" = "butterfly_difference_cbrt ~ temperature_avg",
  "M28" = "butterfly_difference_cbrt ~ butterflies_direct_sun_t_lag",
  "M29" = "butterfly_difference_cbrt ~ max_gust + temperature_avg",
  "M30" = "butterfly_difference_cbrt ~ max_gust + butterflies_direct_sun_t_lag",
  "M31" = "butterfly_difference_cbrt ~ temperature_avg + butterflies_direct_sun_t_lag",
  "M32" = "butterfly_difference_cbrt ~ max_gust + temperature_avg + butterflies_direct_sun_t_lag",
  "M33" = "butterfly_difference_cbrt ~ max_gust * temperature_avg",
  "M34" = "butterfly_difference_cbrt ~ max_gust * butterflies_direct_sun_t_lag",
  "M35" = "butterfly_difference_cbrt ~ temperature_avg * butterflies_direct_sun_t_lag",
  "M36" = "butterfly_difference_cbrt ~ max_gust * temperature_avg + butterflies_direct_sun_t_lag",
  "M37" = "butterfly_difference_cbrt ~ max_gust * butterflies_direct_sun_t_lag + temperature_avg",
  "M38" = "butterfly_difference_cbrt ~ temperature_avg * butterflies_direct_sun_t_lag + max_gust",
  "M39" = "butterfly_difference_cbrt ~ max_gust * temperature_avg + max_gust * butterflies_direct_sun_t_lag + temperature_avg * butterflies_direct_sun_t_lag",
  "M40" = "butterfly_difference_cbrt ~ max_gust * temperature_avg * butterflies_direct_sun_t_lag",
  "M41" = "butterfly_difference_cbrt ~ s(temperature_avg) + s(butterflies_direct_sun_t_lag)",
  "M42" = "butterfly_difference_cbrt ~ temperature_avg + s(butterflies_direct_sun_t_lag)",
  "M43" = "butterfly_difference_cbrt ~ s(max_gust) + temperature_avg + s(butterflies_direct_sun_t_lag)",
  "M44" = "butterfly_difference_cbrt ~ s(temperature_avg) + s(butterflies_direct_sun_t_lag)",
  "M45" = "butterfly_difference_cbrt ~ s(max_gust) + s(temperature_avg) + s(butterflies_direct_sun_t_lag)",
  "M46" = "butterfly_difference_cbrt ~ temperature_avg + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)",
  "M47" = "butterfly_difference_cbrt ~ s(temperature_avg) + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)",
  "M48" = "butterfly_difference_cbrt ~ s(max_gust) + s(temperature_avg) + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)",

  # Tensor product interaction models (from fact-check branch best models)
  "M49" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + ti(max_gust, butterflies_direct_sun_t_lag)",
  "M50" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(time_within_day_t) + ti(max_gust, butterflies_direct_sun_t_lag)",
  "M51" = "butterfly_difference_cbrt ~ ti(max_gust, butterflies_direct_sun_t_lag)",
  "M52" = "butterfly_difference_cbrt ~ s(temperature_avg) + s(time_within_day_t) + ti(max_gust, butterflies_direct_sun_t_lag)",
  # Additional ti() variants
  "M53" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + ti(max_gust, butterflies_direct_sun_t_lag)",
  "M54" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + ti(max_gust, temperature_avg)",
  "M55" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + ti(temperature_avg, butterflies_direct_sun_t_lag)"
)

# --- Cross-day models (from monarch_daily_gam_analysis.qmd, M+B models) ---
cross_day_models <- list(
  # Without baseline
  "M1"  = "butterfly_diff_95th_sqrt ~ 1",
  "M2"  = "butterfly_diff_95th_sqrt ~ wind_max_gust_t_1",
  "M3"  = "butterfly_diff_95th_sqrt ~ temp_max_t_1",
  "M4"  = "butterfly_diff_95th_sqrt ~ temp_min_t_1",
  "M5"  = "butterfly_diff_95th_sqrt ~ temp_at_max_count_t_1",
  "M6"  = "butterfly_diff_95th_sqrt ~ sum_butterflies_direct_sun_t_1",
  "M8"  = "butterfly_diff_95th_sqrt ~ temp_max_t_1 + temp_min_t_1",
  "M9"  = "butterfly_diff_95th_sqrt ~ temp_max_t_1 + temp_at_max_count_t_1",
  "M10" = "butterfly_diff_95th_sqrt ~ temp_min_t_1 + temp_at_max_count_t_1",
  "M11" = "butterfly_diff_95th_sqrt ~ temp_max_t_1 + temp_min_t_1 + temp_at_max_count_t_1",
  "M12" = "butterfly_diff_95th_sqrt ~ wind_max_gust_t_1 + temp_max_t_1",
  "M13" = "butterfly_diff_95th_sqrt ~ wind_max_gust_t_1 + temp_min_t_1",
  "M14" = "butterfly_diff_95th_sqrt ~ wind_max_gust_t_1 + temp_at_max_count_t_1",
  "M15" = "butterfly_diff_95th_sqrt ~ wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "M16" = "butterfly_diff_95th_sqrt ~ temp_at_max_count_t_1 + sum_butterflies_direct_sun_t_1",
  "M17" = "butterfly_diff_95th_sqrt ~ temp_max_t_1 + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "M18" = "butterfly_diff_95th_sqrt ~ temp_min_t_1 + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "M19" = "butterfly_diff_95th_sqrt ~ temp_at_max_count_t_1 + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "M20" = "butterfly_diff_95th_sqrt ~ temp_max_t_1 + temp_min_t_1 + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "M21" = "butterfly_diff_95th_sqrt ~ temp_max_t_1 + temp_min_t_1 + temp_at_max_count_t_1 + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "M24" = "butterfly_diff_95th_sqrt ~ s(wind_max_gust_t_1)",
  "M25" = "butterfly_diff_95th_sqrt ~ s(temp_max_t_1)",
  "M26" = "butterfly_diff_95th_sqrt ~ s(temp_min_t_1)",
  "M27" = "butterfly_diff_95th_sqrt ~ s(temp_at_max_count_t_1)",
  "M28" = "butterfly_diff_95th_sqrt ~ s(sum_butterflies_direct_sun_t_1)",
  "M30" = "butterfly_diff_95th_sqrt ~ s(temp_max_t_1) + s(temp_min_t_1)",
  "M31" = "butterfly_diff_95th_sqrt ~ s(temp_at_max_count_t_1) + s(wind_max_gust_t_1)",
  "M32" = "butterfly_diff_95th_sqrt ~ s(temp_at_max_count_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "M33" = "butterfly_diff_95th_sqrt ~ s(wind_max_gust_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "M34" = "butterfly_diff_95th_sqrt ~ s(temp_at_max_count_t_1) + s(wind_max_gust_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "M35" = "butterfly_diff_95th_sqrt ~ s(temp_max_t_1) + s(temp_min_t_1) + s(wind_max_gust_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "M37" = "butterfly_diff_95th_sqrt ~ s(temp_max_t_1) + s(temp_min_t_1) + s(temp_at_max_count_t_1) + s(wind_max_gust_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "M38" = "butterfly_diff_95th_sqrt ~ temp_at_max_count_t_1 + s(wind_max_gust_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "M39" = "butterfly_diff_95th_sqrt ~ s(temp_at_max_count_t_1) + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "M40" = "butterfly_diff_95th_sqrt ~ s(temp_at_max_count_t_1) + wind_max_gust_t_1 + s(sum_butterflies_direct_sun_t_1)",
  "M41" = "butterfly_diff_95th_sqrt ~ temp_at_max_count_t_1 * wind_max_gust_t_1",
  "M42" = "butterfly_diff_95th_sqrt ~ temp_at_max_count_t_1 * sum_butterflies_direct_sun_t_1",
  "M43" = "butterfly_diff_95th_sqrt ~ wind_max_gust_t_1 * sum_butterflies_direct_sun_t_1",
  "M44" = "butterfly_diff_95th_sqrt ~ temp_at_max_count_t_1 * wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "M45" = "butterfly_diff_95th_sqrt ~ temp_at_max_count_t_1 + wind_max_gust_t_1 * sum_butterflies_direct_sun_t_1",
  "M46" = "butterfly_diff_95th_sqrt ~ temp_at_max_count_t_1 * wind_max_gust_t_1 * sum_butterflies_direct_sun_t_1",
  "M47" = "butterfly_diff_95th_sqrt ~ I(temp_max_t_1 - temp_min_t_1)",
  "M48" = "butterfly_diff_95th_sqrt ~ I(temp_max_t_1 - temp_min_t_1) + wind_max_gust_t_1",
  "M49" = "butterfly_diff_95th_sqrt ~ s(I(temp_max_t_1 - temp_min_t_1))",
  "M50" = "butterfly_diff_95th_sqrt ~ s(I(temp_max_t_1 - temp_min_t_1)) + s(wind_max_gust_t_1)",
  # With baseline
  "B1"  = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1",
  "B2"  = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + wind_max_gust_t_1",
  "B3"  = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_max_t_1",
  "B4"  = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_min_t_1",
  "B5"  = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_at_max_count_t_1",
  "B6"  = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + sum_butterflies_direct_sun_t_1",
  "B8"  = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_max_t_1 + temp_min_t_1",
  "B9"  = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_max_t_1 + temp_at_max_count_t_1",
  "B10" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_min_t_1 + temp_at_max_count_t_1",
  "B11" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_max_t_1 + temp_min_t_1 + temp_at_max_count_t_1",
  "B12" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + wind_max_gust_t_1 + temp_max_t_1",
  "B13" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + wind_max_gust_t_1 + temp_min_t_1",
  "B14" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + wind_max_gust_t_1 + temp_at_max_count_t_1",
  "B15" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "B16" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_at_max_count_t_1 + sum_butterflies_direct_sun_t_1",
  "B17" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_max_t_1 + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "B18" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_min_t_1 + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "B19" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_at_max_count_t_1 + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "B20" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_max_t_1 + temp_min_t_1 + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "B21" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_max_t_1 + temp_min_t_1 + temp_at_max_count_t_1 + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "B24" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(wind_max_gust_t_1)",
  "B25" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(temp_max_t_1)",
  "B26" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(temp_min_t_1)",
  "B27" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(temp_at_max_count_t_1)",
  "B28" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(sum_butterflies_direct_sun_t_1)",
  "B29" = "butterfly_diff_95th_sqrt ~ s(butterflies_95th_percentile_t_1)",
  "B29a" = "butterfly_diff_95th_sqrt ~ s(butterflies_95th_percentile_t_1) + wind_max_gust_t_1",
  "B29b" = "butterfly_diff_95th_sqrt ~ s(butterflies_95th_percentile_t_1) + temp_at_max_count_t_1",
  "B29c" = "butterfly_diff_95th_sqrt ~ s(butterflies_95th_percentile_t_1) + s(wind_max_gust_t_1)",
  "B29d" = "butterfly_diff_95th_sqrt ~ s(butterflies_95th_percentile_t_1) + s(temp_at_max_count_t_1)",
  "B30" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(temp_max_t_1) + s(temp_min_t_1)",
  "B31" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(temp_at_max_count_t_1) + s(wind_max_gust_t_1)",
  "B32" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(temp_at_max_count_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "B33" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(wind_max_gust_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "B34" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(temp_at_max_count_t_1) + s(wind_max_gust_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "B35" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(temp_max_t_1) + s(temp_min_t_1) + s(wind_max_gust_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "B37" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(temp_max_t_1) + s(temp_min_t_1) + s(temp_at_max_count_t_1) + s(wind_max_gust_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "B38" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_at_max_count_t_1 + s(wind_max_gust_t_1) + s(sum_butterflies_direct_sun_t_1)",
  "B39" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(temp_at_max_count_t_1) + wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "B40" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(temp_at_max_count_t_1) + wind_max_gust_t_1 + s(sum_butterflies_direct_sun_t_1)",
  "B41" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_at_max_count_t_1 * wind_max_gust_t_1",
  "B42" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_at_max_count_t_1 * sum_butterflies_direct_sun_t_1",
  "B43" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + wind_max_gust_t_1 * sum_butterflies_direct_sun_t_1",
  "B44" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_at_max_count_t_1 * wind_max_gust_t_1 + sum_butterflies_direct_sun_t_1",
  "B45" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_at_max_count_t_1 + wind_max_gust_t_1 * sum_butterflies_direct_sun_t_1",
  "B46" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + temp_at_max_count_t_1 * wind_max_gust_t_1 * sum_butterflies_direct_sun_t_1",
  "B47" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + I(temp_max_t_1 - temp_min_t_1)",
  "B48" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + I(temp_max_t_1 - temp_min_t_1) + wind_max_gust_t_1",
  "B49" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(I(temp_max_t_1 - temp_min_t_1))",
  "B50" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 + s(I(temp_max_t_1 - temp_min_t_1)) + s(wind_max_gust_t_1)",
  "B51" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 * wind_max_gust_t_1",
  "B52" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 * temp_at_max_count_t_1",
  "B53" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 * sum_butterflies_direct_sun_t_1",
  "B54" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 * wind_max_gust_t_1 + temp_at_max_count_t_1",
  "B55" = "butterfly_diff_95th_sqrt ~ butterflies_95th_percentile_t_1 * temp_at_max_count_t_1 + wind_max_gust_t_1",

  # Tensor product interaction models
  "BT1" = "butterfly_diff_95th_sqrt ~ s(butterflies_95th_percentile_t_1) + ti(wind_max_gust_t_1, sum_butterflies_direct_sun_t_1)",
  "BT2" = "butterfly_diff_95th_sqrt ~ s(butterflies_95th_percentile_t_1) + s(temp_at_max_count_t_1) + ti(wind_max_gust_t_1, sum_butterflies_direct_sun_t_1)",
  "BT3" = "butterfly_diff_95th_sqrt ~ s(butterflies_95th_percentile_t_1) + s(temp_max_t_1) + ti(wind_max_gust_t_1, sum_butterflies_direct_sun_t_1)",
  "BT4" = "butterfly_diff_95th_sqrt ~ ti(wind_max_gust_t_1, sum_butterflies_direct_sun_t_1)",
  "BT5" = "butterfly_diff_95th_sqrt ~ s(temp_at_max_count_t_1) + ti(wind_max_gust_t_1, sum_butterflies_direct_sun_t_1)",
  "BT6" = "butterfly_diff_95th_sqrt ~ s(butterflies_95th_percentile_t_1) + ti(temp_max_t_1, wind_max_gust_t_1)",
  "BT7" = "butterfly_diff_95th_sqrt ~ s(butterflies_95th_percentile_t_1) + ti(temp_at_max_count_t_1, sum_butterflies_direct_sun_t_1)"
)

# ============================================================================
# MODEL FITTING FUNCTION
# ============================================================================

fit_one_model <- function(formula_str, data, random_struct, corr_struct,
                          timeout_sec = 60) {
  # Use R's setTimeLimit to prevent infinite convergence loops
  result <- tryCatch({
    setTimeLimit(cpu = timeout_sec, elapsed = timeout_sec, transient = TRUE)
    on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))

    ctrl <- lmeControl(maxIter = 200, msMaxIter = 200, opt = "optim")

    if (is.null(corr_struct)) {
      m <- gamm(as.formula(formula_str),
                data = data,
                random = random_struct,
                method = "REML",
                control = ctrl)
    } else {
      m <- gamm(as.formula(formula_str),
                data = data,
                random = random_struct,
                correlation = corr_struct,
                method = "REML",
                control = ctrl)
    }
    s <- summary(m$gam)

    # gamm() returns empty dev.expl, so use r.sq (adjusted R²)
    # and compute marginal R² manually from fitted vs observed
    fitted_vals <- fitted(m$lme)
    response_var <- all.vars(as.formula(formula_str))[1]
    observed <- data[[response_var]]
    ss_res <- sum((observed - fitted_vals)^2, na.rm = TRUE)
    ss_tot <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
    marginal_r2 <- 1 - ss_res / ss_tot

    list(
      r_sq        = s$r.sq,          # adjusted R² from GAM component
      marginal_r2 = marginal_r2,     # marginal R² (fixed + random)
      aic         = AIC(m$lme),
      converged   = TRUE
    )
  }, error = function(e) {
    list(r_sq = NA_real_, marginal_r2 = NA_real_, aic = NA_real_, converged = FALSE)
  })

  # Reset time limit (belt and suspenders)
  setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  result
}

# ============================================================================
# RUN SWEEP FOR ONE SUBSET
# ============================================================================

run_sweep <- function(data, model_list, random_struct, corr_struct,
                      interval_label, anchor_label, n_obs, results_file) {
  cat(sprintf("  %s | %s | N=%d | %d models\n",
              interval_label, anchor_label, n_obs, length(model_list)))

  n_models <- length(model_list)
  for (mi in seq_along(model_list)) {
    model_name <- names(model_list)[mi]
    formula_str <- model_list[[model_name]]
    res <- fit_one_model(formula_str, data, random_struct, corr_struct)

    row <- data.frame(
      interval_label  = interval_label,
      anchor_strategy = anchor_label,
      model_id        = model_name,
      formula         = formula_str,
      n_obs           = n_obs,
      r_sq            = res$r_sq,
      marginal_r2     = res$marginal_r2,
      aic             = res$aic,
      converged       = res$converged,
      stringsAsFactors = FALSE
    )

    # Append incrementally
    needs_header <- !file.exists(results_file) ||
                    file.info(results_file)$size == 0
    write.table(row, results_file, append = TRUE, sep = ",",
                row.names = FALSE, col.names = needs_header,
                quote = TRUE)
  }
  cat(sprintf("    Done: %d/%d converged\n",
              sum(sapply(names(model_list), function(nm) TRUE)), n_models))
  flush.console()
}

# ============================================================================
# MAIN
# ============================================================================

cat("=" , rep("=", 69), "\n", sep = "")
cat("PARAMETER SWEEP: Fitting models\n")
cat(rep("=", 70), "\n", sep = "")

results_file <- file.path(out_dir, "results.csv")
# Clear previous results
if (file.exists(results_file)) file.remove(results_file)

# --- Within-day ---
within_file <- file.path(out_dir, "within_day_pairs.csv")
if (file.exists(within_file)) {
  cat("\n--- WITHIN-DAY MODELS ---\n")
  wd <- read_csv(within_file, show_col_types = FALSE)

  # Drop rows with missing key predictors
  wd <- wd %>% filter(
    !is.na(butterfly_difference_cbrt),
    !is.na(total_butterflies_t_lag),
    !is.na(max_gust),
    !is.na(temperature_avg),
    !is.na(butterflies_direct_sun_t_lag),
    !is.na(time_within_day_t),
    !is.na(observation_order_within_day_t),
    !is.na(deployment_day),
    !is.na(deployment_id)
  )

  intervals <- sort(unique(wd$interval_minutes))
  cat(sprintf("  %d intervals: %s\n", length(intervals),
              paste(intervals, collapse = ", ")))

  for (iv in intervals) {
    subset <- wd %>% filter(interval_minutes == iv)
    n <- nrow(subset)
    if (n < 20) next

    # Simplified random structure for robustness at smaller N
    rs <- list(deployment_id = ~1)

    # AR(1) only if enough obs per deployment_day
    obs_per_dd <- subset %>% count(deployment_day) %>% pull(n)
    if (median(obs_per_dd) >= 2) {
      cs <- corAR1(form = ~ observation_order_within_day_t | deployment_day)
    } else {
      cs <- NULL
    }

    run_sweep(subset, within_day_models, rs, cs,
              paste0(iv, "min"), "within_day", n, results_file)
  }
} else {
  cat("No within-day pairs file found, skipping.\n")
}

# --- Cross-day ---
cross_file <- file.path(out_dir, "cross_day_pairs.csv")
if (file.exists(cross_file)) {
  cat("\n--- CROSS-DAY MODELS ---\n")
  cd <- read_csv(cross_file, show_col_types = FALSE)

  cd <- cd %>% filter(
    !is.na(butterfly_diff_95th_sqrt),
    !is.na(butterflies_95th_percentile_t_1),
    !is.na(wind_max_gust_t_1),
    !is.na(temp_max_t_1),
    !is.na(temp_min_t_1),
    !is.na(temp_at_max_count_t_1),
    !is.na(sum_butterflies_direct_sun_t_1),
    !is.na(deployment_id),
    !is.na(day_sequence)
  )

  combos <- cd %>%
    distinct(interval_days, anchor_strategy) %>%
    arrange(interval_days, anchor_strategy)

  cat(sprintf("  %d interval × anchor combinations\n", nrow(combos)))

  for (i in seq_len(nrow(combos))) {
    iv_days <- combos$interval_days[i]
    anchor  <- combos$anchor_strategy[i]
    subset  <- cd %>%
      filter(interval_days == iv_days, anchor_strategy == anchor)
    n <- nrow(subset)
    if (n < 10) next

    rs <- list(deployment_id = ~1)

    # AR(1) on day_sequence if enough data
    n_deps <- n_distinct(subset$deployment_id)
    obs_per_dep <- subset %>% count(deployment_id) %>% pull(n)
    if (n_deps >= 2 && median(obs_per_dep) >= 3) {
      cs <- corAR1(form = ~ day_sequence | deployment_id)
    } else {
      cs <- NULL
    }

    run_sweep(subset, cross_day_models, rs, cs,
              paste0(iv_days, "day"), anchor, n, results_file)
  }
} else {
  cat("No cross-day pairs file found, skipping.\n")
}

# ============================================================================
# SUMMARIZE AND PLOT
# ============================================================================

cat("\n--- PLOTTING ---\n")

if (!file.exists(results_file)) {
  cat("No results to plot.\n")
  quit(save = "no")
}

results <- read_csv(results_file, show_col_types = FALSE)
cat(sprintf("Total model fits: %d (%d converged)\n",
            nrow(results), sum(results$converged, na.rm = TRUE)))

# Best model per interval × anchor (by deviance explained)
best <- results %>%
  filter(converged) %>%
  group_by(interval_label, anchor_strategy) %>%
  slice_max(r_sq, n = 1, with_ties = FALSE) %>%
  ungroup()

# Parse numeric interval for plotting
best <- best %>%
  mutate(
    interval_hours = case_when(
      grepl("min$", interval_label) ~
        as.numeric(gsub("min$", "", interval_label)) / 60,
      grepl("day$", interval_label) ~
        as.numeric(gsub("day$", "", interval_label)) * 24,
      TRUE ~ NA_real_
    )
  )

write_csv(best, file.path(out_dir, "best_model_curve.csv"))
cat("Saved best_model_curve.csv\n")

# --- Plot ---
p <- ggplot(best, aes(x = interval_hours, y = r_sq,
                       color = anchor_strategy, shape = anchor_strategy)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  scale_x_log10(
    name = "Interval length",
    breaks = c(0.5, 1, 2, 4, 8, 24, 48, 96, 168, 336),
    labels = c("30m", "1h", "2h", "4h", "8h", "1d", "2d", "4d", "1wk", "2wk")
  ) +
  scale_y_continuous(name = "Adjusted R² (best model)",
                     labels = scales::percent_format(accuracy = 1)) +
  scale_color_brewer(palette = "Set2", name = "Anchor strategy") +
  scale_shape_manual(values = c(16, 17, 15, 18, 8, 3),
                     name = "Anchor strategy") +
  labs(
    title = "Parameter Sweep: Optimal time interval for explaining variance",
    subtitle = paste0("N models tested per interval: ",
                      length(within_day_models), " (within-day), ",
                      length(cross_day_models), " (cross-day)"),
    caption = "Within-day = non-overlapping lag pairs. Cross-day = 5 anchor strategies."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  annotation_logticks(sides = "b")

# Add sample size annotation
p <- p +
  geom_text(aes(label = n_obs), vjust = -1.2, size = 2.5, show.legend = FALSE)

ggsave(file.path(out_dir, "sweep_plot.png"), p, width = 14, height = 8, dpi = 200)
ggsave(file.path(out_dir, "sweep_plot.pdf"), p, width = 14, height = 8)
cat("Saved sweep_plot.png and sweep_plot.pdf\n")

# --- Also plot AIC of best model ---
p_aic <- ggplot(best, aes(x = interval_hours, y = aic,
                           color = anchor_strategy)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.8) +
  scale_x_log10(
    name = "Interval length",
    breaks = c(0.5, 1, 2, 4, 8, 24, 48, 96, 168, 336),
    labels = c("30m", "1h", "2h", "4h", "8h", "1d", "2d", "4d", "1wk", "2wk")
  ) +
  scale_color_brewer(palette = "Set2", name = "Anchor strategy") +
  labs(
    title = "Best model AIC across intervals",
    y = "AIC (best model)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

ggsave(file.path(out_dir, "sweep_aic_plot.png"), p_aic, width = 14, height = 8, dpi = 200)
cat("Saved sweep_aic_plot.png\n")

# --- Print summary ---
cat("\n", rep("=", 70), "\n", sep = "")
cat("TOP INTERVALS BY R-SQUARED\n")
cat(rep("=", 70), "\n", sep = "")
best %>%
  arrange(desc(r_sq)) %>%
  head(15) %>%
  mutate(r_sq = round(r_sq, 4), marginal_r2 = round(marginal_r2, 4)) %>%
  select(interval_label, anchor_strategy, model_id, n_obs, r_sq, marginal_r2) %>%
  print(n = 15)

cat("\nDone.\n")
