#!/usr/bin/env Rscript

# Export GAM surfaces to CSV for STL creation

library(tidyverse)
library(mgcv)
library(nlme)
library(here)

# Create output directory
csv_dir <- here("thesis_exports", "3d_prints", "csv")
if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)

# Function to export surface to CSV
export_surface_to_csv <- function(gam_model, x_var, y_var, data, output_file, n_grid = 100) {
  # Get data ranges
  if (!is.null(data)) {
    x_range <- range(data[[x_var]], na.rm = TRUE)
    y_range <- range(data[[y_var]], na.rm = TRUE)
  } else {
    x_range <- range(gam_model$model[[x_var]], na.rm = TRUE)
    y_range <- range(gam_model$model[[y_var]], na.rm = TRUE)
  }

  # Create prediction grid
  x_seq <- seq(x_range[1], x_range[2], length.out = n_grid)
  y_seq <- seq(y_range[1], y_range[2], length.out = n_grid)

  pred_grid <- expand.grid(x_seq, y_seq)
  names(pred_grid) <- c(x_var, y_var)

  # Add other variables
  formula_vars <- all.vars(formula(gam_model))
  for (var in formula_vars) {
    if (!(var %in% c(x_var, y_var, formula_vars[1])) && var %in% names(gam_model$model)) {
      if (is.numeric(gam_model$model[[var]])) {
        pred_grid[[var]] <- mean(gam_model$model[[var]], na.rm = TRUE)
      } else if (is.factor(gam_model$model[[var]])) {
        pred_grid[[var]] <- levels(gam_model$model[[var]])[1]
      }
    }
  }

  # Get predictions (UNCLIPPED)
  pred_vals <- predict(gam_model, newdata = pred_grid, type = "terms", se.fit = FALSE)

  # Find tensor product column
  ti_pattern <- paste0("ti\\(", x_var, ",", y_var, "\\)|",
                      "ti\\(", y_var, ",", x_var, "\\)")
  ti_col <- grep(ti_pattern, colnames(pred_vals), value = TRUE)

  if (length(ti_col) == 0) {
    stop("No tensor product interaction found")
  }

  # Get z values (UNCLIPPED)
  z_vals <- pred_vals[, ti_col]

  # Create data frame for export
  surface_data <- data.frame(
    x = pred_grid[[x_var]],
    y = pred_grid[[y_var]],
    z = z_vals
  )

  # Write to CSV
  write.csv(surface_data, output_file, row.names = FALSE)

  cat(sprintf("Surface data exported to: %s\n", output_file))
  cat(sprintf("Z range: %.2f to %.2f\n", min(z_vals), max(z_vals)))

  return(range(z_vals))
}

# ----------------------------------------------------------------------------
# SUNSET ANALYSIS
# ----------------------------------------------------------------------------
cat("\n=== Exporting Sunset Surface ===\n")

# Load and prepare data
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
      wind_max_gust, sum_butterflies_direct_sun), as.numeric)) %>%
  filter(!is.na(butterfly_diff_95th_sqrt),
    !is.na(max_butterflies_t_1),
    !is.na(lag_duration_hours))

# Fit model
random_structure <- list(deployment_id = ~1)
ar1_cor <- corAR1(form = ~ observation_order_t | deployment_id)
best_formula <- as.formula('butterfly_diff_95th_sqrt ~ max_butterflies_t_1 + lag_duration_hours + ti(wind_max_gust, sum_butterflies_direct_sun)')

sunset_model <- suppressWarnings(
  gamm(best_formula, data = model_data_sunset, random = random_structure,
       correlation = ar1_cor, method = 'REML')
)

# Export surface
sunset_range <- export_surface_to_csv(
  sunset_model$gam,
  "wind_max_gust",
  "sum_butterflies_direct_sun",
  model_data_sunset,
  file.path(csv_dir, "sunset_surface.csv"),
  n_grid = 100
)

# ----------------------------------------------------------------------------
# 30-MINUTE ANALYSIS
# ----------------------------------------------------------------------------
cat("\n=== Exporting 30-Minute Surface ===\n")

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

# Fit model
best_formula <- butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(time_within_day_t) + ti(max_gust, butterflies_direct_sun_t_lag)
random_structure <- list(deployment_id = ~1)
ar1_cor <- corAR1(form = ~ observation_order_within_day_t | deployment_day)

model_30min <- gamm(best_formula, data = model_data_30min, random = random_structure,
                   correlation = ar1_cor, method = 'REML')

# Export surface
min30_range <- export_surface_to_csv(
  model_30min$gam,
  "max_gust",
  "butterflies_direct_sun_t_lag",
  model_data_30min,
  file.path(csv_dir, "30min_surface.csv"),
  n_grid = 100
)

cat("\n=== Export Complete ===\n")
cat("CSV files saved in:", csv_dir, "\n")
cat("\nSurface ranges:\n")
cat(sprintf("  Sunset: %.2f to %.2f\n", sunset_range[1], sunset_range[2]))
cat(sprintf("  30-min: %.2f to %.2f\n", min30_range[1], min30_range[2]))