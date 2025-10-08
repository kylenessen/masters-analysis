#!/usr/bin/env Rscript

# Export script for sunset window GAM analysis
# Produces styled figures matching the 30-minute analysis aesthetics
# Outputs to thesis_exports/sunset/{figures,tables,text}

suppressPackageStartupMessages({
  library(tidyverse)
  library(mgcv)
  library(nlme)
  library(gratia)
  library(patchwork)
  library(here)
})

# ----------------------------------------------------------------------------
# Paths
# ----------------------------------------------------------------------------
export_dir <- here("thesis_exports", "sunset")
fig_dir <- file.path(export_dir, "figures")
tab_dir <- file.path(export_dir, "tables")
text_dir <- file.path(export_dir, "text")
for (d in c(export_dir, fig_dir, tab_dir, text_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ----------------------------------------------------------------------------
# Data
# ----------------------------------------------------------------------------
data_file <- here("data", "monarch_daily_lag_analysis_sunset_window.csv")
stopifnot(file.exists(data_file))
daily_data <- readr::read_csv(data_file, show_col_types = FALSE)

# Create sqrt transformed response
daily_data <- daily_data %>%
  mutate(
    butterfly_diff_95th_sqrt = ifelse(butterfly_diff_95th >= 0,
      sqrt(butterfly_diff_95th),
      -sqrt(-butterfly_diff_95th)
    )
  )

# Filter to quality data
model_data <- daily_data %>%
  filter(metrics_complete >= 0.95) %>%
  arrange(deployment_id, observation_order_t) %>%
  mutate(
    deployment_id = factor(deployment_id),
    across(c(
      max_butterflies_t_1, lag_duration_hours,
      temp_min, temp_max, temp_at_max_count_t_1,
      wind_max_gust, sum_butterflies_direct_sun
    ), as.numeric)
  ) %>%
  filter(
    !is.na(butterfly_diff_95th_sqrt),
    !is.na(max_butterflies_t_1),
    !is.na(lag_duration_hours)
  )

cat("Model data:", nrow(model_data), "observations\n")
cat("Deployments:", n_distinct(model_data$deployment_id), "\n")

# ----------------------------------------------------------------------------
# Load best model from comparison results
# ----------------------------------------------------------------------------
comparison_file <- here("analysis", "dynamic_window_analysis", "model_comparison_comprehensive.csv")
if (!file.exists(comparison_file)) {
  stop("Model comparison file not found. Run sunset_window_gam_analysis.qmd first.")
}

model_comparison <- readr::read_csv(comparison_file, show_col_types = FALSE)
best_model_name <- model_comparison %>%
  arrange(AICc) %>%
  slice(1) %>%
  pull(model)

cat("\nBest model:", best_model_name, "\n")
cat("Description:", model_comparison$description[1], "\n\n")

# ----------------------------------------------------------------------------
# Refit best model
# ----------------------------------------------------------------------------
random_structure <- list(deployment_id = ~1)
ar1_cor <- corAR1(form = ~ observation_order_t | deployment_id)

# Get formula from comparison table
best_formula_str <- model_comparison %>%
  filter(model == best_model_name) %>%
  pull(description) %>%
  gsub(".*: ", "", .) # Extract formula part after description

# Reconstruct formula based on model name
# M32: wind_max_gust × sum_butterflies_direct_sun interaction (should be tensor product)
best_formula <- as.formula(
  "butterfly_diff_95th_sqrt ~ max_butterflies_t_1 + lag_duration_hours + ti(wind_max_gust, sum_butterflies_direct_sun)"
)

cat("Fitting best model...\n")
best_model <- gamm(
  best_formula,
  data = model_data,
  random = random_structure,
  correlation = ar1_cor,
  method = "REML"
)

cat("Model fitted successfully.\n")
cat("Model formula:", deparse(formula(best_model$gam)), "\n")
cat("Smooth terms:\n")
print(summary(best_model$gam)$s.table)
cat("\n")

# ----------------------------------------------------------------------------
# Styling helpers (matching 30-min analysis)
# ----------------------------------------------------------------------------
custom_theme <- theme_minimal(base_size = 12) + theme(
  panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
  panel.grid.minor = element_line(color = "gray95", linewidth = 0.3),
  axis.text = element_text(color = "black"),
  axis.title = element_text(color = "black", face = "bold"),
  plot.title = element_blank()
)

lighten_color <- function(hex, amount = 0.12) {
  rgbv <- col2rgb(hex)
  out <- rgbv + (255 - rgbv) * amount
  rgb(out[1], out[2], out[3], maxColorValue = 255)
}

# Colors
col_prev <- "#9673c5"
col_lag <- "#79a44c"
col_prev_l <- lighten_color(col_prev)
col_lag_l <- lighten_color(col_lag)

# ----------------------------------------------------------------------------
# Partial effects plots
# ----------------------------------------------------------------------------
# Note: M32 has linear baseline terms (not smooths), so skip partial effects
# The key result is the tensor product interaction
cat("Note: Model has linear baseline terms (not smooth).\n")
cat("Skipping partial effects plots for linear terms.\n\n")

# ----------------------------------------------------------------------------
# Interaction plot (wind x sun)
# ----------------------------------------------------------------------------
# Check if model has tensor product interaction
smooth_terms <- rownames(summary(best_model$gam)$s.table)
has_tensor <- any(grepl("ti\\(.*wind_max_gust.*sum_butterflies_direct_sun", smooth_terms))

cat("Checking for tensor product interaction...\n")
cat("Smooth terms:", paste(smooth_terms, collapse = ", "), "\n")
cat("Has wind × sun tensor:", has_tensor, "\n\n")

if (has_tensor) {
  src_file <- here("analysis", "plot_binned_interaction.R")
  if (file.exists(src_file)) {
    source(src_file)
  }

  if (exists("create_binned_interaction_plot")) {
    cat("Creating interaction plot (wind x sun)...\n")
    p_inter_binned <- create_binned_interaction_plot(
      gam_model = best_model$gam,
      x_var = "wind_max_gust",
      y_var = "sum_butterflies_direct_sun",
      data = model_data,
      xlab = "Maximum wind speed (m/s)",
      ylab = "Butterflies in direct sun",
      n = 400,
      limits = c(-6, 6),
      nbreaks = 17,
      breaks = c(-6, -5, -4, -3, -2, -1, -0.5, 0, 0.5, 1, 2, 3, 4, 5, 6),
      labels = c("-6", "-5", "-4", "-3", "-2", "-1", "-0.5", "0", "+0.5", "+1", "+2", "+3", "+4", "+5", "+6"),
      too_far = 0.04,
      barheight = 34,
      barwidth = 1.0,
      legend_text_size = 8,
      legend_key_height_cm = 1.4
    )

    ggsave(file.path(fig_dir, "interaction_wind_x_sun_binned.png"), p_inter_binned, width = 7, height = 6, dpi = 300, bg = "white")
    cat("Saved: interaction_wind_x_sun_binned.png\n")
  } else {
    warning("create_binned_interaction_plot function not found. Skipping interaction plot.")
  }
} else {
  cat("Note: Best model has linear interaction only (not tensor product).\n")
  cat("Linear interaction plots require manual prediction grids.\n")
  cat("Skipping interaction surface plot.\n")
}

# ----------------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------------
cat("\n")
cat("Export complete. Outputs in:", export_dir, "\n")
cat("- Figures:", fig_dir, "\n")
cat("- Tables:", tab_dir, "\n")
cat("- Text:", text_dir, "\n")
