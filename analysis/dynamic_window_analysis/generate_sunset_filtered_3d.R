#!/usr/bin/env Rscript

# Generate all 3D visualizations for filtered sunset analysis
# Filters: wind <= 10 m/s, butterflies <= 700

library(tidyverse)
library(mgcv)
library(nlme)
library(here)

# Source 3D plotting functions
source(here("analysis", "dynamic_window_analysis", "create_3d_interaction_plot.R"))

# Create output directories
export_dir <- here("thesis_exports", "sunset_filtered")
fig_dir <- file.path(export_dir, "figures")
csv_dir <- file.path(export_dir, "csv")
for (d in c(export_dir, fig_dir, csv_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ----------------------------------------------------------------------------
# Load and filter data
# ----------------------------------------------------------------------------
cat("\n=== Loading and Filtering Sunset Data ===\n")

data_file <- here("data", "monarch_daily_lag_analysis_sunset_window.csv")
daily_data <- readr::read_csv(data_file, show_col_types = FALSE)

# Transform response variable
daily_data <- daily_data %>%
  mutate(butterfly_diff_95th_sqrt = ifelse(butterfly_diff_95th >= 0,
    sqrt(butterfly_diff_95th), -sqrt(-butterfly_diff_95th)))

# Apply all filters including the new ones
cat("Applying filters:\n")
cat("  - metrics_complete >= 0.95\n")
cat("  - wind_max_gust <= 10 m/s\n")
cat("  - sum_butterflies_direct_sun <= 700\n")
cat("  - Remove missing values\n\n")

model_data <- daily_data %>%
  filter(metrics_complete >= 0.95) %>%
  filter(wind_max_gust <= 10) %>%  # NEW FILTER
  filter(sum_butterflies_direct_sun <= 700) %>%  # NEW FILTER
  arrange(deployment_id, observation_order_t) %>%
  mutate(
    deployment_id = factor(deployment_id),
    across(c(max_butterflies_t_1, lag_duration_hours,
      temp_min, temp_max, temp_at_max_count_t_1,
      wind_max_gust, sum_butterflies_direct_sun), as.numeric)) %>%
  filter(!is.na(butterfly_diff_95th_sqrt),
    !is.na(max_butterflies_t_1),
    !is.na(lag_duration_hours))

n_obs_original <- nrow(daily_data %>% filter(metrics_complete >= 0.95))
n_obs_filtered <- nrow(model_data)

cat(sprintf("Original observations (metrics_complete >= 0.95): %d\n", n_obs_original))
cat(sprintf("After wind/butterfly filters: %d\n", n_obs_filtered))
cat(sprintf("Observations removed: %d (%.1f%%)\n",
    n_obs_original - n_obs_filtered,
    (n_obs_original - n_obs_filtered) / n_obs_original * 100))

# Report ranges
cat("\nData ranges after filtering:\n")
cat(sprintf("  Wind: %.1f to %.1f m/s\n",
    min(model_data$wind_max_gust, na.rm = TRUE),
    max(model_data$wind_max_gust, na.rm = TRUE)))
cat(sprintf("  Butterflies in sun: %.0f to %.0f\n",
    min(model_data$sum_butterflies_direct_sun, na.rm = TRUE),
    max(model_data$sum_butterflies_direct_sun, na.rm = TRUE)))

# ----------------------------------------------------------------------------
# Fit GAM model
# ----------------------------------------------------------------------------
cat("\n=== Fitting GAM Model ===\n")

random_structure <- list(deployment_id = ~1)
ar1_cor <- corAR1(form = ~ observation_order_t | deployment_id)
best_formula <- as.formula('butterfly_diff_95th_sqrt ~ max_butterflies_t_1 + lag_duration_hours + ti(wind_max_gust, sum_butterflies_direct_sun)')

best_model <- suppressWarnings(
  gamm(best_formula, data = model_data, random = random_structure,
       correlation = ar1_cor, method = 'REML')
)

cat("Model fitted successfully\n")

# Get the interaction effect range
pred_grid <- expand.grid(
  wind_max_gust = seq(min(model_data$wind_max_gust), max(model_data$wind_max_gust), length.out = 100),
  sum_butterflies_direct_sun = seq(min(model_data$sum_butterflies_direct_sun),
                                   max(model_data$sum_butterflies_direct_sun), length.out = 100)
)
pred_grid$max_butterflies_t_1 <- mean(model_data$max_butterflies_t_1)
pred_grid$lag_duration_hours <- mean(model_data$lag_duration_hours)

pred_vals <- predict(best_model$gam, newdata = pred_grid, type = "terms", se.fit = FALSE)
ti_col <- grep("ti\\(", colnames(pred_vals), value = TRUE)
effect_range <- range(pred_vals[, ti_col], na.rm = TRUE)

cat(sprintf("Interaction effect range: %.2f to %.2f\n", effect_range[1], effect_range[2]))

# ----------------------------------------------------------------------------
# Create 3D visualizations
# ----------------------------------------------------------------------------
cat("\n=== Creating 3D Visualizations ===\n")

# 1. Static 3D surface plot
cat("Creating static 3D surface plot...\n")
png(file.path(fig_dir, "interaction_3d_surface.png"), width = 1000, height = 800)
surf_result <- create_3d_interaction_surface(
  gam_model = best_model$gam,
  x_var = "wind_max_gust",
  y_var = "sum_butterflies_direct_sun",
  data = model_data,
  n_grid = 60,
  theta = 35,
  phi = 25,
  xlab = "Wind speed (m/s)",
  ylab = "Butterflies in sun",
  zlab = "Interaction effect",
  main = "Wind × Sun Interaction (Filtered: wind ≤ 10 m/s, butterflies ≤ 700)",
  color_scheme = "coolwarm",
  shade = 0.3
)
dev.off()
cat("  ✓ interaction_3d_surface.png\n")

# 2. Contour + 3D side-by-side
cat("Creating contour + 3D plot...\n")
png(file.path(fig_dir, "interaction_contour_and_3d.png"), width = 1400, height = 700)
create_3d_surface_with_contour(
  gam_model = best_model$gam,
  x_var = "wind_max_gust",
  y_var = "sum_butterflies_direct_sun",
  data = model_data,
  n_grid = 60,
  xlab = "Wind speed (m/s)",
  ylab = "Butterflies in sun",
  main = "Filtered Tensor Product Interaction"
)
dev.off()
cat("  ✓ interaction_contour_and_3d.png\n")

# 3. Interactive HTML versions
if (requireNamespace("plotly", quietly = TRUE) && requireNamespace("htmlwidgets", quietly = TRUE)) {

  # Static interactive version
  cat("Creating interactive 3D plot...\n")
  p_3d_interactive <- create_3d_interaction_plotly(
    gam_model = best_model$gam,
    x_var = "wind_max_gust",
    y_var = "sum_butterflies_direct_sun",
    data = model_data,
    n_grid = 60,
    xlab = "Wind speed (m/s)",
    ylab = "Butterflies in sun",
    zlab = "Interaction effect",
    title = "Filtered GAM Surface (wind ≤ 10 m/s, butterflies ≤ 700)",
    use_diverging = TRUE,
    clip_symmetric = TRUE
  )
  htmlwidgets::saveWidget(p_3d_interactive,
                          file.path(fig_dir, "interaction_3d_interactive.html"),
                          selfcontained = TRUE)
  cat("  ✓ interaction_3d_interactive.html\n")

  # Animated interactive version
  cat("Creating animated interactive 3D plot...\n")
  p_3d_animated <- create_3d_interaction_plotly_animated(
    gam_model = best_model$gam,
    x_var = "wind_max_gust",
    y_var = "sum_butterflies_direct_sun",
    data = model_data,
    n_grid = 60,
    xlab = "Wind speed (m/s)",
    ylab = "Butterflies in sun",
    zlab = "Interaction effect",
    title = "Filtered GAM Surface - Animated",
    use_diverging = TRUE,
    clip_symmetric = TRUE,
    rotation_duration = 10000
  )
  htmlwidgets::saveWidget(p_3d_animated,
                          file.path(fig_dir, "interaction_3d_animated.html"),
                          selfcontained = TRUE)
  cat("  ✓ interaction_3d_animated.html\n")
}

# 4. Rotating GIF
if (requireNamespace("magick", quietly = TRUE)) {
  cat("Creating rotating GIF animation...\n")
  create_3d_rotation_gif_magick(
    gam_model = best_model$gam,
    x_var = "wind_max_gust",
    y_var = "sum_butterflies_direct_sun",
    data = model_data,
    n_grid = 60,
    xlab = "Wind speed (m/s)",
    ylab = "Butterflies in sun",
    main = "Wind × Sun (Filtered)",
    output_file = file.path(fig_dir, "interaction_3d_rotation.gif"),
    n_frames = 36,
    fps = 10,
    width = 800,
    height = 600
  )
  cat("  ✓ interaction_3d_rotation.gif\n")
}

# ----------------------------------------------------------------------------
# Export surface data for 3D printing
# ----------------------------------------------------------------------------
cat("\n=== Exporting Surface for 3D Printing ===\n")

# Export to CSV for STL creation
export_surface_to_csv <- function(gam_model, x_var, y_var, data, output_file, n_grid = 100) {
  x_range <- range(data[[x_var]], na.rm = TRUE)
  y_range <- range(data[[y_var]], na.rm = TRUE)

  x_seq <- seq(x_range[1], x_range[2], length.out = n_grid)
  y_seq <- seq(y_range[1], y_range[2], length.out = n_grid)

  pred_grid <- expand.grid(x_seq, y_seq)
  names(pred_grid) <- c(x_var, y_var)

  formula_vars <- all.vars(formula(gam_model))
  for (var in formula_vars) {
    if (!(var %in% c(x_var, y_var, formula_vars[1])) && var %in% names(gam_model$model)) {
      if (is.numeric(gam_model$model[[var]])) {
        pred_grid[[var]] <- mean(gam_model$model[[var]], na.rm = TRUE)
      }
    }
  }

  pred_vals <- predict(gam_model, newdata = pred_grid, type = "terms", se.fit = FALSE)
  ti_pattern <- paste0("ti\\(", x_var, ",", y_var, "\\)|ti\\(", y_var, ",", x_var, "\\)")
  ti_col <- grep(ti_pattern, colnames(pred_vals), value = TRUE)
  z_vals <- pred_vals[, ti_col]

  surface_data <- data.frame(
    x = pred_grid[[x_var]],
    y = pred_grid[[y_var]],
    z = z_vals
  )

  write.csv(surface_data, output_file, row.names = FALSE)
  cat(sprintf("Surface data exported to: %s\n", basename(output_file)))
  cat(sprintf("Z range (unclipped): %.2f to %.2f\n", min(z_vals), max(z_vals)))

  return(range(z_vals))
}

z_range <- export_surface_to_csv(
  best_model$gam,
  "wind_max_gust",
  "sum_butterflies_direct_sun",
  model_data,
  file.path(csv_dir, "sunset_filtered_surface.csv"),
  n_grid = 100
)

# ----------------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------------
cat("\n=== Summary ===\n")
cat(sprintf("Filtered dataset: %d observations\n", n_obs_filtered))
cat(sprintf("Wind range: %.1f - %.1f m/s\n",
    min(model_data$wind_max_gust), max(model_data$wind_max_gust)))
cat(sprintf("Butterflies range: %.0f - %.0f\n",
    min(model_data$sum_butterflies_direct_sun), max(model_data$sum_butterflies_direct_sun)))
cat(sprintf("Interaction effect range: %.2f to %.2f\n", effect_range[1], effect_range[2]))
cat(sprintf("\nAll outputs saved to: %s\n", export_dir))

# Create STL instruction file
stl_instructions <- paste0(
  "To create the 3D printable STL file, run:\n\n",
  "uv run --with numpy-stl python analysis/dynamic_window_analysis/create_stl_from_csv.py \\\n",
  "  thesis_exports/sunset_filtered/csv/sunset_filtered_surface.csv \\\n",
  "  thesis_exports/sunset_filtered/sunset_filtered.stl \\\n",
  "  --width 100 --depth 100 --height 45 --base 5\n"
)
cat("\n", stl_instructions)
writeLines(stl_instructions, file.path(export_dir, "create_stl_command.txt"))