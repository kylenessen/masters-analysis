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

    # Calculate actual range of the interaction effect
    wind_range <- range(model_data$wind_max_gust, na.rm = TRUE)
    sun_range <- range(model_data$sum_butterflies_direct_sun, na.rm = TRUE)

    pred_grid <- expand.grid(
      wind_max_gust = seq(wind_range[1], wind_range[2], length.out = 100),
      sum_butterflies_direct_sun = seq(sun_range[1], sun_range[2], length.out = 100)
    )
    # Add other required variables at their means
    pred_grid$max_butterflies_t_1 <- mean(model_data$max_butterflies_t_1, na.rm = TRUE)
    pred_grid$lag_duration_hours <- mean(model_data$lag_duration_hours, na.rm = TRUE)

    pred_vals <- predict(best_model$gam, newdata = pred_grid, type = "terms", se.fit = FALSE)
    ti_col <- grep("ti\\(wind_max_gust,sum_butterflies_direct_sun\\)", colnames(pred_vals), value = TRUE)
    actual_range <- range(pred_vals[, ti_col], na.rm = TRUE)

    # Use -16 to 16 range with oob (out of bounds) handling for values > 16
    color_limits <- c(-16, 16)

    cat("Actual interaction effect range:", round(actual_range[1], 2), "to", round(actual_range[2], 2), "\n")
    cat("Using color limits:", color_limits[1], "to", color_limits[2], "\n")
    cat("Note: Values beyond ±16 will be capped at limit colors\n\n")

    # Create breaks every 2
    break_seq <- seq(-16, 16, by = 2)
    break_labels <- as.character(break_seq)
    break_labels[break_seq > 0] <- paste0("+", break_labels[break_seq > 0])
    # Add indicator for out-of-bounds
    break_labels[1] <- paste0(break_labels[1], "−")  # Use minus sign
    break_labels[length(break_labels)] <- paste0(break_labels[length(break_labels)], "+")

    p_inter_binned <- create_binned_interaction_plot(
      gam_model = best_model$gam,
      x_var = "wind_max_gust",
      y_var = "sum_butterflies_direct_sun",
      data = model_data,
      xlab = "Maximum wind speed (m/s)",
      ylab = "Butterflies in direct sun",
      n = 400,
      limits = color_limits,
      nbreaks = length(break_seq),
      breaks = break_seq,
      labels = break_labels,
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
# GAM basis dimension check (gam.check)
# ----------------------------------------------------------------------------
cat("Running gam.check...\n")
check_output <- capture.output(gam.check(best_model$gam, rep = 500))
writeLines(check_output, file.path(text_dir, "gam_check_output.txt"))
cat("Saved: gam_check_output.txt\n\n")

# ----------------------------------------------------------------------------
# Bivariate plot: Wind speed vs butterfly change
# ----------------------------------------------------------------------------
cat("Creating bivariate plot: wind speed vs butterfly change...\n")

# Calculate correlation
wind_corr <- cor(model_data$wind_max_gust, model_data$butterfly_diff_95th_sqrt,
                 use = "complete.obs")

# Fit linear model for trend line
lm_wind <- lm(butterfly_diff_95th_sqrt ~ wind_max_gust, data = model_data)
r_squared <- summary(lm_wind)$r.squared

cat(sprintf("Wind vs change correlation: r = %.2f, R² = %.4f\n", wind_corr, r_squared))

p_wind_bivariate <- ggplot(model_data, aes(x = wind_max_gust, y = butterfly_diff_95th_sqrt)) +
  geom_point(alpha = 0.5, size = 2, color = "#4d4d4d") +
  geom_vline(xintercept = 2, color = "red", linetype = "dashed", linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "gray65", linewidth = 0.5) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Maximum wind speed (m/s)",
    y = "Butterfly abundance change (cube root transformed)",
    title = sprintf("Maximum Wind Speed vs Butterfly Abundance Change\nCorrelation: r = %.2f", wind_corr)
  ) +
  custom_theme +
  theme(plot.title = element_text(size = 14, hjust = 0, face = "plain"))

ggsave(file.path(fig_dir, "wind_vs_change_bivariate.png"), p_wind_bivariate,
       width = 7, height = 6, dpi = 300, bg = "white")
cat("Saved: wind_vs_change_bivariate.png\n\n")

# Untransformed version
wind_corr_raw <- cor(model_data$wind_max_gust, model_data$butterfly_diff_95th,
                     use = "complete.obs")

# Fit linear model for untransformed data
lm_wind_raw <- lm(butterfly_diff_95th ~ wind_max_gust, data = model_data)
p_value_raw <- summary(lm_wind_raw)$p.value[2]  # p-value for the slope

cat(sprintf("Wind vs change correlation (untransformed): r = %.2f, p = %.4f\n", wind_corr_raw, p_value_raw))

p_wind_bivariate_raw <- ggplot(model_data, aes(x = wind_max_gust, y = butterfly_diff_95th)) +
  geom_point(alpha = 0.5, size = 2, color = "#4d4d4d") +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue", fill = "steelblue", alpha = 0.25, linewidth = 1) +
  geom_vline(xintercept = 2, color = "red", linetype = "dashed", linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "gray65", linewidth = 0.5) +
  scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Maximum wind speed (m/s)",
    y = "Butterfly abundance change",
    title = sprintf("Site Fidelity (Maximum cluster count to next day's sunset)\nr = %.2f, p = %.4f", wind_corr_raw, p_value_raw)
  ) +
  custom_theme +
  theme(plot.title = element_text(size = 14, hjust = 0, face = "plain"))

ggsave(file.path(fig_dir, "wind_vs_change_bivariate_untransformed.png"), p_wind_bivariate_raw,
       width = 7, height = 6, dpi = 300, bg = "white")
cat("Saved: wind_vs_change_bivariate_untransformed.png\n\n")

# ----------------------------------------------------------------------------
# Model diagnostics with autocorrelation plots
# ----------------------------------------------------------------------------
cat("Creating diagnostic plots...\n")

res_df <- tibble(
  fitted = fitted(best_model$lme),
  resid  = residuals(best_model$lme, type = "normalized")
)

# ACF plot
png(file.path(fig_dir, "diag_acf.png"), width = 900, height = 600)
acf(res_df$resid, main = "ACF of normalized residuals")
dev.off()
cat("Saved: diag_acf.png\n")

# PACF plot
png(file.path(fig_dir, "diag_pacf.png"), width = 900, height = 600)
pacf(res_df$resid, main = "PACF of normalized residuals")
dev.off()
cat("Saved: diag_pacf.png\n")

# Combined 1x2 diagnostic panel: Q-Q plot and Residuals vs Fitted
diag_scatter <- ggplot(res_df, aes(fitted, resid)) +
  geom_point(alpha = 0.25, size = 0.8, color = "#4d4d4d") +
  geom_smooth(se = FALSE, color = "#2c7fb8", linewidth = 0.8, method = "loess", span = 0.8) +
  geom_hline(yintercept = 0, color = "gray65") +
  labs(x = "Fitted values", y = "Standardized residuals") +
  theme_minimal()

diag_qq <- ggplot(res_df, aes(sample = resid)) +
  stat_qq(alpha = 0.25, size = 0.8, color = "#4d4d4d") +
  stat_qq_line(color = "#2c7fb8", linewidth = 0.8) +
  labs(x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal()

diag_1x2 <- wrap_plots(diag_qq, diag_scatter, nrow = 1, ncol = 2)
ggsave(file.path(fig_dir, "diag_qq_and_residuals_1x2.png"), diag_1x2, width = 12, height = 5, dpi = 300, bg = "white")
cat("Saved: diag_qq_and_residuals_1x2.png\n")

# ----------------------------------------------------------------------------
# Summary
# ----------------------------------------------------------------------------
cat("\n")
cat("Export complete. Outputs in:", export_dir, "\n")
cat("- Figures:", fig_dir, "\n")
cat("- Tables:", tab_dir, "\n")
cat("- Text:", text_dir, "\n")
