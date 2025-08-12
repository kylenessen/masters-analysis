# Comprehensive visualization of best model results
# Focus on lagged response model with relative change

library(tidyverse)
library(glmmTMB)
library(ggeffects)
library(patchwork)
library(viridis)
library(here)
library(broom.mixed)

# Set theme for all plots
theme_set(theme_minimal(base_size = 12) +
          theme(panel.grid.minor = element_blank(),
                strip.background = element_rect(fill = "grey95", color = NA),
                legend.position = "bottom"))

# Create output directory
results_dir <- here("results", "H1_final_visualizations")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# Load and prepare data ----
cat("Loading data and refitting best model...\n")
df_raw <- readr::read_rds(here("results", "H1_interval_30min_terms_prepared.rds"))

# Prepare data with lagged response
df <- df_raw %>%
  arrange(view_id, timestamp) %>%
  group_by(view_id) %>%
  mutate(
    # Calculate relative change
    relative_change = case_when(
      abundance_index_t_minus_1 == 0 & abundance_index_t == 0 ~ 0,
      abundance_index_t_minus_1 == 0 ~ 1,
      abundance_index_t == 0 ~ -1,
      TRUE ~ (abundance_index_t - abundance_index_t_minus_1) / 
             (abundance_index_t + abundance_index_t_minus_1)
    ),
    # Lagged relative change
    relative_change_lag1 = lag(relative_change),
    # Scale predictors
    sustained_wind_c = scale(sustained_minutes_above_2ms, center = TRUE, scale = FALSE)[,1],
    gust_wind_c = scale(gust_minutes_above_2ms, center = TRUE, scale = FALSE)[,1],
    ambient_temp_c = scale(ambient_temp, center = TRUE, scale = FALSE)[,1],
    sunlight_c = scale(sunlight_exposure_prop, center = TRUE, scale = FALSE)[,1],
    # Factors
    view_id = as.factor(view_id),
    labeler = as.factor(labeler)
  ) %>%
  ungroup() %>%
  filter(
    !is.na(relative_change_lag1),
    !is.na(sustained_wind_c),
    !is.na(gust_wind_c),
    !is.na(ambient_temp_c),
    !is.na(sunlight_c)
  )

# Store centering values for back-transformation
sustained_wind_mean <- mean(df_raw$sustained_minutes_above_2ms, na.rm = TRUE)
sustained_wind_sd <- sd(df_raw$sustained_minutes_above_2ms, na.rm = TRUE)
temp_mean <- mean(df_raw$ambient_temp, na.rm = TRUE)
temp_sd <- sd(df_raw$ambient_temp, na.rm = TRUE)

cat("Data prepared with", nrow(df), "observations\n")

# Refit best model ----
cat("Fitting lagged response model...\n")
m_best <- glmmTMB(
  relative_change ~ relative_change_lag1 +
                    sustained_wind_c + gust_wind_c + 
                    ambient_temp_c + sunlight_c +
                    (1|view_id) + (1|labeler),
  data = df
)

# ========================================
# 1. MODEL SUMMARY VISUALIZATION
# ========================================

cat("\n=== Creating model summary visualization ===\n")

# Extract coefficients with confidence intervals
coef_df <- tidy(m_best, conf.int = TRUE, effects = "fixed") %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term_clean = case_when(
      term == "relative_change_lag1" ~ "Previous Change",
      term == "sustained_wind_c" ~ "Sustained Wind",
      term == "gust_wind_c" ~ "Wind Gusts",
      term == "ambient_temp_c" ~ "Temperature",
      term == "sunlight_c" ~ "Sunlight Exposure",
      TRUE ~ term
    ),
    significant = p.value < 0.05,
    sig_label = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    )
  )

# Forest plot of coefficients
p_forest <- ggplot(coef_df, aes(x = estimate, y = reorder(term_clean, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                 height = 0.2, color = "gray30") +
  geom_point(aes(color = significant), size = 3) +
  geom_text(aes(label = sig_label, x = conf.high + 0.002), 
            hjust = 0, vjust = 0.5, size = 5) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05")) +
  labs(
    title = "Model Coefficients: Factors Affecting Butterfly Abundance Change",
    subtitle = "30-minute intervals with lagged response control",
    x = "Effect on Relative Change",
    y = NULL,
    color = "Significance"
  ) +
  theme(legend.position = "bottom")

ggsave(here(results_dir, "forest_plot_coefficients.png"), p_forest, 
       width = 10, height = 6, dpi = 300)

# ========================================
# 2. MARGINAL EFFECTS PLOTS
# ========================================

cat("Creating marginal effects plots...\n")

# Predicted effects for sustained wind
wind_pred <- ggpredict(m_best, 
                       terms = "sustained_wind_c [-10:20 by=0.5]",
                       type = "fe")

# Convert back to original scale
wind_pred$x <- wind_pred$x + sustained_wind_mean

p_wind_effect <- ggplot(wind_pred, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, fill = "blue") +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_rug(data = df, aes(x = sustained_minutes_above_2ms, y = NULL),
           alpha = 0.1, sides = "b") +
  labs(
    title = "Wind Effect on Butterfly Abundance Change",
    subtitle = "Marginal effect with 95% CI (controlling for other variables)",
    x = "Minutes with Sustained Wind > 2 m/s",
    y = "Predicted Relative Change"
  ) +
  annotate("text", x = 15, y = 0.01, 
           label = "Effect: not significant (p = 0.17)",
           color = "gray50", size = 3.5)

# Predicted effects for temperature
temp_pred <- ggpredict(m_best, 
                       terms = "ambient_temp_c [-5:5 by=0.2]",
                       type = "fe")

# Convert back to original scale
temp_pred$x <- temp_pred$x + temp_mean

p_temp_effect <- ggplot(temp_pred, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              alpha = 0.2, fill = "red") +
  geom_line(color = "red", linewidth = 1.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_rug(data = df %>% filter(!is.na(ambient_temp)), 
           aes(x = ambient_temp_c + temp_mean, y = NULL),
           alpha = 0.1, sides = "b") +
  labs(
    title = "Temperature Effect on Butterfly Abundance Change",
    subtitle = "Marginal effect with 95% CI (controlling for other variables)",
    x = "Temperature (°C)",
    y = "Predicted Relative Change"
  ) +
  annotate("text", x = temp_mean + 2, y = 0.06, 
           label = "Effect: highly significant (p < 0.001)",
           color = "red", size = 3.5)

# Combine wind and temperature effects
p_effects <- p_wind_effect / p_temp_effect +
  plot_annotation(
    title = "Environmental Effects on Monarch Butterfly Abundance Changes",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave(here(results_dir, "marginal_effects.png"), p_effects, 
       width = 10, height = 10, dpi = 300)

# ========================================
# 3. LAGGED RESPONSE VISUALIZATION
# ========================================

cat("Creating lagged response visualization...\n")

# Show autocorrelation pattern
p_autocorr <- ggplot(df, aes(x = relative_change_lag1, y = relative_change)) +
  geom_point(alpha = 0.2, color = "blue") +
  geom_density_2d(color = "darkblue", alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  geom_abline(intercept = 0, slope = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Temporal Autocorrelation in Butterfly Abundance Changes",
    subtitle = paste("Correlation =", round(cor(df$relative_change, df$relative_change_lag1), 3)),
    x = "Previous Relative Change (t-1)",
    y = "Current Relative Change (t)"
  ) +
  coord_fixed()

ggsave(here(results_dir, "autocorrelation_pattern.png"), p_autocorr, 
       width = 8, height = 8, dpi = 300)

# ========================================
# 4. INTERACTION VISUALIZATION
# ========================================

cat("Creating interaction plots...\n")

# Create categories for visualization
df_viz <- df %>%
  mutate(
    wind_category = case_when(
      sustained_minutes_above_2ms == 0 ~ "No Wind",
      sustained_minutes_above_2ms <= 10 ~ "Low Wind (1-10 min)",
      TRUE ~ "High Wind (>10 min)"
    ) %>% factor(levels = c("No Wind", "Low Wind (1-10 min)", "High Wind (>10 min)")),
    temp_category = case_when(
      ambient_temp_c < -2 ~ "Cold",
      ambient_temp_c <= 2 ~ "Average",
      TRUE ~ "Warm"
    ) %>% factor(levels = c("Cold", "Average", "Warm"))
  )

# Box plot by wind and temperature categories
p_interaction <- ggplot(df_viz, aes(x = wind_category, y = relative_change)) +
  geom_boxplot(aes(fill = temp_category), alpha = 0.7, outlier.alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  scale_fill_manual(values = c("Cold" = "blue", "Average" = "gray", "Warm" = "red"),
                    name = "Temperature") +
  labs(
    title = "Butterfly Abundance Changes by Wind and Temperature",
    subtitle = "Distribution of 30-minute relative changes",
    x = "Wind Exposure Category",
    y = "Relative Change in Abundance"
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

ggsave(here(results_dir, "interaction_wind_temperature.png"), p_interaction, 
       width = 10, height = 6, dpi = 300)

# ========================================
# 5. MODEL DIAGNOSTICS VISUALIZATION
# ========================================

cat("Creating diagnostic plots...\n")

# Extract residuals and fitted values
df_diag <- df %>%
  mutate(
    fitted = fitted(m_best),
    residuals = residuals(m_best),
    std_residuals = residuals / sd(residuals)
  )

# Diagnostic plots
p_qq <- ggplot(df_diag, aes(sample = std_residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot", x = "Theoretical Quantiles", y = "Standardized Residuals")

p_resid_fitted <- ggplot(df_diag, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = TRUE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")

p_resid_wind <- ggplot(df_diag, aes(x = sustained_minutes_above_2ms, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = TRUE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Residuals vs Wind", x = "Sustained Wind (minutes)", y = "Residuals")

p_resid_temp <- ggplot(df_diag, aes(x = ambient_temp_c + temp_mean, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = TRUE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Residuals vs Temperature", x = "Temperature (°C)", y = "Residuals")

# Combine diagnostic plots
p_diagnostics <- (p_qq | p_resid_fitted) / (p_resid_wind | p_resid_temp) +
  plot_annotation(
    title = "Model Diagnostics: Lagged Response Model",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave(here(results_dir, "model_diagnostics.png"), p_diagnostics, 
       width = 12, height = 10, dpi = 300)

# ========================================
# 6. TIME SERIES VISUALIZATION
# ========================================

cat("Creating time series visualization...\n")

# Select a representative deployment for time series
example_view <- df %>%
  group_by(view_id) %>%
  summarise(n = n()) %>%
  filter(n > 100) %>%
  slice(1) %>%
  pull(view_id)

df_ts <- df %>%
  filter(view_id == example_view) %>%
  arrange(timestamp) %>%
  mutate(
    time_index = row_number()
  )

# Get fitted values for this subset
fitted_subset <- fitted(m_best)[df$view_id == example_view]
df_ts$predicted <- fitted_subset

# Time series plot
p_timeseries <- ggplot(df_ts, aes(x = time_index)) +
  # Wind exposure as background
  geom_area(aes(y = sustained_minutes_above_2ms / max(sustained_minutes_above_2ms) * 0.5),
            fill = "lightblue", alpha = 0.3) +
  # Actual changes
  geom_line(aes(y = relative_change, color = "Observed"), linewidth = 1) +
  # Model predictions
  geom_line(aes(y = predicted, color = "Predicted"), linewidth = 1, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
  scale_color_manual(values = c("Observed" = "black", "Predicted" = "red"),
                     name = "") +
  labs(
    title = paste("Time Series Example: View", example_view),
    subtitle = "Observed vs predicted relative changes (blue shading = wind exposure)",
    x = "Time Index (30-minute intervals)",
    y = "Relative Change"
  ) +
  theme(legend.position = "top")

ggsave(here(results_dir, "timeseries_example.png"), p_timeseries, 
       width = 12, height = 6, dpi = 300)

# ========================================
# 7. SUMMARY STATISTICS TABLE
# ========================================

cat("Creating summary statistics...\n")

# Model summary statistics
model_summary <- data.frame(
  Metric = c("Observations", "Views", "Labelers", 
             "Mean Relative Change", "SD Relative Change",
             "Autocorrelation", "R² (marginal)", "R² (conditional)",
             "AIC", "Wind Effect (p-value)", "Temp Effect (p-value)"),
  Value = c(
    nrow(df),
    n_distinct(df$view_id),
    n_distinct(df$labeler),
    round(mean(df$relative_change), 3),
    round(sd(df$relative_change), 3),
    round(cor(df$relative_change, df$relative_change_lag1), 3),
    round(performance::r2(m_best)$R2_marginal, 3),
    round(performance::r2(m_best)$R2_conditional, 3),
    round(AIC(m_best), 1),
    paste0(round(coef_df$estimate[coef_df$term_clean == "Sustained Wind"], 4), 
           " (", round(coef_df$p.value[coef_df$term_clean == "Sustained Wind"], 3), ")"),
    paste0(round(coef_df$estimate[coef_df$term_clean == "Temperature"], 4),
           " (", round(coef_df$p.value[coef_df$term_clean == "Temperature"], 4), ")")
  )
)

write_csv(model_summary, here(results_dir, "model_summary_statistics.csv"))

# ========================================
# 8. PUBLICATION-READY COMBINED FIGURE
# ========================================

cat("Creating publication figure...\n")

# Simplified versions for publication
p_pub_effects <- p_wind_effect + p_temp_effect +
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Environmental Effects on Monarch Butterfly Abundance",
    caption = "Error bands show 95% confidence intervals. N = 1,642 observations.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.caption = element_text(size = 9, color = "gray50")
    )
  )

ggsave(here(results_dir, "Figure_1_environmental_effects.png"), p_pub_effects, 
       width = 12, height = 6, dpi = 300)

# Combined diagnostic figure
p_pub_diagnostic <- p_autocorr + p_interaction +
  plot_layout(ncol = 2, widths = c(1, 1.2)) +
  plot_annotation(
    title = "Temporal Patterns and Environmental Interactions",
    theme = theme(plot.title = element_text(size = 14, face = "bold"))
  )

ggsave(here(results_dir, "Figure_2_patterns.png"), p_pub_diagnostic, 
       width = 14, height = 6, dpi = 300)

# ========================================
# FINAL SUMMARY
# ========================================

cat("\n=== VISUALIZATION COMPLETE ===\n")
cat("Created the following figures:\n")
cat("1. forest_plot_coefficients.png - Model coefficient estimates\n")
cat("2. marginal_effects.png - Wind and temperature marginal effects\n")
cat("3. autocorrelation_pattern.png - Temporal autocorrelation\n")
cat("4. interaction_wind_temperature.png - Wind-temperature interaction\n")
cat("5. model_diagnostics.png - Residual diagnostics\n")
cat("6. timeseries_example.png - Example time series\n")
cat("7. Figure_1_environmental_effects.png - Publication figure 1\n")
cat("8. Figure_2_patterns.png - Publication figure 2\n")
cat("9. model_summary_statistics.csv - Summary statistics table\n")
cat("\nAll figures saved to:", results_dir, "\n")