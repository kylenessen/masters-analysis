#!/usr/bin/env Rscript
# Fact-check: Sunset window GAMM analysis
library(dplyr)
library(readr)
library(mgcv)
library(nlme)
library(here)

cat("======================================================================\n")
cat("SUNSET WINDOW FACT-CHECK\n")
cat("======================================================================\n\n")

# Load and prep data (matching sunset_window_gam_analysis.qmd)
daily_data <- read_csv(here("data", "monarch_daily_lag_analysis_sunset_window.csv"),
                       show_col_types = FALSE)

# Create sqrt response (same as qmd line 64-70)
daily_data <- daily_data %>%
  mutate(
    butterfly_diff_sqrt = ifelse(butterfly_diff >= 0,
                                 sqrt(butterfly_diff),
                                 -sqrt(-butterfly_diff))
  )

cat("Raw data:", nrow(daily_data), "rows\n")

# Filter and prep (same as qmd lines 443-461)
model_data <- daily_data %>%
  filter(metrics_complete >= 0.95) %>%
  arrange(deployment_id, observation_order_t) %>%
  mutate(
    deployment_id = factor(deployment_id),
    across(c(max_butterflies_t_1, lag_duration_hours,
             temp_min, temp_max, temp_at_max_count_t_1,
             wind_max_gust, sum_butterflies_direct_sun), as.numeric)
  ) %>%
  filter(
    !is.na(butterfly_diff_sqrt),
    !is.na(max_butterflies_t_1),
    !is.na(lag_duration_hours)
  )

cat("Filtered model data:", nrow(model_data), "observations\n")
cat("  Manuscript says: 96\n\n")

# Descriptive stats (from filtered data)
cat("--- Descriptive Stats (filtered, n=", nrow(model_data), ") ---\n")
cat(sprintf("  Daily max BAI range: %.0f - %.0f\n",
            min(model_data$max_butterflies_t_1), max(model_data$max_butterflies_t_1)))
cat(sprintf("  Daily max BAI mean ± SD: %.1f ± %.1f\n",
            mean(model_data$max_butterflies_t_1), sd(model_data$max_butterflies_t_1)))
cat(sprintf("  Change range: %.0f to %.0f\n",
            min(model_data$butterfly_diff), max(model_data$butterfly_diff)))
cat(sprintf("  Change mean ± SD: %.1f ± %.1f\n",
            mean(model_data$butterfly_diff), sd(model_data$butterfly_diff)))
cat(sprintf("  Wind max gust range: %.1f - %.1f\n",
            min(model_data$wind_max_gust), max(model_data$wind_max_gust)))
cat(sprintf("  Wind max gust mean ± SD: %.1f ± %.1f\n",
            mean(model_data$wind_max_gust), sd(model_data$wind_max_gust)))
cat(sprintf("  Sun exposure range: %.0f - %.0f\n",
            min(model_data$sum_butterflies_direct_sun), max(model_data$sum_butterflies_direct_sun)))
cat(sprintf("  Sun exposure mean ± SD: %.1f ± %.1f\n",
            mean(model_data$sum_butterflies_direct_sun), sd(model_data$sum_butterflies_direct_sun)))
cat(sprintf("  Lag duration mean: %.1f hours\n",
            mean(model_data$lag_duration_hours)))
cat("\nManuscript says:\n")
cat("  BAI: 0-770, mean=134.7±138.1\n")
cat("  Change: -376 to +464, mean=-10.5±111.6\n")
cat("  Wind: 2.0-12.8, mean=4.5±1.8\n")
cat("  Sun: 0-1122, mean=139.8±206.9\n")
cat("  Lag duration mean: 29.6 hours\n\n")

# Linear regression (sunset)
cat("--- Sunset Linear Regression ---\n")
lm_data <- daily_data %>%
  filter(!is.na(wind_max_gust), !is.na(butterfly_diff))
lm_sunset <- lm(butterfly_diff ~ wind_max_gust, data = lm_data)
lm_s <- summary(lm_sunset)
cat(sprintf("  n = %d\n", nrow(lm_data)))
cat(sprintf("  β = %.2f\n", coef(lm_sunset)["wind_max_gust"]))
cat(sprintf("  SE = %.2f\n", lm_s$coefficients["wind_max_gust", "Std. Error"]))
cat(sprintf("  p = %.3f\n", lm_s$coefficients["wind_max_gust", "Pr(>|t|)"]))
cat(sprintf("  r = %.3f\n", cor(lm_data$wind_max_gust, lm_data$butterfly_diff)))
cat(sprintf("  R² = %.3f\n", lm_s$r.squared))
cat("\nManuscript: β=5.34, SE=4.63, p=0.252, n=101, r=0.115, R²=0.013\n\n")

# Model structures
random_structure <- list(deployment_id = ~1)
ar1_cor <- corAR1(form = ~ observation_order_t | deployment_id)
k_baseline <- 5
k_lag <- 5

fit_safe <- function(formula_str, name) {
  cat(sprintf("  Fitting %s...", name))
  tryCatch({
    m <- gamm(as.formula(formula_str),
              data = model_data,
              random = random_structure,
              correlation = ar1_cor,
              method = "REML")
    cat(sprintf(" AIC = %.2f\n", AIC(m$lme)))
    m
  }, error = function(e) {
    cat(sprintf(" FAILED: %s\n", e$message))
    NULL
  })
}

# Fit the top models from manuscript Table 3.6
cat("--- Fitting Sunset GAMM Models ---\n")

# M32: wind × sun interaction (linear baseline) — manuscript best
M32 <- fit_safe(
  "butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours + ti(wind_max_gust, sum_butterflies_direct_sun)",
  "M32")

# M52: wind + sun + interaction
M52 <- fit_safe(
  "butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours + s(wind_max_gust) + s(sum_butterflies_direct_sun) + ti(wind_max_gust, sum_butterflies_direct_sun)",
  "M52")

# M20: temp_min × sun interaction
M20 <- fit_safe(
  "butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours + ti(temp_min, sum_butterflies_direct_sun)",
  "M20")

# M2: null (linear baseline)
M2 <- fit_safe(
  "butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours",
  "M2")

# M10: wind single predictor (linear)
M10 <- fit_safe(
  "butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours + s(wind_max_gust)",
  "M10")

# Model comparison
cat("\n--- Model Comparison ---\n")
models_list <- list(M32 = M32, M52 = M52, M20 = M20, M2 = M2, M10 = M10)
models_list <- models_list[!sapply(models_list, is.null)]

if (length(models_list) > 0) {
  n_obs <- nrow(model_data)
  aic_df <- data.frame(
    Model = names(models_list),
    AIC = sapply(models_list, function(m) AIC(m$lme)),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      # Use AICc for small sample
      k = sapply(models_list, function(m) attr(logLik(m$lme), "df")),
      AICc = AIC + (2 * k * (k + 1)) / (n_obs - k - 1)
    ) %>%
    arrange(AICc) %>%
    mutate(
      Delta_AICc = AICc - min(AICc),
      Weight = exp(-0.5 * Delta_AICc) / sum(exp(-0.5 * Delta_AICc))
    )
  print(aic_df)
}

cat("\nManuscript Table 3.6:\n")
cat("  M32: AICc=648.85, ΔAICc=0.00, Weight=0.85\n")
cat("  M52: AICc=654.45, ΔAICc=5.60, Weight=0.05\n")
cat("  M20: AICc=656.77, ΔAICc=7.92, Weight=0.02\n\n")

# Best model (M32) summary
if (!is.null(M32)) {
  cat("--- Best Model (M32) Summary ---\n")
  s <- summary(M32$gam)

  # Parametric coefficients
  cat("\nParametric coefficients:\n")
  print(s$p.table)

  cat("\nSmooth terms:\n")
  print(s$s.table)

  cat(sprintf("\nAdj. R² = %.3f\n", s$r.sq))
  cat(sprintf("Scale est. = %.2f\n", s$scale))
  cat(sprintf("n = %d\n", s$n))

  cat("\nManuscript Table A:\n")
  cat("  Previous day max count: edf=1.00, F=24.88, p<0.001\n")
  cat("  Window duration: edf=1.00, F=1.14, p=0.289\n")
  cat("  Wind×Sunlight: edf=6.68, F=4.10, p<0.001\n")
  cat("  Adj. R²=0.397, Scale est.=41.06, n=96\n")
}

cat("\n======================================================================\n")
cat("SUNSET FACT-CHECK COMPLETE\n")
cat("======================================================================\n")
