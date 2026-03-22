#!/usr/bin/env Rscript
# Fact-check: 24-hour window GAMM analysis
library(dplyr)
library(readr)
library(mgcv)
library(nlme)
library(here)

cat("======================================================================\n")
cat("24-HOUR WINDOW FACT-CHECK\n")
cat("======================================================================\n\n")

daily_data <- read_csv(here("data", "monarch_daily_lag_analysis_24hr_window.csv"),
                       show_col_types = FALSE)

daily_data <- daily_data %>%
  mutate(
    butterfly_diff_sqrt = ifelse(butterfly_diff >= 0,
                                 sqrt(butterfly_diff),
                                 -sqrt(-butterfly_diff))
  )

cat("Raw data:", nrow(daily_data), "rows\n")

# Filter (same as qmd)
model_data <- daily_data %>%
  filter(metrics_complete >= 0.95) %>%
  arrange(deployment_id, observation_order_t) %>%
  mutate(
    deployment_id = factor(deployment_id),
    across(c(max_butterflies_t_1,
             temp_min, temp_max, temp_at_max_count_t_1,
             wind_max_gust, sum_butterflies_direct_sun), as.numeric)
  ) %>%
  filter(
    !is.na(butterfly_diff_sqrt),
    !is.na(max_butterflies_t_1)
  )

cat("Filtered model data:", nrow(model_data), "observations\n")
cat("  Manuscript says: 94 pairs\n\n")

# Model structures (no lag_duration since fixed at 24hr)
random_structure <- list(deployment_id = ~1)
ar1_cor <- corAR1(form = ~ observation_order_t | deployment_id)
k_baseline <- 5

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

cat("--- Fitting 24-hour GAMM Models ---\n")

# M31: wind × sun interaction (smooth baseline) — manuscript best
M31 <- fit_safe(
  "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + ti(wind_max_gust, sum_butterflies_direct_sun)",
  "M31")

# M32: wind × sun interaction (linear baseline)
M32 <- fit_safe(
  "butterfly_diff_sqrt ~ max_butterflies_t_1 + ti(wind_max_gust, sum_butterflies_direct_sun)",
  "M32")

# M23: temp_max × wind (smooth baseline)
M23 <- fit_safe(
  "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + ti(temp_max, wind_max_gust)",
  "M23")

# M29: temp × sun (smooth baseline)
M29 <- fit_safe(
  "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + ti(temp_at_max_count_t_1, sum_butterflies_direct_sun)",
  "M29")

# M19: min temp × sun (smooth baseline)
M19 <- fit_safe(
  "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + ti(temp_min, sum_butterflies_direct_sun)",
  "M19")

# M51: wind + sun + interaction (smooth baseline)
M51 <- fit_safe(
  "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + s(wind_max_gust) + s(sum_butterflies_direct_sun) + ti(wind_max_gust, sum_butterflies_direct_sun)",
  "M51")

# M1: null (smooth baseline)
M1 <- fit_safe(
  "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline)",
  "M1")

# Model comparison with AICc
cat("\n--- Model Comparison ---\n")
models_list <- list(M31=M31, M32=M32, M23=M23, M29=M29, M19=M19, M51=M51, M1=M1)
models_list <- models_list[!sapply(models_list, is.null)]

if (length(models_list) > 0) {
  n_obs <- nrow(model_data)
  aic_df <- data.frame(
    Model = names(models_list),
    AIC = sapply(models_list, function(m) AIC(m$lme)),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
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

cat("\nManuscript Table 3.8:\n")
cat("  M31: AICc=636.3, Weight=0.507, adj R²=0.235\n")
cat("  M23: AICc=639.9, ΔAICc=3.6, Weight=0.084\n")
cat("  M29: AICc=640.8, ΔAICc=4.4, Weight=0.056\n")
cat("  M19: AICc=641.1, ΔAICc=4.8, Weight=0.047\n")
cat("  M51: AICc=641.7, ΔAICc=5.4, Weight=0.034\n\n")

# Best model summary
if (!is.null(M31)) {
  cat("--- Best Model (M31) Summary ---\n")
  s <- summary(M31$gam)
  cat("\nParametric coefficients:\n")
  print(s$p.table)
  cat("\nSmooth terms:\n")
  print(s$s.table)
  cat(sprintf("\nAdj. R² = %.3f\n", s$r.sq))
  cat(sprintf("Scale est. = %.2f\n", s$scale))
  cat(sprintf("n = %d\n", s$n))
  cat("\nManuscript: adj R²=0.235\n")
}

cat("\n======================================================================\n")
cat("24-HOUR FACT-CHECK COMPLETE\n")
cat("======================================================================\n")
