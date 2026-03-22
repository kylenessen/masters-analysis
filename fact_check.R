#!/usr/bin/env Rscript
# ============================================================================
# MANUSCRIPT FACT-CHECK SCRIPT
# Reproduces every number cited in the manuscript and compares to stated values
# ============================================================================

library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(ggplot2)
library(mgcv)
library(nlme)
library(here)

sep_line <- paste(rep("=", 70), collapse="")
cat(sep_line, "\n")
cat("MANUSCRIPT FACT-CHECK\n")
cat(sep_line, "\n\n")

# ============================================================================
# 1. LOAD DATA
# ============================================================================
monarch_data <- read_csv(here("data", "monarch_analysis_lag30min.csv"),
                         show_col_types = FALSE)

cat("Loaded", nrow(monarch_data), "paired observations\n\n")

# ============================================================================
# 2. DESCRIPTIVE STATISTICS (from all unique observations)
# ============================================================================
cat(sep_line, "\n")
cat("SECTION: DESCRIPTIVE STATISTICS\n")
cat(sep_line, "\n\n")

# Build unique observations from both _t and _t_lag columns
obs_t <- monarch_data %>%
  select(image_filename = image_filename_t,
         deployment_id, deployment_day,
         butterflies = total_butterflies_t,
         sun = butterflies_direct_sun_t,
         temp = temperature_t,
         ts = timestamp_t)

obs_lag <- monarch_data %>%
  select(image_filename = image_filename_t_lag,
         deployment_id, deployment_day,
         butterflies = total_butterflies_t_lag,
         sun = butterflies_direct_sun_t_lag,
         temp = temperature_t_lag,
         ts = timestamp_t_lag)

all_obs <- bind_rows(obs_t, obs_lag) %>%
  distinct(image_filename, .keep_all = TRUE)

cat("Total unique observations:", nrow(all_obs), "\n")
cat("  Manuscript says: 2,028\n\n")

# Date range
dates <- as.Date(substr(all_obs$ts, 1, 10))
cat("Date range:", as.character(min(dates)), "to", as.character(max(dates)), "\n")
cat("Calendar span (inclusive):", as.numeric(max(dates) - min(dates)) + 1, "days\n")
cat("Unique dates with observations:", n_distinct(dates), "\n")
cat("  Manuscript says: 80-day monitoring period\n\n")

# Deployment-days
cat("Unique deployment-days:", n_distinct(all_obs$deployment_day), "\n")
cat("  Manuscript says: 115\n\n")

# Observation hours
cat("Observation hours:", nrow(all_obs) * 0.5, "\n")
cat("  Manuscript says: 1,014\n\n")

# Wind (from paired intervals)
cat("--- Wind (from 1,894 paired intervals) ---\n")
wind <- monarch_data$max_gust
cat(sprintf("  Range: %.1f - %.1f m/s\n", min(wind), max(wind)))
cat(sprintf("  Mean ± SD: %.1f ± %.1f\n", mean(wind), sd(wind)))
cat(sprintf("  Median: %.1f\n", median(wind)))
cat(sprintf("  IQR: %.1f - %.1f\n", quantile(wind, 0.25), quantile(wind, 0.75)))
cat("  Manuscript: 0.0-12.4, 2.2±1.4, median 2.2, IQR 1.3-3.0\n\n")

# Temperature (from all unique obs)
cat("--- Temperature (from all unique obs) ---\n")
temp <- all_obs$temp[!is.na(all_obs$temp)]
cat(sprintf("  Range: %.1f - %.1f °C\n", min(temp), max(temp)))
cat(sprintf("  Mean ± SD: %.1f ± %.1f\n", mean(temp), sd(temp)))
cat(sprintf("  Median: %.1f\n", median(temp)))
cat(sprintf("  IQR: %.1f - %.1f\n", quantile(temp, 0.25), quantile(temp, 0.75)))
cat("  Manuscript: 3.0-30.0, 14.5±3.9, median 14.0, IQR 12.0-17.0\n\n")

# Solar exposure
cat("--- Solar exposure (from all unique obs) ---\n")
sun_present <- all_obs$sun[all_obs$sun > 0]
cat(sprintf("  Obs with direct sun: %d (%.1f%%)\n",
            length(sun_present), 100 * length(sun_present) / nrow(all_obs)))
cat(sprintf("  Mean when present: %.1f\n", mean(sun_present)))
cat(sprintf("  Range when present: %.0f - %.0f\n", min(sun_present), max(sun_present)))
cat("  Manuscript: 612 (30.2%), mean 16.8, range 1-295\n\n")

# Butterfly abundance
cat("--- Butterfly abundance (from all unique obs) ---\n")
bfly <- all_obs$butterflies
cat(sprintf("  Range: %.0f - %.0f\n", min(bfly), max(bfly)))
cat(sprintf("  Mean ± SD: %.1f ± %.1f\n", mean(bfly), sd(bfly)))
cat(sprintf("  Median: %.0f\n", median(bfly)))
cat(sprintf("  IQR: %.0f - %.0f\n", quantile(bfly, 0.25), quantile(bfly, 0.75)))
zeros <- sum(bfly == 0)
cat(sprintf("  Zero-count: %d (%.1f%%)\n", zeros, 100 * zeros / nrow(all_obs)))
cat("  Manuscript: 0-950, 80.9±101.4, median 36, IQR 8-118, zeros 71 (3.5%)\n\n")

# Per-deployment stats
cat("--- Per-deployment stats (from all unique obs) ---\n")
dep_stats <- all_obs %>%
  group_by(deployment_id) %>%
  summarise(mean_bfly = mean(butterflies), max_bfly = max(butterflies), n = n())
cat("  Mean per deployment:\n")
for (i in seq_len(nrow(dep_stats))) {
  cat(sprintf("    %s: mean=%.1f, max=%.0f (n=%d)\n",
              dep_stats$deployment_id[i], dep_stats$mean_bfly[i],
              dep_stats$max_bfly[i], dep_stats$n[i]))
}
cat(sprintf("  Mean of max BAI across deployments: %.1f\n", mean(dep_stats$max_bfly)))
cat(sprintf("  Deployments with max > 100: %d\n", sum(dep_stats$max_bfly > 100)))
cat("  Manuscript: SC9=0.5, UDMH2=382.5, mean max=336.7, 8 with >100\n\n")

# Peak hour
cat("--- Peak hour (from all unique obs) ---\n")
hours <- all_obs %>%
  mutate(hour = as.integer(substr(ts, 12, 13))) %>%
  count(hour) %>%
  arrange(desc(n))
cat("  Top 3 hours:\n")
for (i in 1:3) {
  cat(sprintf("    %02d:00 — %d observations\n", hours$hour[i], hours$n[i]))
}
cat("  Manuscript says: 14:00 (202 observations)\n\n")

# ============================================================================
# 3. LINEAR REGRESSION (wind vs CiBAI)
# ============================================================================
cat(sep_line, "\n")
cat("SECTION: LINEAR REGRESSION\n")
cat(sep_line, "\n\n")

# 30-minute linear regression
cat("--- 30-minute: max_gust vs butterfly_difference ---\n")
lm_30 <- lm(butterfly_difference ~ max_gust, data = monarch_data)
lm_30_summary <- summary(lm_30)
cat(sprintf("  β = %.2f\n", coef(lm_30)["max_gust"]))
cat(sprintf("  SE = %.2f\n", lm_30_summary$coefficients["max_gust", "Std. Error"]))
cat(sprintf("  p = %.3f\n", lm_30_summary$coefficients["max_gust", "Pr(>|t|)"]))
cat(sprintf("  r = %.3f\n", cor(monarch_data$max_gust, monarch_data$butterfly_difference)))
cat(sprintf("  R² = %.3f\n", lm_30_summary$r.squared))
cat(sprintf("  n = %d\n", nrow(monarch_data)))
cat("  Manuscript: β=1.19, SE=0.66, p=0.073, r=0.041, R²=0.002, n=1894\n\n")

# ============================================================================
# 4. 30-MINUTE GAMM ANALYSIS
# ============================================================================
cat(sep_line, "\n")
cat("SECTION: 30-MINUTE GAMM ANALYSIS\n")
cat(sep_line, "\n\n")

# Data prep (same as qmd)
model_data <- monarch_data %>%
  filter(
    !is.na(butterfly_difference_cbrt),
    !is.na(total_butterflies_t_lag),
    !is.na(max_gust),
    !is.na(temperature_avg),
    !is.na(butterflies_direct_sun_t_lag),
    !is.na(deployment_id),
    !is.na(deployment_day),
    !is.na(Observer),
    !is.na(observation_order_within_day_t)
  )

cat("Clean dataset:", nrow(model_data), "observations\n\n")

# Define structures
random_structure <- list(deployment_id = ~1, Observer = ~1, deployment_day = ~1)
correlation_structure <- corAR1(form = ~ observation_order_within_day_t | deployment_day)

# Fit key models only (top 5 from manuscript + a few others for verification)
key_models <- list(
  "M50" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(time_within_day_t) + ti(max_gust, butterflies_direct_sun_t_lag)",
  "M23" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)",
  "M22" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + temperature_avg + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)",
  "M24" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(max_gust) + s(temperature_avg) + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)",
  "M52" = "butterfly_difference_cbrt ~ s(temperature_avg) + s(time_within_day_t) + ti(max_gust, butterflies_direct_sun_t_lag)",
  "M49" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + ti(max_gust, butterflies_direct_sun_t_lag)",
  "M51" = "butterfly_difference_cbrt ~ ti(max_gust, butterflies_direct_sun_t_lag)",
  "M25" = "butterfly_difference_cbrt ~ 1"
)

fit_model <- function(formula_str, data) {
  tryCatch({
    gamm(as.formula(formula_str),
         data = data,
         random = random_structure,
         correlation = correlation_structure,
         method = "REML")
  }, error = function(e) {
    message("Failed: ", formula_str, "\n  ", e$message)
    NULL
  })
}

cat("Fitting key models...\n")
fitted <- list()
for (name in names(key_models)) {
  cat(sprintf("  Fitting %s...", name))
  fitted[[name]] <- fit_model(key_models[[name]], model_data)
  if (!is.null(fitted[[name]])) {
    cat(sprintf(" AIC = %.2f\n", AIC(fitted[[name]]$lme)))
  } else {
    cat(" FAILED\n")
  }
}

# Model comparison
cat("\n--- Model Selection (top models by AIC) ---\n")
aic_df <- data.frame(
  Model = character(),
  AIC = numeric(),
  stringsAsFactors = FALSE
)
for (name in names(fitted)) {
  if (!is.null(fitted[[name]])) {
    aic_df <- rbind(aic_df, data.frame(Model = name, AIC = AIC(fitted[[name]]$lme)))
  }
}
aic_df <- aic_df %>%
  arrange(AIC) %>%
  mutate(
    Delta_AIC = AIC - min(AIC),
    Weight = exp(-0.5 * Delta_AIC) / sum(exp(-0.5 * Delta_AIC))
  )
print(aic_df)
cat("\nManuscript Table 3.1:\n")
cat("  M50: AIC=8074.03, ΔAIC=0.00, Weight=0.86\n")
cat("  M23: AIC=8077.86, ΔAIC=3.83, Weight=0.13\n")
cat("  M22: AIC=8082.90, ΔAIC=8.87, Weight=0.01\n")
cat("  M24: AIC=8084.05, ΔAIC=10.02, Weight=0.01\n")
cat("  M52: AIC=8092.72, ΔAIC=18.69, Weight=0.00\n\n")

# Best model (M50) summary
cat("--- Best Model (M50) Summary ---\n")
if (!is.null(fitted[["M50"]])) {
  m50_summary <- summary(fitted[["M50"]]$gam)
  cat("\nSmooth terms:\n")
  print(m50_summary$s.table)
  cat(sprintf("\nAdj. R² = %.3f\n", m50_summary$r.sq))
  cat(sprintf("Scale est. = %.2f\n", m50_summary$scale))
  cat(sprintf("n = %d\n", m50_summary$n))
  cat("\nManuscript Table 3.2:\n")
  cat("  Previous butterfly count: edf=2.41, F=12.50, p<0.001\n")
  cat("  Average temperature: edf=3.68, F=3.19, p=0.057\n")
  cat("  Time since sunrise: edf=4.87, F=9.85, p<0.001\n")
  cat("  Wind×Sunlight: edf=7.35, F=4.67, p<0.001\n")
  cat("  Adj. R²=0.064, Scale est.=4.03, n=1894\n")
}

# ============================================================================
# 5. THRESHOLD ANALYSIS (30-minute)
# ============================================================================
cat("\n")
cat(sep_line, "\n")
cat("SECTION: THRESHOLD ANALYSIS (30-min)\n")
cat(sep_line, "\n\n")

threshold_models <- list(
  "T50" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(time_within_day_t) + ti(minutes_above_threshold, butterflies_direct_sun_t_lag)",
  "T23" = "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)"
)

cat("Fitting threshold models...\n")
fitted_threshold <- list()
for (name in names(threshold_models)) {
  cat(sprintf("  Fitting %s...", name))
  fitted_threshold[[name]] <- fit_model(threshold_models[[name]], model_data)
  if (!is.null(fitted_threshold[[name]])) {
    cat(sprintf(" AIC = %.2f\n", AIC(fitted_threshold[[name]]$lme)))
  } else {
    cat(" FAILED\n")
  }
}

cat("\nManuscript Table 3.4:\n")
cat("  T50: AIC=8077.23, Weight=0.55\n")
cat("  T23: AIC=8077.86, Weight=0.40\n\n")

# T50 smooth terms
if (!is.null(fitted_threshold[["T50"]])) {
  cat("--- T50 Summary ---\n")
  t50_summary <- summary(fitted_threshold[["T50"]]$gam)
  print(t50_summary$s.table)
  cat(sprintf("\nAdj. R² = %.3f\n", t50_summary$r.sq))
}

# ============================================================================
# 6. POWER ANALYSIS
# ============================================================================
cat("\n")
cat(sep_line, "\n")
cat("SECTION: POWER ANALYSIS\n")
cat("(Skipping simulation — checking exported results instead)\n")
cat(sep_line, "\n\n")

power_file <- here("thesis_exports", "tables", "power_analysis_table.csv")
if (file.exists(power_file)) {
  power_results <- read_csv(power_file, show_col_types = FALSE)
  cat("Power analysis from exported table:\n")
  print(power_results)
  cat("\nManuscript Table 3.5:\n")
  cat("  0.05 SD: 16.5%\n")
  cat("  0.10 SD: 56%\n")
  cat("  0.15 SD: 87.5%\n")
  cat("  0.20 SD: 98.5%\n")
} else {
  cat("Power analysis table not found at:", power_file, "\n")
  cat("Will need to run analysis/monarch_gam_power_analysis.R\n")
}

# ============================================================================
# 7. SUNSET WINDOW ANALYSIS
# ============================================================================
cat("\n")
cat(sep_line, "\n")
cat("SECTION: SUNSET WINDOW ANALYSIS\n")
cat(sep_line, "\n\n")

sunset_file <- here("data", "monarch_daily_lag_analysis_sunset_window.csv")
if (file.exists(sunset_file)) {
  sunset_data <- read_csv(sunset_file, show_col_types = FALSE)
  cat("Sunset window data loaded:", nrow(sunset_data), "rows\n")
  cat("  Manuscript says: 96 pairs (for GAMM), 101 (for linear regression)\n\n")

  # Descriptive stats for sunset window
  cat("--- Sunset Descriptive Stats ---\n")
  cat(sprintf("  Daily max BAI range: %.0f - %.0f\n",
              min(sunset_data$max_butterflies_t_1, na.rm=TRUE),
              max(sunset_data$max_butterflies_t_1, na.rm=TRUE)))
  cat(sprintf("  Daily max BAI mean ± SD: %.1f ± %.1f\n",
              mean(sunset_data$max_butterflies_t_1, na.rm=TRUE),
              sd(sunset_data$max_butterflies_t_1, na.rm=TRUE)))
  cat("  Manuscript: 0-770, mean=134.7±138.1\n\n")

  # Check for wind columns
  wind_col <- intersect(c("wind_max_gust", "wind_max_gust_t_1"), names(sunset_data))
  if (length(wind_col) > 0) {
    wc <- wind_col[1]
    w <- sunset_data[[wc]][!is.na(sunset_data[[wc]])]
    cat(sprintf("  Wind max gust range: %.1f - %.1f m/s\n", min(w), max(w)))
    cat(sprintf("  Wind max gust mean ± SD: %.1f ± %.1f\n", mean(w), sd(w)))
    cat("  Manuscript: 2.0-12.8, mean=4.5±1.8\n\n")
  }

  # Linear regression (sunset)
  # Need to find the right column names
  cat("  Column names:\n")
  cat("  ", paste(names(sunset_data)[1:20], collapse=", "), "\n\n")

} else {
  cat("Sunset window data file not found\n")
  cat("Looking for alternative files...\n")
  list.files(here("data"), pattern = "sunset|daily", full.names = FALSE) %>% print()
}

# ============================================================================
# 8. BASIS DIMENSION CHECKS (from M50)
# ============================================================================
cat("\n")
cat(sep_line, "\n")
cat("SECTION: BASIS DIMENSION CHECKS\n")
cat(sep_line, "\n\n")

if (!is.null(fitted[["M50"]])) {
  cat("gam.check output for M50:\n")
  gam_check <- capture.output(gam.check(fitted[["M50"]]$gam, type = "deviance"))
  # Print just the k-index table
  k_start <- grep("k'", gam_check)
  if (length(k_start) > 0) {
    cat(paste(gam_check[k_start:length(gam_check)], collapse = "\n"))
  }
  cat("\n\nManuscript Table 3.3:\n")
  cat("  s(total_butterflies_t_lag): k'=9, edf=2.41, k-index=1.00, p=0.735\n")
  cat("  s(temperature_avg): k'=9, edf=3.67, k-index=1.02, p=0.895\n")
  cat("  s(time_within_day_t): k'=9, edf=4.87, k-index=0.96, p=0.065\n")
  cat("  ti(max_gust,butterflies_direct_sun_t_lag): k'=16, edf=7.35, k-index=0.99, p=0.490\n")
}

cat("\n\n")
cat(sep_line, "\n")
cat("FACT-CHECK COMPLETE\n")
cat("Compare outputs above to manuscript values\n")
cat(sep_line, "\n")
