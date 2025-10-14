#!/usr/bin/env Rscript

# Script to analyze the rarity of butterflies in direct sunlight
# Focus on how rare it is to have substantial numbers in sun

suppressPackageStartupMessages({
  library(tidyverse)
  library(knitr)
  library(here)
})

# Create output directory
output_dir <- here("analysis", "reports", "butterfly_distributions")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

cat("========================================\n")
cat("RARITY OF BUTTERFLIES IN DIRECT SUNLIGHT\n")
cat("========================================\n\n")

# ----------------------------------------------------------------------------
# 30-minute analysis
# ----------------------------------------------------------------------------
cat("30-MINUTE INTERVAL ANALYSIS\n")
cat("----------------------------\n\n")

data_30min_file <- here("data", "monarch_analysis_lag30min.csv")
if (file.exists(data_30min_file)) {
  dat_30min <- readr::read_csv(data_30min_file, show_col_types = FALSE)

  # Filter to complete cases
  complete_30min <- dat_30min %>%
    filter(!is.na(butterflies_direct_sun_t_lag))

  n_total <- nrow(complete_30min)

  # Calculate different thresholds for butterflies in sun
  sun_categories <- complete_30min %>%
    summarise(
      total_obs = n(),
      # Zero
      n_zero = sum(butterflies_direct_sun_t_lag == 0),
      pct_zero = sum(butterflies_direct_sun_t_lag == 0) / n() * 100,
      # Exactly 1
      n_exactly_1 = sum(butterflies_direct_sun_t_lag == 1),
      pct_exactly_1 = sum(butterflies_direct_sun_t_lag == 1) / n() * 100,
      # 1-5
      n_1_to_5 = sum(butterflies_direct_sun_t_lag >= 1 & butterflies_direct_sun_t_lag <= 5),
      pct_1_to_5 = sum(butterflies_direct_sun_t_lag >= 1 & butterflies_direct_sun_t_lag <= 5) / n() * 100,
      # 1-10
      n_1_to_10 = sum(butterflies_direct_sun_t_lag >= 1 & butterflies_direct_sun_t_lag <= 10),
      pct_1_to_10 = sum(butterflies_direct_sun_t_lag >= 1 & butterflies_direct_sun_t_lag <= 10) / n() * 100,
      # Less than 10
      n_less_than_10 = sum(butterflies_direct_sun_t_lag < 10),
      pct_less_than_10 = sum(butterflies_direct_sun_t_lag < 10) / n() * 100,
      # Exactly 10
      n_exactly_10 = sum(butterflies_direct_sun_t_lag == 10),
      pct_exactly_10 = sum(butterflies_direct_sun_t_lag == 10) / n() * 100,
      # More than 10
      n_more_than_10 = sum(butterflies_direct_sun_t_lag > 10),
      pct_more_than_10 = sum(butterflies_direct_sun_t_lag > 10) / n() * 100,
      # More than 20
      n_more_than_20 = sum(butterflies_direct_sun_t_lag > 20),
      pct_more_than_20 = sum(butterflies_direct_sun_t_lag > 20) / n() * 100,
      # More than 50
      n_more_than_50 = sum(butterflies_direct_sun_t_lag > 50),
      pct_more_than_50 = sum(butterflies_direct_sun_t_lag > 50) / n() * 100,
      # More than 100
      n_more_than_100 = sum(butterflies_direct_sun_t_lag > 100),
      pct_more_than_100 = sum(butterflies_direct_sun_t_lag > 100) / n() * 100
    )

  # Print key statistics
  cat(sprintf("Total observations: %d\n\n", n_total))

  cat("BUTTERFLIES IN DIRECT SUN - BREAKDOWN:\n")
  cat("---------------------------------------\n")
  cat(sprintf("     0 butterflies: %4d observations (%.1f%%)\n",
              sun_categories$n_zero, sun_categories$pct_zero))
  cat(sprintf("     1 butterfly:   %4d observations (%.1f%%)\n",
              sun_categories$n_exactly_1, sun_categories$pct_exactly_1))
  cat(sprintf("   1-5 butterflies: %4d observations (%.1f%%)\n",
              sun_categories$n_1_to_5, sun_categories$pct_1_to_5))
  cat(sprintf("  1-10 butterflies: %4d observations (%.1f%%)\n",
              sun_categories$n_1_to_10, sun_categories$pct_1_to_10))
  cat(sprintf("    <10 butterflies: %4d observations (%.1f%%)\n",
              sun_categories$n_less_than_10, sun_categories$pct_less_than_10))

  cat("\n")
  cat("RARE EVENTS (butterflies in direct sun):\n")
  cat("-----------------------------------------\n")
  cat(sprintf("  >10 butterflies: %4d observations (%.1f%%) *** RARE ***\n",
              sun_categories$n_more_than_10, sun_categories$pct_more_than_10))
  cat(sprintf("  >20 butterflies: %4d observations (%.1f%%) *** VERY RARE ***\n",
              sun_categories$n_more_than_20, sun_categories$pct_more_than_20))
  cat(sprintf("  >50 butterflies: %4d observations (%.1f%%) *** EXTREMELY RARE ***\n",
              sun_categories$n_more_than_50, sun_categories$pct_more_than_50))
  cat(sprintf(" >100 butterflies: %4d observations (%.1f%%) *** EXCEPTIONALLY RARE ***\n",
              sun_categories$n_more_than_100, sun_categories$pct_more_than_100))

  # Get actual values for rare events
  rare_events <- complete_30min %>%
    filter(butterflies_direct_sun_t_lag > 10) %>%
    arrange(desc(butterflies_direct_sun_t_lag))

  cat("\n")
  cat(sprintf("Highest values of butterflies in sun (top 10): %s\n",
              paste(head(rare_events$butterflies_direct_sun_t_lag, 10), collapse = ", ")))

  # Distribution summary
  cat("\n\nSUMMARY STATISTICS:\n")
  cat("-------------------\n")
  quantiles <- quantile(complete_30min$butterflies_direct_sun_t_lag,
                        probs = c(0, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1))

  cat("Percentiles of butterflies in direct sun:\n")
  cat(sprintf("   0%% (min):  %.0f\n", quantiles[1]))
  cat(sprintf("  25%%:        %.0f\n", quantiles[2]))
  cat(sprintf("  50%% (median): %.0f\n", quantiles[3]))
  cat(sprintf("  75%%:        %.0f\n", quantiles[4]))
  cat(sprintf("  90%%:        %.0f\n", quantiles[5]))
  cat(sprintf("  95%%:        %.0f\n", quantiles[6]))
  cat(sprintf("  99%%:        %.0f\n", quantiles[7]))
  cat(sprintf(" 100%% (max):  %.0f\n", quantiles[8]))

  cat(sprintf("\nMean:   %.2f\n", mean(complete_30min$butterflies_direct_sun_t_lag)))
  cat(sprintf("Median: %.0f\n", median(complete_30min$butterflies_direct_sun_t_lag)))
  cat(sprintf("SD:     %.2f\n", sd(complete_30min$butterflies_direct_sun_t_lag)))

} else {
  cat("30-minute data file not found\n")
}

# ----------------------------------------------------------------------------
# Sunset window analysis
# ----------------------------------------------------------------------------
cat("\n\n========================================\n")
cat("SUNSET WINDOW ANALYSIS\n")
cat("----------------------------\n\n")

data_sunset_file <- here("data", "monarch_daily_lag_analysis_sunset_window.csv")
if (file.exists(data_sunset_file)) {
  dat_sunset <- readr::read_csv(data_sunset_file, show_col_types = FALSE)

  # Filter to complete cases
  complete_sunset <- dat_sunset %>%
    filter(!is.na(sum_butterflies_direct_sun))

  n_total <- nrow(complete_sunset)

  # Calculate different thresholds for butterflies in sun
  sun_categories <- complete_sunset %>%
    summarise(
      total_obs = n(),
      # Zero
      n_zero = sum(sum_butterflies_direct_sun == 0),
      pct_zero = sum(sum_butterflies_direct_sun == 0) / n() * 100,
      # 1-10
      n_1_to_10 = sum(sum_butterflies_direct_sun >= 1 & sum_butterflies_direct_sun <= 10),
      pct_1_to_10 = sum(sum_butterflies_direct_sun >= 1 & sum_butterflies_direct_sun <= 10) / n() * 100,
      # Less than 10
      n_less_than_10 = sum(sum_butterflies_direct_sun < 10),
      pct_less_than_10 = sum(sum_butterflies_direct_sun < 10) / n() * 100,
      # More than 10
      n_more_than_10 = sum(sum_butterflies_direct_sun > 10),
      pct_more_than_10 = sum(sum_butterflies_direct_sun > 10) / n() * 100,
      # More than 50
      n_more_than_50 = sum(sum_butterflies_direct_sun > 50),
      pct_more_than_50 = sum(sum_butterflies_direct_sun > 50) / n() * 100,
      # More than 100
      n_more_than_100 = sum(sum_butterflies_direct_sun > 100),
      pct_more_than_100 = sum(sum_butterflies_direct_sun > 100) / n() * 100,
      # More than 200
      n_more_than_200 = sum(sum_butterflies_direct_sun > 200),
      pct_more_than_200 = sum(sum_butterflies_direct_sun > 200) / n() * 100,
      # More than 500
      n_more_than_500 = sum(sum_butterflies_direct_sun > 500),
      pct_more_than_500 = sum(sum_butterflies_direct_sun > 500) / n() * 100
    )

  # Print key statistics
  cat(sprintf("Total days: %d\n\n", n_total))

  cat("SUM OF BUTTERFLIES IN DIRECT SUN - BREAKDOWN:\n")
  cat("----------------------------------------------\n")
  cat(sprintf("      0 butterflies: %3d days (%.1f%%)\n",
              sun_categories$n_zero, sun_categories$pct_zero))
  cat(sprintf("   1-10 butterflies: %3d days (%.1f%%)\n",
              sun_categories$n_1_to_10, sun_categories$pct_1_to_10))
  cat(sprintf("    <10 butterflies: %3d days (%.1f%%)\n",
              sun_categories$n_less_than_10, sun_categories$pct_less_than_10))

  cat("\n")
  cat("FREQUENCY OF DIFFERENT THRESHOLDS:\n")
  cat("-----------------------------------\n")
  cat(sprintf("   >10 butterflies: %3d days (%.1f%%)\n",
              sun_categories$n_more_than_10, sun_categories$pct_more_than_10))
  cat(sprintf("   >50 butterflies: %3d days (%.1f%%)\n",
              sun_categories$n_more_than_50, sun_categories$pct_more_than_50))
  cat(sprintf("  >100 butterflies: %3d days (%.1f%%)\n",
              sun_categories$n_more_than_100, sun_categories$pct_more_than_100))
  cat(sprintf("  >200 butterflies: %3d days (%.1f%%)\n",
              sun_categories$n_more_than_200, sun_categories$pct_more_than_200))
  cat(sprintf("  >500 butterflies: %3d days (%.1f%%)\n",
              sun_categories$n_more_than_500, sun_categories$pct_more_than_500))

  # Get actual values for high days
  high_days <- complete_sunset %>%
    filter(sum_butterflies_direct_sun > 100) %>%
    arrange(desc(sum_butterflies_direct_sun))

  cat("\n")
  cat(sprintf("Highest daily sums of butterflies in sun (top 10): %s\n",
              paste(head(high_days$sum_butterflies_direct_sun, 10), collapse = ", ")))

  # Distribution summary
  cat("\n\nSUMMARY STATISTICS:\n")
  cat("-------------------\n")
  quantiles <- quantile(complete_sunset$sum_butterflies_direct_sun,
                        probs = c(0, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1))

  cat("Percentiles of sum butterflies in direct sun:\n")
  cat(sprintf("   0%% (min):  %.0f\n", quantiles[1]))
  cat(sprintf("  25%%:        %.0f\n", quantiles[2]))
  cat(sprintf("  50%% (median): %.0f\n", quantiles[3]))
  cat(sprintf("  75%%:        %.0f\n", quantiles[4]))
  cat(sprintf("  90%%:        %.0f\n", quantiles[5]))
  cat(sprintf("  95%%:        %.0f\n", quantiles[6]))
  cat(sprintf("  99%%:        %.0f\n", quantiles[7]))
  cat(sprintf(" 100%% (max):  %.0f\n", quantiles[8]))

  cat(sprintf("\nMean:   %.2f\n", mean(complete_sunset$sum_butterflies_direct_sun)))
  cat(sprintf("Median: %.0f\n", median(complete_sunset$sum_butterflies_direct_sun)))
  cat(sprintf("SD:     %.2f\n", sd(complete_sunset$sum_butterflies_direct_sun)))

} else {
  cat("Sunset window data file not found\n")
}

# ----------------------------------------------------------------------------
# FINAL COMPARISON
# ----------------------------------------------------------------------------
cat("\n\n========================================\n")
cat("KEY FINDING: RARITY OF BUTTERFLIES IN DIRECT SUN\n")
cat("========================================\n\n")

if (exists("complete_30min")) {
  pct_more_than_10 <- sum(complete_30min$butterflies_direct_sun_t_lag > 10) / nrow(complete_30min) * 100
  pct_less_than_10 <- sum(complete_30min$butterflies_direct_sun_t_lag < 10) / nrow(complete_30min) * 100

  cat("30-MINUTE INTERVALS:\n")
  cat("--------------------\n")
  cat(sprintf("Observations with <10 butterflies in sun:  %.1f%%\n", pct_less_than_10))
  cat(sprintf("Observations with >10 butterflies in sun:  %.1f%% *** RARE EVENT ***\n", pct_more_than_10))
  cat(sprintf("\nThis means that %.1f%% of all 30-minute observations have fewer than 10 butterflies in direct sun!\n",
              pct_less_than_10))
}

if (exists("complete_sunset")) {
  pct_more_than_10 <- sum(complete_sunset$sum_butterflies_direct_sun > 10) / nrow(complete_sunset) * 100
  pct_less_than_10 <- sum(complete_sunset$sum_butterflies_direct_sun < 10) / nrow(complete_sunset) * 100

  cat("\n\nSUNSET WINDOW:\n")
  cat("--------------\n")
  cat(sprintf("Days with <10 total butterflies in sun:  %.1f%%\n", pct_less_than_10))
  cat(sprintf("Days with >10 total butterflies in sun:  %.1f%%\n", pct_more_than_10))
}

cat("\n========================================\n")
cat("Analysis complete!\n")
cat("========================================\n")