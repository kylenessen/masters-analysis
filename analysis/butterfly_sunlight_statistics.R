#!/usr/bin/env Rscript

# Script to generate comprehensive descriptive statistics for butterfly observations
# Focusing on butterflies in direct sunlight for both analyses

suppressPackageStartupMessages({
  library(tidyverse)
  library(knitr)
  library(here)
})

# Create output directory
output_dir <- here("analysis", "reports", "butterfly_distributions")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Function to calculate comprehensive statistics
calculate_stats <- function(data, total_col, sun_col, analysis_name) {

  # Filter to complete cases
  complete_data <- data %>%
    filter(!is.na({{total_col}}) & !is.na({{sun_col}}))

  n_total <- nrow(complete_data)

  # Calculate statistics for total butterflies
  total_stats <- complete_data %>%
    summarise(
      mean = mean({{total_col}}),
      median = median({{total_col}}),
      sd = sd({{total_col}}),
      min = min({{total_col}}),
      max = max({{total_col}}),
      q25 = quantile({{total_col}}, 0.25),
      q75 = quantile({{total_col}}, 0.75),
      iqr = IQR({{total_col}}),
      n_zero = sum({{total_col}} == 0),
      pct_zero = sum({{total_col}} == 0) / n() * 100
    ) %>%
    mutate(variable = "Total butterflies")

  # Calculate statistics for butterflies in sun
  sun_stats <- complete_data %>%
    summarise(
      mean = mean({{sun_col}}),
      median = median({{sun_col}}),
      sd = sd({{sun_col}}),
      min = min({{sun_col}}),
      max = max({{sun_col}}),
      q25 = quantile({{sun_col}}, 0.25),
      q75 = quantile({{sun_col}}, 0.75),
      iqr = IQR({{sun_col}}),
      n_zero = sum({{sun_col}} == 0),
      pct_zero = sum({{sun_col}} == 0) / n() * 100
    ) %>%
    mutate(variable = "Butterflies in sun")

  # Combine statistics
  combined_stats <- bind_rows(total_stats, sun_stats) %>%
    mutate(analysis = analysis_name, .before = variable) %>%
    mutate(n_total = n_total)

  # Calculate proportion statistics when total > 0
  prop_stats <- complete_data %>%
    filter({{total_col}} > 0) %>%
    mutate(prop_in_sun = {{sun_col}} / {{total_col}}) %>%
    summarise(
      n_with_butterflies = n(),
      prop_mean = mean(prop_in_sun),
      prop_median = median(prop_in_sun),
      prop_sd = sd(prop_in_sun),
      prop_min = min(prop_in_sun),
      prop_max = max(prop_in_sun),
      n_zero_sun_when_total_positive = sum({{sun_col}} == 0),
      pct_zero_sun_when_total_positive = sum({{sun_col}} == 0) / n() * 100
    ) %>%
    mutate(analysis = analysis_name)

  # Distribution of sun observations by total butterfly count bins
  distribution_by_bins <- complete_data %>%
    mutate(
      total_bin = cut({{total_col}},
                      breaks = c(-Inf, 0, 10, 50, 100, 200, Inf),
                      labels = c("0", "1-10", "11-50", "51-100", "101-200", ">200"))
    ) %>%
    group_by(total_bin) %>%
    summarise(
      n_obs = n(),
      mean_sun = mean({{sun_col}}),
      median_sun = median({{sun_col}}),
      n_zero_sun = sum({{sun_col}} == 0),
      pct_zero_sun = sum({{sun_col}} == 0) / n() * 100,
      .groups = "drop"
    ) %>%
    mutate(analysis = analysis_name)

  # Additional category: less than 10 butterflies
  less_than_10_stats <- complete_data %>%
    filter({{total_col}} < 10) %>%
    summarise(
      n_obs = n(),
      mean_sun = mean({{sun_col}}),
      median_sun = median({{sun_col}}),
      n_zero_sun = sum({{sun_col}} == 0),
      pct_zero_sun = sum({{sun_col}} == 0) / n() * 100,
      pct_of_total_obs = n() / nrow(complete_data) * 100
    ) %>%
    mutate(analysis = analysis_name, category = "<10 butterflies")

  return(list(
    basic_stats = combined_stats,
    proportion_stats = prop_stats,
    distribution_by_bins = distribution_by_bins,
    less_than_10 = less_than_10_stats
  ))
}

# ----------------------------------------------------------------------------
# 30-minute analysis
# ----------------------------------------------------------------------------
cat("========================================\n")
cat("30-MINUTE INTERVAL ANALYSIS\n")
cat("========================================\n\n")

data_30min_file <- here("data", "monarch_analysis_lag30min.csv")
if (file.exists(data_30min_file)) {
  dat_30min <- readr::read_csv(data_30min_file, show_col_types = FALSE)

  stats_30min <- calculate_stats(
    dat_30min,
    total_butterflies_t_lag,
    butterflies_direct_sun_t_lag,
    "30-minute intervals"
  )

  # Print detailed statistics
  cat("Basic Statistics:\n")
  cat("-----------------\n")
  stats_30min$basic_stats %>%
    select(variable, n_zero, pct_zero, mean, median, sd, min, max, q25, q75) %>%
    kable(format = "simple", digits = 2) %>%
    print()

  cat("\n\nZERO OBSERVATIONS FOR BUTTERFLIES IN DIRECT SUN:\n")
  cat("------------------------------------------------\n")
  zero_stats_30min <- stats_30min$basic_stats %>%
    filter(variable == "Butterflies in sun")
  cat(sprintf("Total observations: %d\n", zero_stats_30min$n_total))
  cat(sprintf("Observations with ZERO butterflies in sun: %d\n", zero_stats_30min$n_zero))
  cat(sprintf("Percentage with ZERO butterflies in sun: %.1f%%\n\n", zero_stats_30min$pct_zero))

  cat("When total butterflies > 0:\n")
  cat(sprintf("  Observations with butterflies present: %d\n",
              stats_30min$proportion_stats$n_with_butterflies))
  cat(sprintf("  Of these, observations with ZERO in sun: %d\n",
              stats_30min$proportion_stats$n_zero_sun_when_total_positive))
  cat(sprintf("  Percentage with ZERO in sun (when butterflies present): %.1f%%\n\n",
              stats_30min$proportion_stats$pct_zero_sun_when_total_positive))

  cat("Distribution by butterfly count bins:\n")
  stats_30min$distribution_by_bins %>%
    select(total_bin, n_obs, n_zero_sun, pct_zero_sun, mean_sun, median_sun) %>%
    kable(format = "simple", digits = 2,
          col.names = c("Total butterflies", "N obs", "N zero sun", "% zero sun", "Mean sun", "Median sun")) %>%
    print()

  cat("\n\nSPECIAL FOCUS: Less than 10 butterflies:\n")
  cat("------------------------------------------\n")
  cat(sprintf("Observations with <10 butterflies: %d (%.1f%% of all observations)\n",
              stats_30min$less_than_10$n_obs,
              stats_30min$less_than_10$pct_of_total_obs))
  cat(sprintf("Of these, observations with ZERO in sun: %d (%.1f%%)\n",
              stats_30min$less_than_10$n_zero_sun,
              stats_30min$less_than_10$pct_zero_sun))
  cat(sprintf("Mean butterflies in sun when <10 total: %.2f\n",
              stats_30min$less_than_10$mean_sun))
  cat(sprintf("Median butterflies in sun when <10 total: %.0f\n",
              stats_30min$less_than_10$median_sun))

} else {
  cat("30-minute data file not found\n")
}

# ----------------------------------------------------------------------------
# Sunset window analysis
# ----------------------------------------------------------------------------
cat("\n\n========================================\n")
cat("SUNSET WINDOW ANALYSIS\n")
cat("========================================\n\n")

data_sunset_file <- here("data", "monarch_daily_lag_analysis_sunset_window.csv")
if (file.exists(data_sunset_file)) {
  dat_sunset <- readr::read_csv(data_sunset_file, show_col_types = FALSE)

  stats_sunset <- calculate_stats(
    dat_sunset,
    max_butterflies_t_1,
    sum_butterflies_direct_sun,
    "Sunset window"
  )

  # Print detailed statistics
  cat("Basic Statistics:\n")
  cat("-----------------\n")
  stats_sunset$basic_stats %>%
    select(variable, n_zero, pct_zero, mean, median, sd, min, max, q25, q75) %>%
    mutate(
      variable = case_when(
        variable == "Total butterflies" ~ "Max butterfly count",
        variable == "Butterflies in sun" ~ "Sum butterflies in sun",
        TRUE ~ variable
      )
    ) %>%
    kable(format = "simple", digits = 2) %>%
    print()

  cat("\n\nZERO OBSERVATIONS FOR BUTTERFLIES IN DIRECT SUN:\n")
  cat("------------------------------------------------\n")
  zero_stats_sunset <- stats_sunset$basic_stats %>%
    filter(variable == "Butterflies in sun")
  cat(sprintf("Total observations (days): %d\n", zero_stats_sunset$n_total))
  cat(sprintf("Days with ZERO butterflies in sun: %d\n", zero_stats_sunset$n_zero))
  cat(sprintf("Percentage with ZERO butterflies in sun: %.1f%%\n\n", zero_stats_sunset$pct_zero))

  cat("When max butterflies > 0:\n")
  cat(sprintf("  Days with butterflies present: %d\n",
              stats_sunset$proportion_stats$n_with_butterflies))
  cat(sprintf("  Of these, days with ZERO in sun: %d\n",
              stats_sunset$proportion_stats$n_zero_sun_when_total_positive))
  cat(sprintf("  Percentage with ZERO in sun (when butterflies present): %.1f%%\n\n",
              stats_sunset$proportion_stats$pct_zero_sun_when_total_positive))

  cat("Distribution by max butterfly count bins:\n")
  stats_sunset$distribution_by_bins %>%
    select(total_bin, n_obs, n_zero_sun, pct_zero_sun, mean_sun, median_sun) %>%
    kable(format = "simple", digits = 2,
          col.names = c("Max butterflies", "N days", "N zero sun", "% zero sun", "Mean sun", "Median sun")) %>%
    print()

  cat("\n\nSPECIAL FOCUS: Less than 10 max butterflies:\n")
  cat("----------------------------------------------\n")
  cat(sprintf("Days with <10 max butterflies: %d (%.1f%% of all days)\n",
              stats_sunset$less_than_10$n_obs,
              stats_sunset$less_than_10$pct_of_total_obs))
  cat(sprintf("Of these, days with ZERO in sun: %d (%.1f%%)\n",
              stats_sunset$less_than_10$n_zero_sun,
              stats_sunset$less_than_10$pct_zero_sun))
  cat(sprintf("Mean sum butterflies in sun when <10 max: %.2f\n",
              stats_sunset$less_than_10$mean_sun))
  cat(sprintf("Median sum butterflies in sun when <10 max: %.0f\n",
              stats_sunset$less_than_10$median_sun))

} else {
  cat("Sunset window data file not found\n")
}

# ----------------------------------------------------------------------------
# Comparative summary
# ----------------------------------------------------------------------------
cat("\n\n========================================\n")
cat("COMPARATIVE SUMMARY\n")
cat("========================================\n\n")

if (exists("stats_30min") && exists("stats_sunset")) {

  # Create comparison table for zero observations
  zero_comparison <- tibble(
    Analysis = c("30-minute intervals", "Sunset window"),
    `Total obs` = c(
      stats_30min$basic_stats$n_total[1],
      stats_sunset$basic_stats$n_total[1]
    ),
    `Zero sun (n)` = c(
      stats_30min$basic_stats %>% filter(variable == "Butterflies in sun") %>% pull(n_zero),
      stats_sunset$basic_stats %>% filter(variable == "Butterflies in sun") %>% pull(n_zero)
    ),
    `Zero sun (%)` = c(
      stats_30min$basic_stats %>% filter(variable == "Butterflies in sun") %>% pull(pct_zero),
      stats_sunset$basic_stats %>% filter(variable == "Butterflies in sun") %>% pull(pct_zero)
    ),
    `Mean sun` = c(
      stats_30min$basic_stats %>% filter(variable == "Butterflies in sun") %>% pull(mean),
      stats_sunset$basic_stats %>% filter(variable == "Butterflies in sun") %>% pull(mean)
    ),
    `Median sun` = c(
      stats_30min$basic_stats %>% filter(variable == "Butterflies in sun") %>% pull(median),
      stats_sunset$basic_stats %>% filter(variable == "Butterflies in sun") %>% pull(median)
    ),
    `Max sun` = c(
      stats_30min$basic_stats %>% filter(variable == "Butterflies in sun") %>% pull(max),
      stats_sunset$basic_stats %>% filter(variable == "Butterflies in sun") %>% pull(max)
    )
  )

  cat("Zero butterfly observations in direct sun:\n")
  zero_comparison %>%
    kable(format = "simple", digits = 1) %>%
    print()

  # Save results to CSV
  write_csv(zero_comparison,
            file.path(output_dir, "butterfly_sun_zero_statistics.csv"))

  # Save detailed statistics
  all_basic_stats <- bind_rows(stats_30min$basic_stats, stats_sunset$basic_stats)
  write_csv(all_basic_stats,
            file.path(output_dir, "butterfly_descriptive_statistics.csv"))

  all_bin_stats <- bind_rows(stats_30min$distribution_by_bins, stats_sunset$distribution_by_bins)
  write_csv(all_bin_stats,
            file.path(output_dir, "butterfly_distribution_by_bins.csv"))

  cat("\n\nStatistics saved to:\n")
  cat("  - butterfly_sun_zero_statistics.csv\n")
  cat("  - butterfly_descriptive_statistics.csv\n")
  cat("  - butterfly_distribution_by_bins.csv\n")
}

# ----------------------------------------------------------------------------
# Additional analysis: temporal patterns of zeros
# ----------------------------------------------------------------------------
cat("\n\n========================================\n")
cat("TEMPORAL PATTERNS OF ZERO OBSERVATIONS\n")
cat("========================================\n\n")

if (exists("dat_30min")) {
  # Check if there are patterns in when zeros occur
  # First check if DateTime column exists, otherwise try other date columns
  if ("DateTime" %in% names(dat_30min)) {
    temporal_30min <- dat_30min %>%
      filter(!is.na(butterflies_direct_sun_t_lag)) %>%
      mutate(
        hour = lubridate::hour(DateTime),
        has_sun = butterflies_direct_sun_t_lag > 0
      ) %>%
      group_by(hour) %>%
      summarise(
        n_obs = n(),
        n_with_sun = sum(has_sun),
        pct_with_sun = mean(has_sun) * 100,
        .groups = "drop"
      )

    cat("30-minute analysis - Observations with butterflies in sun by hour:\n")
    temporal_30min %>%
      kable(format = "simple", digits = 1,
            col.names = c("Hour", "N obs", "N with sun", "% with sun")) %>%
      print()
  } else if ("date" %in% names(dat_30min) && "time" %in% names(dat_30min)) {
    # Alternative if DateTime doesn't exist but date and time do
    temporal_30min <- dat_30min %>%
      filter(!is.na(butterflies_direct_sun_t_lag)) %>%
      mutate(
        hour = as.numeric(substr(time, 1, 2)),
        has_sun = butterflies_direct_sun_t_lag > 0
      ) %>%
      group_by(hour) %>%
      summarise(
        n_obs = n(),
        n_with_sun = sum(has_sun),
        pct_with_sun = mean(has_sun) * 100,
        .groups = "drop"
      )

    cat("30-minute analysis - Observations with butterflies in sun by hour:\n")
    temporal_30min %>%
      kable(format = "simple", digits = 1,
            col.names = c("Hour", "N obs", "N with sun", "% with sun")) %>%
      print()
  } else {
    cat("Note: DateTime column not found for temporal analysis\n")
  }
}

cat("\n========================================\n")
cat("Analysis complete!\n")
cat("========================================\n")