#!/usr/bin/env Rscript

# Script to create histograms comparing total butterflies vs butterflies in direct sunlight
# For both 30-minute and sunset window analyses

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(here)
})

# Create output directory
output_dir <- here("analysis", "reports", "butterfly_distributions")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Custom theme matching the style from the GAM reports
custom_theme <- theme_minimal(base_size = 12) + theme(
  panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
  panel.grid.minor = element_line(color = "gray95", linewidth = 0.3),
  axis.text = element_text(color = "black"),
  axis.title = element_text(color = "black", face = "bold"),
  plot.title = element_text(size = 14, hjust = 0.5, face = "plain"),
  plot.subtitle = element_text(size = 11, hjust = 0.5, face = "italic", color = "gray40"),
  legend.position = "top"
)

# Colors for the two groups
col_total <- "#2c7fb8"  # Blue for total butterflies
col_sun <- "#fd8d3c"    # Orange for butterflies in sun

# ----------------------------------------------------------------------------
# 30-minute analysis data
# ----------------------------------------------------------------------------
cat("Loading 30-minute analysis data...\n")
data_30min_file <- here("data", "monarch_analysis_lag30min.csv")
if (file.exists(data_30min_file)) {
  dat_30min <- readr::read_csv(data_30min_file, show_col_types = FALSE)

  # Create long format for easier plotting
  butterfly_data_30min <- dat_30min %>%
    filter(!is.na(total_butterflies_t_lag) & !is.na(butterflies_direct_sun_t_lag)) %>%
    select(
      total = total_butterflies_t_lag,
      in_sun = butterflies_direct_sun_t_lag
    ) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "count") %>%
    mutate(
      type = factor(type,
                    levels = c("total", "in_sun"),
                    labels = c("Total butterflies", "Butterflies in direct sun"))
    )

  # Summary statistics
  stats_30min <- dat_30min %>%
    filter(!is.na(total_butterflies_t_lag) & !is.na(butterflies_direct_sun_t_lag)) %>%
    summarise(
      total_mean = mean(total_butterflies_t_lag),
      total_median = median(total_butterflies_t_lag),
      total_sd = sd(total_butterflies_t_lag),
      sun_mean = mean(butterflies_direct_sun_t_lag),
      sun_median = median(butterflies_direct_sun_t_lag),
      sun_sd = sd(butterflies_direct_sun_t_lag),
      n = n()
    )

  cat("\n30-minute analysis statistics:\n")
  cat(sprintf("Total butterflies: Mean = %.1f, Median = %.0f, SD = %.1f\n",
              stats_30min$total_mean, stats_30min$total_median, stats_30min$total_sd))
  cat(sprintf("Butterflies in sun: Mean = %.1f, Median = %.0f, SD = %.1f\n",
              stats_30min$sun_mean, stats_30min$sun_median, stats_30min$sun_sd))
  cat(sprintf("N observations: %d\n\n", stats_30min$n))

  # Create overlapping histogram for 30-minute data
  p_30min_overlap <- ggplot(butterfly_data_30min, aes(x = count, fill = type)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30, color = "white", linewidth = 0.3) +
    scale_fill_manual(values = c("Total butterflies" = col_total,
                                 "Butterflies in direct sun" = col_sun)) +
    labs(
      x = "Butterfly count",
      y = "Frequency",
      title = "Distribution of Butterfly Counts (30-minute intervals)",
      subtitle = sprintf("N = %d observations", stats_30min$n),
      fill = ""
    ) +
    custom_theme +
    geom_vline(xintercept = stats_30min$total_median,
               color = col_total, linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = stats_30min$sun_median,
               color = col_sun, linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = stats_30min$total_median, y = Inf,
             label = sprintf("Median total: %.0f", stats_30min$total_median),
             vjust = 2, hjust = -0.1, color = col_total, size = 3.5) +
    annotate("text", x = stats_30min$sun_median, y = Inf,
             label = sprintf("Median sun: %.0f", stats_30min$sun_median),
             vjust = 3.5, hjust = -0.1, color = col_sun, size = 3.5)

  # Create side-by-side histogram for 30-minute data
  p_30min_side <- ggplot(butterfly_data_30min, aes(x = count, fill = type)) +
    geom_histogram(bins = 30, color = "white", linewidth = 0.3) +
    facet_wrap(~ type, scales = "free_y", ncol = 1) +
    scale_fill_manual(values = c("Total butterflies" = col_total,
                                 "Butterflies in direct sun" = col_sun)) +
    labs(
      x = "Butterfly count",
      y = "Frequency",
      title = "Distribution of Butterfly Counts (30-minute intervals)",
      subtitle = sprintf("N = %d observations", stats_30min$n)
    ) +
    custom_theme +
    theme(legend.position = "none")

  # Save 30-minute plots
  ggsave(
    file.path(output_dir, "butterflies_histogram_30min_overlapping.png"),
    p_30min_overlap, width = 10, height = 6, dpi = 300, bg = "white"
  )

  ggsave(
    file.path(output_dir, "butterflies_histogram_30min_sidebyside.png"),
    p_30min_side, width = 8, height = 8, dpi = 300, bg = "white"
  )

  cat("Saved 30-minute histograms\n")

} else {
  cat("30-minute data file not found\n")
}

# ----------------------------------------------------------------------------
# Sunset window analysis data
# ----------------------------------------------------------------------------
cat("\nLoading sunset window analysis data...\n")
data_sunset_file <- here("data", "monarch_daily_lag_analysis_sunset_window.csv")
if (file.exists(data_sunset_file)) {
  dat_sunset <- readr::read_csv(data_sunset_file, show_col_types = FALSE)

  # For sunset data, we use max_butterflies_t_1 and sum_butterflies_direct_sun
  butterfly_data_sunset <- dat_sunset %>%
    filter(!is.na(max_butterflies_t_1) & !is.na(sum_butterflies_direct_sun)) %>%
    select(
      total = max_butterflies_t_1,
      in_sun = sum_butterflies_direct_sun
    ) %>%
    pivot_longer(cols = everything(), names_to = "type", values_to = "count") %>%
    mutate(
      type = factor(type,
                    levels = c("total", "in_sun"),
                    labels = c("Max butterfly count", "Sum butterflies in sun"))
    )

  # Summary statistics
  stats_sunset <- dat_sunset %>%
    filter(!is.na(max_butterflies_t_1) & !is.na(sum_butterflies_direct_sun)) %>%
    summarise(
      total_mean = mean(max_butterflies_t_1),
      total_median = median(max_butterflies_t_1),
      total_sd = sd(max_butterflies_t_1),
      sun_mean = mean(sum_butterflies_direct_sun),
      sun_median = median(sum_butterflies_direct_sun),
      sun_sd = sd(sum_butterflies_direct_sun),
      n = n()
    )

  cat("\nSunset window analysis statistics:\n")
  cat(sprintf("Max butterfly count: Mean = %.1f, Median = %.0f, SD = %.1f\n",
              stats_sunset$total_mean, stats_sunset$total_median, stats_sunset$total_sd))
  cat(sprintf("Sum butterflies in sun: Mean = %.1f, Median = %.0f, SD = %.1f\n",
              stats_sunset$sun_mean, stats_sunset$sun_median, stats_sunset$sun_sd))
  cat(sprintf("N observations: %d\n\n", stats_sunset$n))

  # Create overlapping histogram for sunset data
  p_sunset_overlap <- ggplot(butterfly_data_sunset, aes(x = count, fill = type)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30, color = "white", linewidth = 0.3) +
    scale_fill_manual(values = c("Max butterfly count" = col_total,
                                 "Sum butterflies in sun" = col_sun)) +
    labs(
      x = "Butterfly count",
      y = "Frequency",
      title = "Distribution of Butterfly Counts (Sunset window)",
      subtitle = sprintf("N = %d daily observations", stats_sunset$n),
      fill = ""
    ) +
    custom_theme +
    geom_vline(xintercept = stats_sunset$total_median,
               color = col_total, linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = stats_sunset$sun_median,
               color = col_sun, linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = stats_sunset$total_median, y = Inf,
             label = sprintf("Median max: %.0f", stats_sunset$total_median),
             vjust = 2, hjust = -0.1, color = col_total, size = 3.5) +
    annotate("text", x = stats_sunset$sun_median, y = Inf,
             label = sprintf("Median sun: %.0f", stats_sunset$sun_median),
             vjust = 3.5, hjust = -0.1, color = col_sun, size = 3.5)

  # Create side-by-side histogram for sunset data
  p_sunset_side <- ggplot(butterfly_data_sunset, aes(x = count, fill = type)) +
    geom_histogram(bins = 30, color = "white", linewidth = 0.3) +
    facet_wrap(~ type, scales = "free", ncol = 1) +
    scale_fill_manual(values = c("Max butterfly count" = col_total,
                                 "Sum butterflies in sun" = col_sun)) +
    labs(
      x = "Butterfly count",
      y = "Frequency",
      title = "Distribution of Butterfly Counts (Sunset window)",
      subtitle = sprintf("N = %d daily observations", stats_sunset$n)
    ) +
    custom_theme +
    theme(legend.position = "none")

  # Save sunset plots
  ggsave(
    file.path(output_dir, "butterflies_histogram_sunset_overlapping.png"),
    p_sunset_overlap, width = 10, height = 6, dpi = 300, bg = "white"
  )

  ggsave(
    file.path(output_dir, "butterflies_histogram_sunset_sidebyside.png"),
    p_sunset_side, width = 8, height = 8, dpi = 300, bg = "white"
  )

  cat("Saved sunset window histograms\n")

} else {
  cat("Sunset window data file not found\n")
}

# ----------------------------------------------------------------------------
# Create combined comparison plot
# ----------------------------------------------------------------------------
if (exists("butterfly_data_30min") && exists("butterfly_data_sunset")) {

  # Add analysis type to each dataset
  butterfly_data_30min <- butterfly_data_30min %>%
    mutate(analysis = "30-minute intervals")

  butterfly_data_sunset <- butterfly_data_sunset %>%
    mutate(
      analysis = "Sunset window",
      # Rename for clarity in combined plot
      type = recode(type,
                   "Max butterfly count" = "Total butterflies",
                   "Sum butterflies in sun" = "Butterflies in direct sun")
    )

  # Combine datasets
  butterfly_combined <- bind_rows(butterfly_data_30min, butterfly_data_sunset)

  # Create combined comparison plot
  p_combined <- ggplot(butterfly_combined, aes(x = count, fill = type)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30,
                   color = "white", linewidth = 0.3) +
    facet_wrap(~ analysis, scales = "free_y", ncol = 1) +
    scale_fill_manual(values = c("Total butterflies" = col_total,
                                 "Butterflies in direct sun" = col_sun)) +
    labs(
      x = "Butterfly count",
      y = "Frequency",
      title = "Comparison of Butterfly Count Distributions",
      subtitle = "30-minute intervals vs Sunset window analysis",
      fill = ""
    ) +
    custom_theme

  # Save combined plot
  ggsave(
    file.path(output_dir, "butterflies_histogram_combined.png"),
    p_combined, width = 10, height = 8, dpi = 300, bg = "white"
  )

  cat("\nSaved combined comparison histogram\n")

  # Create proportion in sun comparison
  prop_30min <- dat_30min %>%
    filter(!is.na(total_butterflies_t_lag) & !is.na(butterflies_direct_sun_t_lag)) %>%
    mutate(prop_in_sun = butterflies_direct_sun_t_lag / pmax(total_butterflies_t_lag, 1)) %>%
    pull(prop_in_sun)

  prop_sunset <- dat_sunset %>%
    filter(!is.na(max_butterflies_t_1) & !is.na(sum_butterflies_direct_sun)) %>%
    mutate(prop_in_sun = sum_butterflies_direct_sun / pmax(max_butterflies_t_1, 1)) %>%
    pull(prop_in_sun)

  prop_data <- tibble(
    proportion = c(prop_30min, prop_sunset),
    analysis = c(rep("30-minute intervals", length(prop_30min)),
                rep("Sunset window", length(prop_sunset)))
  )

  p_proportion <- ggplot(prop_data, aes(x = proportion, fill = analysis)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30,
                   color = "white", linewidth = 0.3) +
    scale_fill_manual(values = c("30-minute intervals" = col_total,
                                 "Sunset window" = col_sun)) +
    scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(
      x = "Proportion of butterflies in direct sun",
      y = "Frequency",
      title = "Distribution of Proportion of Butterflies in Direct Sun",
      subtitle = "Comparison between analysis windows",
      fill = "Analysis type"
    ) +
    custom_theme +
    geom_vline(xintercept = median(prop_30min, na.rm = TRUE),
               color = col_total, linetype = "dashed", linewidth = 0.8, alpha = 0.7) +
    geom_vline(xintercept = median(prop_sunset, na.rm = TRUE),
               color = col_sun, linetype = "dashed", linewidth = 0.8, alpha = 0.7)

  ggsave(
    file.path(output_dir, "butterflies_proportion_in_sun.png"),
    p_proportion, width = 10, height = 6, dpi = 300, bg = "white"
  )

  cat("Saved proportion in sun histogram\n")

  # Print proportion statistics
  cat("\nProportion of butterflies in direct sun:\n")
  cat(sprintf("30-minute: Mean = %.1f%%, Median = %.1f%%\n",
              mean(prop_30min, na.rm = TRUE) * 100,
              median(prop_30min, na.rm = TRUE) * 100))
  cat(sprintf("Sunset: Mean = %.1f%%, Median = %.1f%%\n",
              mean(prop_sunset, na.rm = TRUE) * 100,
              median(prop_sunset, na.rm = TRUE) * 100))
}

# ----------------------------------------------------------------------------
# Export to thesis directories if needed
# ----------------------------------------------------------------------------
# Copy key plots to thesis export directories
thesis_30min_dir <- here("thesis_exports", "30_min", "figures")
thesis_sunset_dir <- here("thesis_exports", "sunset", "figures")

if (dir.exists(thesis_30min_dir) && exists("p_30min_overlap")) {
  file.copy(
    file.path(output_dir, "butterflies_histogram_30min_overlapping.png"),
    file.path(thesis_30min_dir, "butterflies_distribution_histogram.png"),
    overwrite = TRUE
  )
  cat("\nCopied 30-minute histogram to thesis exports\n")
}

if (dir.exists(thesis_sunset_dir) && exists("p_sunset_overlap")) {
  file.copy(
    file.path(output_dir, "butterflies_histogram_sunset_overlapping.png"),
    file.path(thesis_sunset_dir, "butterflies_distribution_histogram.png"),
    overwrite = TRUE
  )
  cat("Copied sunset histogram to thesis exports\n")
}

cat("\n========================================\n")
cat("All histograms saved to:", output_dir, "\n")
cat("========================================\n")