#!/usr/bin/env Rscript
# ============================================================================
# REVISION FIGURE GENERATION SCRIPT
# Adds two figures requested during manuscript revision:
#   EDIT-012: combined two-panel wind vs dBI scatter (30-min + Next Day Window)
#   EDIT-015: histogram of wind speeds measured at occupied clusters
# Style (theme, fonts, dpi) mirrors generate_publication_figures.R so the new
# figures render identically alongside the existing ones.
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(here)
})

cfg <- list(
  out_dir = here("thesis_exports", "publication_figures"),
  display_width = 6,
  target_axis_title = 12,
  target_axis_text  = 10,
  target_legend_title = 11,
  target_legend_text  = 10,
  scatter_w = 7,
  dpi = 600
)
if (!dir.exists(cfg$out_dir)) dir.create(cfg$out_dir, recursive = TRUE)

make_pub_theme <- function(fig_width) {
  s <- fig_width / cfg$display_width
  theme_minimal(base_size = round(cfg$target_axis_title * s)) +
    theme(
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
      panel.grid.minor = element_line(color = "gray95", linewidth = 0.3),
      axis.text = element_text(color = "black", size = round(cfg$target_axis_text * s)),
      axis.title = element_text(color = "black", size = round(cfg$target_axis_title * s)),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      legend.title = element_text(size = round(cfg$target_legend_title * s)),
      legend.text = element_text(size = round(cfg$target_legend_text * s)),
      strip.text = element_text(size = round(cfg$target_legend_title * s))
    )
}

save_fig <- function(filename, plot, w, h) {
  ggsave(file.path(cfg$out_dir, filename), plot, width = w, height = h, dpi = cfg$dpi, bg = "white")
  cat(sprintf("  Saved: %s\n", filename))
}

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------
monarch_data <- read_csv(here("data", "monarch_analysis_lag30min.csv"), show_col_types = FALSE)

model_data <- monarch_data %>%
  filter(
    !is.na(butterfly_difference_cbrt), !is.na(total_butterflies_t_lag),
    !is.na(max_gust), !is.na(temperature_avg), !is.na(butterflies_direct_sun_t_lag),
    !is.na(deployment_id), !is.na(deployment_day), !is.na(Observer),
    !is.na(observation_order_within_day_t)
  )

sunset_lm_data <- read_csv(here("data", "monarch_daily_lag_analysis_sunset_window.csv"),
                           show_col_types = FALSE) %>%
  filter(!is.na(wind_max_gust), !is.na(butterfly_diff))

cat(sprintf("30-min n = %d; Next Day Window n = %d\n", nrow(model_data), nrow(sunset_lm_data)))

# ---------------------------------------------------------------------------
# EDIT-012: combined two-panel scatter
# ---------------------------------------------------------------------------
p_main <- ggplot(model_data, aes(x = max_gust, y = butterfly_difference)) +
  geom_jitter(alpha = 0.4, size = 1.5, color = "#4d4d4d", width = 0.1, height = 0) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue", fill = "steelblue",
              alpha = 0.25, linewidth = 1) +
  geom_hline(yintercept = 0, color = "gray65", linewidth = 0.5) +
  labs(x = "Maximum wind speed (m/s)",
       y = expression(paste("Change in Butterfly Index (", Delta, "BI)"))) +
  make_pub_theme(cfg$scatter_w)

p_sunset_scatter <- ggplot(sunset_lm_data, aes(x = wind_max_gust, y = butterfly_diff)) +
  geom_jitter(alpha = 0.5, size = 2, color = "#4d4d4d", width = 0.1, height = 0) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue", fill = "steelblue",
              alpha = 0.25, linewidth = 1) +
  geom_hline(yintercept = 0, color = "gray65", linewidth = 0.5) +
  labs(x = "Maximum wind speed (m/s)",
       y = expression(paste("Change in Butterfly Index (", Delta, "BI)"))) +
  make_pub_theme(cfg$scatter_w)

combined <- (p_main + p_sunset_scatter) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16, face = "bold"))

save_fig("fig02_wind_vs_dbi_combined.png", combined, 12, 5)

# ---------------------------------------------------------------------------
# EDIT-015: wind speeds measured at occupied clusters
# ---------------------------------------------------------------------------
occupied <- monarch_data %>% filter(!is.na(max_gust), total_butterflies_t > 0)
n_occ <- nrow(occupied)
n_above <- sum(occupied$max_gust >= 2)
pct_above <- 100 * n_above / n_occ
cat(sprintf("Occupied cluster observations: %d; with max gust >= 2 m/s: %d (%.1f%%)\n",
            n_occ, n_above, pct_above))

p_hist <- ggplot(occupied, aes(x = max_gust)) +
  geom_histogram(binwidth = 0.5, boundary = 0, fill = "steelblue", color = "white", linewidth = 0.2) +
  geom_vline(xintercept = 2, color = "black", linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = 2.2, y = Inf, hjust = 0, vjust = 1.6,
           label = sprintf("%.0f%% of occupied-cluster\nobservations exceeded 2 m/s", pct_above),
           size = 4) +
  labs(x = "Maximum wind speed at occupied clusters (m/s)",
       y = "Number of observations") +
  make_pub_theme(cfg$scatter_w)

save_fig("fig_wind_histogram_occupied.png", p_hist, 7, 5)

cat("\nDone.\n")
