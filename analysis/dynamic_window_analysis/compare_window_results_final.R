#!/usr/bin/env Rscript
# Create clean binned gratia plots without contour lines

library(tidyverse)
library(mgcv)
library(nlme)
library(gratia)
library(patchwork)
library(here)

# ============================================================================
# LOAD DATA AND FIT MODELS
# ============================================================================

cat("Loading data and fitting models...\n")

sunset_data <- read_csv(here("data", "monarch_daily_lag_analysis_sunset_window.csv"), show_col_types = FALSE)
sunset_data <- sunset_data %>%
    mutate(
        butterfly_diff_95th_sqrt = ifelse(butterfly_diff_95th >= 0,
            sqrt(butterfly_diff_95th),
            -sqrt(-butterfly_diff_95th)
        )
    ) %>%
    filter(metrics_complete >= 0.95)

hr24_data <- read_csv(here("data", "monarch_daily_lag_analysis_24hr_window.csv"), show_col_types = FALSE)
hr24_data <- hr24_data %>%
    mutate(
        butterfly_diff_95th_sqrt = ifelse(butterfly_diff_95th >= 0,
            sqrt(butterfly_diff_95th),
            -sqrt(-butterfly_diff_95th)
        )
    ) %>%
    filter(metrics_complete >= 0.95)

sunset_model_data <- sunset_data %>%
    arrange(deployment_id, observation_order_t) %>%
    mutate(deployment_id = factor(deployment_id))

hr24_model_data <- hr24_data %>%
    arrange(deployment_id, observation_order_t) %>%
    mutate(deployment_id = factor(deployment_id))

random_structure <- list(deployment_id = ~1)
ar1_cor <- corAR1(form = ~ observation_order_t | deployment_id)

sunset_model <- gamm(
    butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours +
        ti(wind_max_gust, sum_butterflies_direct_sun),
    data = sunset_model_data,
    random = random_structure,
    correlation = ar1_cor,
    method = "REML"
)

hr24_model <- gamm(
    butterfly_diff_sqrt ~ max_butterflies_t_1 +
        ti(wind_max_gust, sum_butterflies_direct_sun),
    data = hr24_model_data,
    random = random_structure,
    correlation = ar1_cor,
    method = "REML"
)

# ============================================================================
# CREATE CLEAN BINNED PLOTS WITHOUT CONTOURS
# ============================================================================

cat("\nCreating clean binned plots without contour lines...\n")

# Sunset plot - binned colors, no contours
p_sunset_base <- draw(sunset_model$gam,
    select = "ti(wind_max_gust,sum_butterflies_direct_sun)",
    residuals = TRUE, # Show data points
    rug = FALSE,
    contour = FALSE, # No contour lines
    n = 400, # Very high resolution grid
    continuous_fill = scale_fill_steps2(
        low = "#2166ac",
        mid = "white",
        high = "#b2182b",
        midpoint = 0,
        n.breaks = 11, # Creates 10 bins
        limits = c(-12, 12),
        name = "Partial\neffect",
        guide = guide_coloursteps(
            barwidth = 1.5,
            barheight = 12,
            title.position = "top"
        )
    )
)

# 24hr plot - binned colors, no contours
p_24hr_base <- draw(hr24_model$gam,
    select = "ti(wind_max_gust,sum_butterflies_direct_sun)",
    residuals = TRUE, # Show data points
    rug = FALSE,
    contour = FALSE, # No contour lines
    n = 400, # Very high resolution grid
    continuous_fill = scale_fill_steps2(
        low = "#2166ac",
        mid = "white",
        high = "#b2182b",
        midpoint = 0,
        n.breaks = 11, # Creates 10 bins
        limits = c(-12, 12),
        name = "Partial\neffect",
        guide = guide_coloursteps(
            barwidth = 1.5,
            barheight = 12,
            title.position = "top"
        )
    )
)

# Customize sunset plot with actual data points
p_sunset_clean <- p_sunset_base[[1]] +
    # Add actual data points without jitter
    geom_point(
        data = sunset_model_data,
        aes(x = wind_max_gust, y = sum_butterflies_direct_sun),
        color = "black", size = 0.8, alpha = 0.25, inherit.aes = FALSE
    ) +
    labs(
        title = "Sunset Window (Max Count → Sunset)",
        subtitle = sprintf(
            "n = %d observations, mean duration = %.1f hours | AIC = %.1f",
            nrow(sunset_model_data),
            mean(sunset_model_data$lag_duration_hours),
            AIC(sunset_model$lme)
        ),
        x = "Wind max gust (m/s)",
        y = "Sum butterflies in direct sun"
    ) +
    theme_minimal(base_size = 11) +
    theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        panel.grid = element_line(color = "gray95", linewidth = 0.2),
        panel.background = element_rect(fill = NA, color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "right",
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 10)
    )

# Customize 24hr plot with actual data points
p_24hr_clean <- p_24hr_base[[1]] +
    # Add actual data points without jitter
    geom_point(
        data = hr24_model_data,
        aes(x = wind_max_gust, y = sum_butterflies_direct_sun),
        color = "black", size = 0.8, alpha = 0.25, inherit.aes = FALSE
    ) +
    labs(
        title = "24-Hour Window (Max Count → +24 hours)",
        subtitle = sprintf(
            "n = %d observations, fixed 24-hour duration | AIC = %.1f",
            nrow(hr24_model_data),
            AIC(hr24_model$lme)
        ),
        x = "Wind max gust (m/s)",
        y = "Sum butterflies in direct sun"
    ) +
    theme_minimal(base_size = 11) +
    theme(
        plot.title = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 10, color = "gray40"),
        panel.grid = element_line(color = "gray95", linewidth = 0.2),
        panel.background = element_rect(fill = NA, color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "right",
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 10)
    )

# Combine plots vertically
p_combined_clean <- p_sunset_clean / p_24hr_clean +
    plot_annotation(
        title = "Wind × Sun Interaction Effects on Butterfly Aggregation",
        subtitle = "Binned visualization: Blue = decreased aggregation | White = no effect | Red = increased aggregation",
        caption = "Gray areas indicate regions without data support",
        theme = theme(
            plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
            plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
            plot.caption = element_text(size = 10, hjust = 0.5, color = "gray50")
        )
    )

ggsave(
    here("analysis", "dynamic_window_analysis", "figures", "gratia_binned_smooth.png"),
    plot = p_combined_clean,
    width = 9,
    height = 11,
    dpi = 300,
    bg = "white"
)

# ============================================================================
# ALSO CREATE SIDE-BY-SIDE VERSION
# ============================================================================

cat("\nCreating side-by-side version...\n")

p_sunset_horiz <- p_sunset_clean +
    theme(legend.position = "none") +
    labs(subtitle = sprintf(
        "n = %d, mean = %.1f hrs",
        nrow(sunset_model_data),
        mean(sunset_model_data$lag_duration_hours)
    ))

p_24hr_horiz <- p_24hr_clean +
    labs(subtitle = sprintf("n = %d, fixed 24 hrs", nrow(hr24_model_data)))

p_horizontal_clean <- (p_sunset_horiz | p_24hr_horiz) +
    plot_annotation(
        title = "Comparison of Window Approaches: Wind × Sun Interaction",
        subtitle = "Clean binned visualization without contour lines",
        theme = theme(
            plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
            plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40")
        )
    )

ggsave(
    here("analysis", "dynamic_window_analysis", "figures", "gratia_binned_smooth_horizontal.png"),
    plot = p_horizontal_clean,
    width = 12,
    height = 6,
    dpi = 300,
    bg = "white"
)

cat("\n✓ Created smooth binned plots (higher resolution, no contours):\n")
cat("  - gratia_binned_smooth.png (Vertical layout)\n")
cat("  - gratia_binned_smooth_horizontal.png (Side-by-side layout)\n")
