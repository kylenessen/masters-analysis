#!/usr/bin/env Rscript
# Utility to create high-resolution, binned interaction plots for GAM tensor products

suppressPackageStartupMessages({
  library(ggplot2)
  library(gratia)
  library(grid)
})

# create_binned_interaction_plot
# - gam_model: a fitted mgcv GAM (e.g., from gamm(...)$gam)
# - x_var, y_var: variable names used in the tensor product term (character)
# - data: data.frame with columns x_var and y_var for overlaying points
# - title, subtitle, xlab, ylab: plot labeling
# - n: grid resolution for gratia::draw
# - nbreaks: number of color breaks (bins = nbreaks - 1)
# - low, mid, high: colors for scale_fill_steps2
# - limits: optional numeric vector length 2 for symmetric color limits
create_binned_interaction_plot <- function(
    gam_model,
    x_var,
    y_var,
    data,
    title = "",
    subtitle = NULL,
    xlab = NULL,
    ylab = NULL,
    n = 400,
    nbreaks = 11,
    low = "#2b83ba", # blue
    mid = "#ffffff", # white (near zero)
    high = "#d7191c", # red
    limits = NULL,
    breaks = NULL,
    labels = NULL,
    barheight = 16,
    barwidth = 1.4,
    legend_text_size = 9,
    legend_key_height_cm = 0.6,
    legend_name = expression(paste("Partial effect on ", Delta, "BI")),
    base_size = 14,
    # Distance-based mask for excluding far-away grid points
    # Default mirrors mgcv::vis.gam (0.1)
    too_far = 0.1,
    # Jitter controls for raw points overlay
    jitter_x = 0.15,
    jitter_y = 5,
    jitter_seed = NULL) {
  sel <- sprintf("ti(%s,%s)", x_var, y_var)

  # Build scale with optional limits
  # If custom breaks are supplied, they define step boundaries and legend ticks
  fill_scale <- scale_fill_steps2(
    low = low,
    mid = mid,
    high = high,
    midpoint = 0,
    n.breaks = nbreaks,
    limits = limits,
    breaks = breaks,
    labels = labels,
    name = legend_name,
    guide = guide_coloursteps(
      barwidth = barwidth,
      barheight = barheight,
      title.position = "top"
    )
  )

  # Draw high-res, binned surface without contours
  p_base <- draw(
    gam_model,
    select = sel,
    residuals = TRUE,
    rug = FALSE,
    contour = FALSE,
    n = n,
    continuous_fill = fill_scale,
    too_far = too_far
  )

  # Prepare jittered points without crossing the zero line on y
  data_pts <- data
  if (!is.null(jitter_seed)) set.seed(jitter_seed)
  if (!is.null(jitter_x) && jitter_x > 0) {
    data_pts[[x_var]] <- base::jitter(data[[x_var]], amount = jitter_x)
  }
  if (!is.null(jitter_y) && jitter_y > 0) {
    yj <- base::jitter(data[[y_var]], amount = jitter_y)
    # Prevent crossing below the zero line on y
    data_pts[[y_var]] <- pmax(0, yj)
  }

  # Compose final plot, overlay jittered data points
  p <- p_base[[1]] +
    geom_point(
      data = data_pts,
      aes(x = .data[[x_var]], y = .data[[y_var]]),
      color = "black", size = 0.3, alpha = 0.1, inherit.aes = FALSE
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = if (is.null(xlab)) x_var else xlab,
      y = if (is.null(ylab)) y_var else ylab
    ) +
    theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      panel.grid = element_line(color = "gray95", linewidth = 0.2),
      panel.background = element_rect(fill = NA, color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = legend_text_size),
      legend.key.height = unit(legend_key_height_cm, "cm"),
      legend.margin = margin(2, 4, 2, 4),
      axis.title = element_text(size = 10)
    )

  return(p)
}
