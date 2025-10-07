library(tidyverse)

# Read the pre-calculated sunset analysis data from Python
daily_max <- read_csv("max_count_timing_until_sunset_analysis_fixed.csv")

# Rename column for consistency
daily_max <- daily_max %>%
  rename(hours_until_sunset_at_max = hours_until_sunset)

# Create the histogram with clean, polished styling
p <- ggplot(daily_max, aes(x = hours_until_sunset_at_max)) +
  geom_histogram(
    binwidth = 0.5,
    fill = "#4A7BA7",  # Professional blue color
    color = "#2C5282",  # Darker blue for edges
    alpha = 0.85,
    linewidth = 0.5
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 2),
    limits = c(-0.25, 11),
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 10),
    breaks = seq(0, 10, by = 2)
  ) +
  labs(
    title = "Timing of Maximum Daily Butterfly Count Relative to Sunset",
    subtitle = paste0("Filtered Dataset: metrics_complete ≥ 0.95 (n = ", nrow(daily_max), " days)"),
    x = "Hours Until Sunset",
    y = "Number of Days"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # Title and subtitle
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5,
                             margin = margin(b = 8), color = "#1A202C"),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#4A5568",
                                 margin = margin(b = 20)),

    # Axis styling
    axis.title.x = element_text(size = 14, margin = margin(t = 10), color = "#2D3748"),
    axis.title.y = element_text(size = 14, margin = margin(r = 10), color = "#2D3748"),
    axis.text = element_text(size = 12, color = "#4A5568"),

    # Grid and background
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#E2E8F0", linewidth = 0.4),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),

    # Axis lines
    axis.line.x = element_line(color = "#718096", linewidth = 0.5),
    axis.line.y = element_blank(),
    axis.ticks.x = element_line(color = "#718096", linewidth = 0.4),
    axis.ticks.y = element_blank(),
    axis.ticks.length.x = unit(0.15, "cm"),

    # Margins
    plot.margin = margin(25, 25, 25, 25)
  )

# Display the plot
print(p)

# Save the plot
ggsave(
  "max_count_timing_until_sunset_histogram.png",
  plot = p,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

# Print summary statistics
cat("\n=== Summary Statistics ===\n")
cat("Number of days analyzed:", nrow(daily_max), "\n")
cat("Mean hours until sunset:", round(mean(daily_max$hours_until_sunset_at_max), 1), "\n")
cat("Median hours until sunset:", round(median(daily_max$hours_until_sunset_at_max), 1), "\n")
cat("Std Dev:", round(sd(daily_max$hours_until_sunset_at_max), 1), "\n")
cat("Range:", round(min(daily_max$hours_until_sunset_at_max), 1), "-",
    round(max(daily_max$hours_until_sunset_at_max), 1), "hours\n")