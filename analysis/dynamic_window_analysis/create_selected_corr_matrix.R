#!/usr/bin/env Rscript

library(tidyverse)
library(corrplot)

# Load the sunset window data
data <- read_csv("../../data/monarch_daily_lag_analysis_sunset_window.csv", show_col_types = FALSE)

# Selected predictors only
selected_predictors <- c(
  "temp_min",
  "temp_max",
  "temp_at_max_count_t_1",
  "wind_max_gust",
  "sum_butterflies_direct_sun"
)

# Create correlation matrix
cor_matrix <- cor(data[, selected_predictors], use = "complete.obs")

# Save as PNG
png("figures/sunset_correlation_selected.png", width = 800, height = 800, res = 120)
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "original",
         addCoef.col = "black",
         number.cex = 0.8,
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.9,
         cl.cex = 0.8,
         title = "Correlation Matrix: Selected Predictors (n=5)",
         mar = c(0, 0, 2, 0))
dev.off()

cat("Created figures/sunset_correlation_selected.png\n")
