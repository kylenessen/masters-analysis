# Wind Metrics Correlation Analysis
# This script creates correlation matrices specifically for wind variables
# from both 30-minute and sunset-to-sunset analyses

library(tidyverse)
library(corrplot)

# Set up output directory for figures
dir.create("analysis/figures", showWarnings = FALSE)

# Load the 30-minute dataset (using 24hr window as proxy for 30min analysis)
df_30min <- read.csv("data/monarch_daily_lag_analysis_24hr_window_filtered.csv")
cat("Loaded 30-minute dataset with", nrow(df_30min), "observations\n")

# Load the sunset dataset
df_sunset <- read.csv("data/monarch_daily_lag_analysis_sunset_window_filtered.csv")
cat("Loaded sunset dataset with", nrow(df_sunset), "observations\n")

# Define wind variables of interest
wind_vars <- c(
  "wind_avg_sustained",      # Average sustained wind speed
  "wind_max_gust",           # Maximum gust speed
  "wind_gust_sum",           # Sum of all gusts
  "wind_gust_sum_above_2ms", # Sum of gusts above 2 m/s
  "wind_gust_hours",         # Hours with gusts
  "wind_minutes_above_2ms",  # Minutes with wind > 2 m/s
  "wind_gust_sd",           # Standard deviation of gusts
  "wind_mode_gust"          # Mode of gust speeds
)

# Function to create and save correlation matrix
create_wind_correlation_matrix <- function(df, analysis_name, dataset_label) {

  # Select only wind variables that exist in the dataset
  available_vars <- intersect(wind_vars, names(df))

  if (length(available_vars) < 2) {
    cat("Not enough wind variables found in", dataset_label, "dataset\n")
    return(NULL)
  }

  # Extract wind data
  wind_data <- df[, available_vars]

  # Remove rows with all NAs
  wind_data <- wind_data[rowSums(is.na(wind_data)) < ncol(wind_data), ]

  cat("\n", dataset_label, "Analysis:\n")
  cat("- Wind variables found:", paste(available_vars, collapse=", "), "\n")
  cat("- Valid observations:", nrow(wind_data), "\n")

  # Calculate correlation matrix (using pairwise complete observations)
  cor_matrix <- cor(wind_data, use = "pairwise.complete.obs")

  # Calculate p-values for correlations
  cor_test_matrix <- matrix(NA, ncol = ncol(cor_matrix), nrow = nrow(cor_matrix))
  for(i in 1:ncol(wind_data)) {
    for(j in 1:ncol(wind_data)) {
      if(i != j) {
        test_result <- cor.test(wind_data[,i], wind_data[,j], use = "complete.obs")
        cor_test_matrix[i,j] <- test_result$p.value
      }
    }
  }

  # Create nice variable labels
  var_labels <- c(
    "wind_avg_sustained" = "Avg Sustained",
    "wind_max_gust" = "Max Gust",
    "wind_gust_sum" = "Gust Sum",
    "wind_gust_sum_above_2ms" = "Gust Sum >2m/s",
    "wind_gust_hours" = "Gust Hours",
    "wind_minutes_above_2ms" = "Minutes >2m/s",
    "wind_gust_sd" = "Gust SD",
    "wind_mode_gust" = "Mode Gust"
  )

  # Update row and column names
  rownames(cor_matrix) <- var_labels[rownames(cor_matrix)]
  colnames(cor_matrix) <- var_labels[colnames(cor_matrix)]

  # Print correlation matrix
  cat("\nCorrelation Matrix:\n")
  print(round(cor_matrix, 3))

  # Create visualization using corrplot
  png(paste0("analysis/figures/wind_correlation_", analysis_name, ".png"),
      width = 10, height = 10, units = "in", res = 300)

  corrplot(cor_matrix,
           method = "color",
           type = "upper",
           order = "hclust",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45,
           tl.cex = 1.2,
           cl.cex = 1,
           number.cex = 0.9,
           col = colorRampPalette(c("#053061", "#2166AC", "#4393C3",
                                   "#92C5DE", "#D1E5F0", "#F7F7F7",
                                   "#FDDBC7", "#F4A582", "#D6604D",
                                   "#B2182B", "#67001F"))(200),
           title = paste(dataset_label, "- Wind Metrics Correlation Matrix"),
           mar = c(0, 0, 2, 0))

  dev.off()

  # Create alternative heatmap visualization using ggplot2
  cor_df <- as.data.frame(as.table(cor_matrix))
  names(cor_df) <- c("Var1", "Var2", "Correlation")

  p <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3.5) +
    scale_fill_gradient2(low = "#053061", mid = "white", high = "#67001F",
                        midpoint = 0, limit = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9)) +
    labs(title = paste(dataset_label, "- Wind Metrics Correlation Matrix"),
         x = "", y = "") +
    coord_fixed()

  ggsave(paste0("analysis/figures/wind_correlation_", analysis_name, "_gg.png"),
         plot = p, width = 10, height = 10, dpi = 300)

  # Print summary statistics
  cat("\nCorrelation Summary:\n")
  cat("- Minimum correlation:", round(min(cor_matrix[upper.tri(cor_matrix)]), 3), "\n")
  cat("- Maximum correlation:", round(max(cor_matrix[upper.tri(cor_matrix)]), 3), "\n")
  cat("- Mean absolute correlation:", round(mean(abs(cor_matrix[upper.tri(cor_matrix)])), 3), "\n")

  # Identify highly correlated pairs (|r| > 0.7)
  high_cor <- which(abs(cor_matrix) > 0.7 & upper.tri(cor_matrix), arr.ind = TRUE)
  if(nrow(high_cor) > 0) {
    cat("\nHighly correlated pairs (|r| > 0.7):\n")
    for(i in 1:nrow(high_cor)) {
      cat(sprintf("  %s vs %s: r = %.3f\n",
                  rownames(cor_matrix)[high_cor[i, 1]],
                  colnames(cor_matrix)[high_cor[i, 2]],
                  cor_matrix[high_cor[i, 1], high_cor[i, 2]]))
    }
  }

  return(cor_matrix)
}

# Create correlation matrices for both analyses
cat("\n========================================\n")
cat("Creating Wind Correlation Matrices\n")
cat("========================================\n")

cor_30min <- create_wind_correlation_matrix(df_30min, "30min", "30-Minute Interval")
cor_sunset <- create_wind_correlation_matrix(df_sunset, "sunset", "Sunset-to-Sunset")

# Save correlation matrices as CSV for reference
if(!is.null(cor_30min)) {
  write.csv(round(cor_30min, 3),
            "analysis/figures/wind_correlation_30min_matrix.csv",
            row.names = TRUE)
}

if(!is.null(cor_sunset)) {
  write.csv(round(cor_sunset, 3),
            "analysis/figures/wind_correlation_sunset_matrix.csv",
            row.names = TRUE)
}

# Create a comparison of key wind metrics between the two analyses
cat("\n========================================\n")
cat("Comparison of Wind Metrics\n")
cat("========================================\n")

# Extract key wind variable for comparison (e.g., max gust)
if("wind_max_gust" %in% names(df_30min) & "wind_max_gust" %in% names(df_sunset)) {

  # Summary statistics
  cat("\nMax Wind Gust Statistics:\n")
  cat("\n30-Minute Analysis:\n")
  cat("  Mean:", round(mean(df_30min$wind_max_gust, na.rm = TRUE), 2), "m/s\n")
  cat("  SD:", round(sd(df_30min$wind_max_gust, na.rm = TRUE), 2), "m/s\n")
  cat("  Range:", round(min(df_30min$wind_max_gust, na.rm = TRUE), 2), "-",
      round(max(df_30min$wind_max_gust, na.rm = TRUE), 2), "m/s\n")

  cat("\nSunset-to-Sunset Analysis:\n")
  cat("  Mean:", round(mean(df_sunset$wind_max_gust, na.rm = TRUE), 2), "m/s\n")
  cat("  SD:", round(sd(df_sunset$wind_max_gust, na.rm = TRUE), 2), "m/s\n")
  cat("  Range:", round(min(df_sunset$wind_max_gust, na.rm = TRUE), 2), "-",
      round(max(df_sunset$wind_max_gust, na.rm = TRUE), 2), "m/s\n")
}

cat("\n========================================\n")
cat("Analysis Complete!\n")
cat("Figures saved in: analysis/figures/\n")
cat("========================================\n")