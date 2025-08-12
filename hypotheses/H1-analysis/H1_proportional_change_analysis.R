# H1 Analysis: Proportional Change in Monarch Abundance
# Testing the hypothesis that wind exposure causes monarchs to abandon roosts
# Response: (New Count - Original Count) / Original Count

# Setup ----
library(tidyverse)
library(lubridate)
library(glmmTMB)
library(performance)
library(broom.mixed)
library(DHARMa)
library(here)
library(corrplot)

# Set random seed for reproducibility
set.seed(42)

# Create results directory if it doesn't exist
results_dir <- here("results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# Load prepared data ----
cat("Loading prepared 30-min interval data...\n")
data_path <- here("results", "H1_interval_30min_terms_prepared.rds")

if (!file.exists(data_path)) {
  stop("Prepared data not found. Please run interval_30min_terms.qmd first to generate the data.")
}

df <- readr::read_rds(data_path)
cat("Loaded", nrow(df), "observations from", length(unique(df$deployment_id)), "deployments\n")

# Calculate proportional change ----
cat("\nCalculating proportional change in abundance...\n")
df <- df %>%
  mutate(
    # Proportional change: (New Count - Original Count) / Original Count
    prop_change = (abundance_index_t - abundance_index_t_minus_1) / abundance_index_t_minus_1,
    
    # Handle division by zero cases (when t-1 = 0)
    prop_change = case_when(
      abundance_index_t_minus_1 == 0 & abundance_index_t == 0 ~ 0,  # 0 to 0 = no change
      abundance_index_t_minus_1 == 0 & abundance_index_t > 0 ~ Inf, # 0 to positive = infinite increase
      TRUE ~ prop_change
    ),
    
    # Create a finite version for modeling (cap infinite values)
    prop_change_finite = case_when(
      is.infinite(prop_change) ~ 10,  # Cap at 1000% increase
      TRUE ~ prop_change
    ),
    
    # Categorize change direction
    change_direction = case_when(
      prop_change > 0 ~ "increase",
      prop_change < 0 ~ "decrease", 
      prop_change == 0 ~ "no_change",
      TRUE ~ "infinite_increase"
    )
  )

# Data exploration ----
cat("\nData exploration:\n")

# Summary of proportional changes
change_summary <- df %>%
  summarise(
    n = n(),
    mean_prop_change = mean(prop_change_finite, na.rm = TRUE),
    median_prop_change = median(prop_change_finite, na.rm = TRUE),
    sd_prop_change = sd(prop_change_finite, na.rm = TRUE),
    min_prop_change = min(prop_change_finite, na.rm = TRUE),
    max_prop_change = max(prop_change_finite, na.rm = TRUE),
    n_increase = sum(change_direction == "increase", na.rm = TRUE),
    n_decrease = sum(change_direction == "decrease", na.rm = TRUE),
    n_no_change = sum(change_direction == "no_change", na.rm = TRUE),
    n_infinite = sum(change_direction == "infinite_increase", na.rm = TRUE)
  )

print(change_summary)

# Distribution of changes by deployment
cat("\nChange patterns by deployment:\n")
deployment_summary <- df %>%
  group_by(deployment_id, view_id) %>%
  summarise(
    n_obs = n(),
    mean_prop_change = mean(prop_change_finite, na.rm = TRUE),
    prop_decreases = mean(change_direction == "decrease", na.rm = TRUE),
    prop_increases = mean(change_direction == "increase", na.rm = TRUE),
    mean_sustained_wind = mean(sustained_minutes_above_2ms, na.rm = TRUE),
    mean_gust_wind = mean(gust_minutes_above_2ms, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_decreases))

print(head(deployment_summary, 10))

# Center and scale predictors for modeling ----
cat("\nPreparing predictors for modeling...\n")
df <- df %>%
  mutate(
    # Center continuous predictors
    sustained_wind_centered = scale(sustained_minutes_above_2ms, center = TRUE, scale = FALSE)[,1],
    gust_wind_centered = scale(gust_minutes_above_2ms, center = TRUE, scale = FALSE)[,1],
    ambient_temp_centered = scale(ambient_temp, center = TRUE, scale = FALSE)[,1],
    sunlight_centered = scale(sunlight_exposure_prop, center = TRUE, scale = FALSE)[,1]
  ) %>%
  # Remove rows with missing key variables for modeling
  filter(
    !is.na(prop_change_finite),
    !is.na(sustained_minutes_above_2ms),
    !is.na(gust_minutes_above_2ms)
  )

cat("After filtering for complete cases:", nrow(df), "observations remain\n")

# Exploratory plots ----
cat("\nCreating exploratory plots...\n")

# Distribution of proportional changes
p1 <- ggplot(df, aes(x = prop_change_finite)) +
  geom_histogram(bins = 50, alpha = 0.7) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Distribution of Proportional Changes in Monarch Abundance",
       x = "Proportional Change", y = "Count") +
  theme_minimal()

ggsave(here(results_dir, "H1_prop_change_distribution.png"), p1, width = 8, height = 6)

# Wind effects on proportional change
p2 <- ggplot(df, aes(x = sustained_minutes_above_2ms, y = prop_change_finite)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Sustained Wind Minutes vs Proportional Change",
       x = "Minutes with Sustained Wind > 2 m/s", y = "Proportional Change") +
  theme_minimal()

ggsave(here(results_dir, "H1_sustained_wind_effect.png"), p2, width = 8, height = 6)

p3 <- ggplot(df, aes(x = gust_minutes_above_2ms, y = prop_change_finite)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Gust Wind Minutes vs Proportional Change",
       x = "Minutes with Gusts > 2 m/s", y = "Proportional Change") +
  theme_minimal()

ggsave(here(results_dir, "H1_gust_wind_effect.png"), p3, width = 8, height = 6)

# Statistical Models ----
cat("\nFitting statistical models...\n")

# Model 1: Simple linear model with wind predictors only
cat("Model 1: Linear model with wind predictors\n")
m1 <- lm(prop_change_finite ~ sustained_wind_centered + gust_wind_centered, 
         data = df)
summary(m1)

# Model 2: Linear mixed model with random effects for deployment/view
cat("Model 2: Linear mixed model with random intercepts\n")
m2 <- glmmTMB(prop_change_finite ~ sustained_wind_centered + gust_wind_centered + 
              (1|deployment_id) + (1|view_id_factor), 
              data = df)
summary(m2)

# Model 3: Add temperature and sunlight
cat("Model 3: Full model with all predictors\n")
df_complete <- df %>%
  filter(!is.na(ambient_temp_centered), !is.na(sunlight_centered))

cat("Complete cases for full model:", nrow(df_complete), "observations\n")

if (nrow(df_complete) > 50) {  # Only fit if sufficient data
  m3 <- glmmTMB(prop_change_finite ~ sustained_wind_centered + gust_wind_centered + 
                ambient_temp_centered + sunlight_centered +
                (1|deployment_id) + (1|view_id_factor), 
                data = df_complete)
  summary(m3)
} else {
  cat("Insufficient complete cases for full model\n")
  m3 <- NULL
}

# Model diagnostics ----
cat("\nModel diagnostics...\n")

# Check model assumptions for the mixed model
if (!is.null(m2)) {
  # Residual plots
  png(here(results_dir, "H1_model_diagnostics.png"), width = 12, height = 8, units = "in", res = 300)
  par(mfrow = c(2, 2))
  
  # QQ plot of residuals
  qqnorm(residuals(m2))
  qqline(residuals(m2))
  title("Q-Q Plot of Residuals")
  
  # Residuals vs fitted
  plot(fitted(m2), residuals(m2))
  abline(h = 0, col = "red")
  title("Residuals vs Fitted")
  
  # Histogram of residuals
  hist(residuals(m2), main = "Distribution of Residuals")
  
  # Residuals vs wind predictor
  plot(df$sustained_wind_centered, residuals(m2))
  abline(h = 0, col = "red")
  title("Residuals vs Sustained Wind")
  
  dev.off()
}

# Model comparison and results ----
cat("\nModel comparison:\n")

# Extract model coefficients and confidence intervals
if (!is.null(m2)) {
  m2_summary <- broom.mixed::tidy(m2, conf.int = TRUE, effects = "fixed")
  print(m2_summary)
  
  # Save results
  write_csv(m2_summary, here(results_dir, "H1_model_results.csv"))
}

if (!is.null(m3)) {
  m3_summary <- broom.mixed::tidy(m3, conf.int = TRUE, effects = "fixed")
  print(m3_summary)
  
  write_csv(m3_summary, here(results_dir, "H1_full_model_results.csv"))
}

# Summary interpretation ----
cat("\n=== ANALYSIS SUMMARY ===\n")
cat("Research Question: Do higher wind speeds cause proportional decreases in monarch abundance?\n")
cat("Response Variable: Proportional change = (New Count - Original Count) / Original Count\n")
cat("Key Predictors: Minutes with sustained winds > 2 m/s, Minutes with gusts > 2 m/s\n")
cat("\nKey Findings:\n")

if (!is.null(m2)) {
  sustained_coef <- m2_summary$estimate[m2_summary$term == "sustained_wind_centered"]
  gust_coef <- m2_summary$estimate[m2_summary$term == "gust_wind_centered"]
  
  cat("- Sustained wind effect:", round(sustained_coef, 4), "\n")
  cat("- Gust wind effect:", round(gust_coef, 4), "\n")
  
  if (sustained_coef < 0) {
    cat("- Sustained winds show NEGATIVE effect (supporting hypothesis)\n")
  } else {
    cat("- Sustained winds show POSITIVE effect (contrary to hypothesis)\n")
  }
  
  if (gust_coef < 0) {
    cat("- Gusts show NEGATIVE effect (supporting hypothesis)\n")
  } else {
    cat("- Gusts show POSITIVE effect (contrary to hypothesis)\n")
  }
}

cat("\nFiles created:\n")
cat("- H1_prop_change_distribution.png: Distribution of proportional changes\n")
cat("- H1_sustained_wind_effect.png: Scatterplot of sustained wind effects\n") 
cat("- H1_gust_wind_effect.png: Scatterplot of gust wind effects\n")
cat("- H1_model_diagnostics.png: Model assumption checks\n")
cat("- H1_model_results.csv: Statistical model results\n")
cat("\nAnalysis complete!\n")