# H1 Analysis: Improved Proportional Change Model
# Addressing distribution and model assumption issues

# Setup ----
library(tidyverse)
library(lubridate)
library(glmmTMB)
library(performance)
library(broom.mixed)
library(DHARMa)
library(here)
library(MASS)

set.seed(42)
results_dir <- here("results")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# Load prepared data ----
cat("Loading prepared 30-min interval data...\n")
data_path <- here("results", "H1_interval_30min_terms_prepared.rds")
df <- readr::read_rds(data_path)

# Improved proportional change calculation ----
cat("\nCalculating improved proportional change metrics...\n")
df <- df %>%
  mutate(
    # Original proportional change
    prop_change_raw = (abundance_index_t - abundance_index_t_minus_1) / abundance_index_t_minus_1,
    
    # Alternative metrics that handle the distribution issues better:
    
    # 1. Log ratio (more symmetric, handles zeros better)
    log_ratio = log((abundance_index_t + 1) / (abundance_index_t_minus_1 + 1)),
    
    # 2. Absolute change
    absolute_change = abundance_index_t - abundance_index_t_minus_1,
    
    # 3. Relative change with better handling of zeros
    relative_change = case_when(
      abundance_index_t_minus_1 == 0 & abundance_index_t == 0 ~ 0,
      abundance_index_t_minus_1 == 0 & abundance_index_t > 0 ~ 1,  # Appearance
      abundance_index_t_minus_1 > 0 & abundance_index_t == 0 ~ -1, # Disappearance  
      TRUE ~ (abundance_index_t - abundance_index_t_minus_1) / (abundance_index_t_minus_1 + abundance_index_t)
    ),
    
    # 4. Categorical change direction
    change_category = case_when(
      abundance_index_t > abundance_index_t_minus_1 ~ "increase",
      abundance_index_t < abundance_index_t_minus_1 ~ "decrease",
      TRUE ~ "no_change"
    ),
    
    # 5. Winsorized proportional change (cap extreme values)
    prop_change_winsorized = case_when(
      abundance_index_t_minus_1 == 0 & abundance_index_t == 0 ~ 0,
      abundance_index_t_minus_1 == 0 & abundance_index_t > 0 ~ 2,  # Cap at 200% increase
      TRUE ~ pmax(-1, pmin(2, (abundance_index_t - abundance_index_t_minus_1) / abundance_index_t_minus_1))
    )
  )

# Center and scale predictors ----
df <- df %>%
  mutate(
    sustained_wind_centered = scale(sustained_minutes_above_2ms, center = TRUE, scale = FALSE)[,1],
    gust_wind_centered = scale(gust_minutes_above_2ms, center = TRUE, scale = FALSE)[,1],
    ambient_temp_centered = scale(ambient_temp, center = TRUE, scale = FALSE)[,1],
    sunlight_centered = scale(sunlight_exposure_prop, center = TRUE, scale = FALSE)[,1]
  ) %>%
  filter(
    !is.na(sustained_minutes_above_2ms),
    !is.na(gust_minutes_above_2ms)
  )

cat("After filtering:", nrow(df), "observations remain\n")

# Compare distributions ----
cat("\nComparing response variable distributions...\n")

# Create comparison plots
p_dist <- df %>%
  dplyr::select(prop_change_raw, log_ratio, relative_change, prop_change_winsorized, absolute_change) %>%
  # Remove infinite values for plotting
  mutate(prop_change_raw = ifelse(is.infinite(prop_change_raw), NA, prop_change_raw)) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value") %>%
  filter(!is.na(value)) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 50, alpha = 0.7) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  facet_wrap(~metric, scales = "free", ncol = 2) +
  labs(title = "Distribution Comparison of Different Change Metrics",
       x = "Value", y = "Count") +
  theme_minimal()

ggsave(here(results_dir, "H1_metric_comparison.png"), p_dist, width = 12, height = 10)

# Model comparison with different response variables ----
cat("\nFitting models with different response variables...\n")

# Complete cases for full models
df_complete <- df %>%
  filter(!is.na(ambient_temp_centered), !is.na(sunlight_centered))

cat("Complete cases for full models:", nrow(df_complete), "observations\n")

# Model 1: Log ratio (most robust to distribution issues)
cat("Model 1: Log ratio response\n")
m1_log <- glmmTMB(log_ratio ~ sustained_wind_centered + gust_wind_centered + 
                  ambient_temp_centered + sunlight_centered +
                  (1|deployment_id) + (1|view_id_factor), 
                  data = df_complete)

# Model 2: Winsorized proportional change  
cat("Model 2: Winsorized proportional change\n")
m2_wins <- glmmTMB(prop_change_winsorized ~ sustained_wind_centered + gust_wind_centered + 
                   ambient_temp_centered + sunlight_centered +
                   (1|deployment_id) + (1|view_id_factor), 
                   data = df_complete)

# Model 3: Relative change (bounded between -1 and 1)
cat("Model 3: Relative change\n") 
m3_rel <- glmmTMB(relative_change ~ sustained_wind_centered + gust_wind_centered + 
                  ambient_temp_centered + sunlight_centered +
                  (1|deployment_id) + (1|view_id_factor), 
                  data = df_complete)

# Model 4: Absolute change (might be more interpretable)
cat("Model 4: Absolute change\n")
m4_abs <- glmmTMB(absolute_change ~ sustained_wind_centered + gust_wind_centered + 
                  ambient_temp_centered + sunlight_centered +
                  (1|deployment_id) + (1|view_id_factor), 
                  data = df_complete)

# Model 5: Categorical model (binomial)
cat("Model 5: Categorical decrease vs increase/no change\n")
df_complete <- df_complete %>%
  mutate(decreased = ifelse(change_category == "decrease", 1, 0))

m5_cat <- glmmTMB(decreased ~ sustained_wind_centered + gust_wind_centered + 
                  ambient_temp_centered + sunlight_centered +
                  (1|deployment_id) + (1|view_id_factor), 
                  family = binomial,
                  data = df_complete)

# Model diagnostics comparison ----
cat("\nModel diagnostics comparison...\n")

# DHARMa residuals for all models
models <- list(
  "Log_Ratio" = m1_log,
  "Winsorized" = m2_wins, 
  "Relative" = m3_rel,
  "Absolute" = m4_abs,
  "Categorical" = m5_cat
)

# Create diagnostic plots
png(here(results_dir, "H1_improved_diagnostics.png"), width = 15, height = 12, units = "in", res = 300)
par(mfrow = c(3, 5))

for (i in 1:length(models)) {
  model_name <- names(models)[i]
  model <- models[[i]]
  
  # DHARMa residuals
  sim_res <- simulateResiduals(model, plot = FALSE)
  
  # QQ plot
  qqnorm(residuals(sim_res), main = paste(model_name, "- DHARMa QQ"))
  qqline(residuals(sim_res))
  
  # Residuals vs fitted
  plot(fitted(model), residuals(sim_res), 
       main = paste(model_name, "- Res vs Fitted"),
       xlab = "Fitted", ylab = "DHARMa Residuals")
  abline(h = 0, col = "red")
  
  # Residuals vs wind
  plot(df_complete$sustained_wind_centered, residuals(sim_res),
       main = paste(model_name, "- Res vs Wind"),
       xlab = "Sustained Wind", ylab = "DHARMa Residuals")
  abline(h = 0, col = "red")
}

dev.off()

# Model comparison table ----
cat("\nModel comparison summary...\n")

model_comparison <- tibble(
  Model = names(models),
  AIC = map_dbl(models, AIC),
  Response = c("Log ratio", "Winsorized prop change", "Relative change", "Absolute change", "Categorical")
) %>%
  arrange(AIC)

print(model_comparison)

# Extract results from best models ----
cat("\nExtracting results from promising models...\n")

# Log ratio model (likely best for symmetric distribution)
log_results <- broom.mixed::tidy(m1_log, conf.int = TRUE, effects = "fixed")
cat("Log ratio model results:\n")
print(log_results)

# Categorical model (most interpretable for hypothesis testing)
cat_results <- broom.mixed::tidy(m5_cat, conf.int = TRUE, effects = "fixed", exponentiate = TRUE)
cat("Categorical model results (odds ratios):\n") 
print(cat_results)

# Save results
write_csv(log_results, here(results_dir, "H1_log_ratio_results.csv"))
write_csv(cat_results, here(results_dir, "H1_categorical_results.csv"))
write_csv(model_comparison, here(results_dir, "H1_model_comparison.csv"))

# Wind effect visualization for best model ----
cat("\nCreating improved effect plots...\n")

# Log ratio effects
p_log_wind <- ggplot(df_complete, aes(x = sustained_minutes_above_2ms, y = log_ratio)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Log Ratio Change vs Sustained Wind",
       subtitle = "Log((Count_t + 1) / (Count_t-1 + 1))",
       x = "Minutes with Sustained Wind > 2 m/s", 
       y = "Log Ratio Change",
       caption = "0 = no change, positive = increase, negative = decrease") +
  theme_minimal()

ggsave(here(results_dir, "H1_log_ratio_wind_effect.png"), p_log_wind, width = 10, height = 7)

# Categorical probability plot
prob_data <- df_complete %>%
  mutate(wind_bin = cut(sustained_minutes_above_2ms, breaks = 5)) %>%
  group_by(wind_bin) %>%
  summarise(
    n = n(),
    prop_decrease = mean(decreased),
    se = sqrt(prop_decrease * (1 - prop_decrease) / n),
    .groups = "drop"
  ) %>%
  filter(!is.na(wind_bin))

p_prob <- ggplot(prob_data, aes(x = wind_bin, y = prop_decrease)) +
  geom_col(alpha = 0.7, fill = "steelblue") +
  geom_errorbar(aes(ymin = prop_decrease - se, ymax = prop_decrease + se), 
                width = 0.2) +
  labs(title = "Probability of Decrease by Wind Exposure",
       x = "Sustained Wind Minutes (binned)", 
       y = "Proportion of Intervals with Decreases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(here(results_dir, "H1_decrease_probability.png"), p_prob, width = 8, height = 6)

# Summary interpretation ----
cat("\n=== IMPROVED ANALYSIS SUMMARY ===\n")
cat("Research Question: Do higher wind speeds cause decreases in monarch abundance?\n")
cat("\nResponse Variables Tested:\n")
cat("1. Log ratio: log((Count_t + 1) / (Count_t-1 + 1)) - most robust\n")
cat("2. Winsorized prop change: Capped proportional change (-100% to +200%)\n") 
cat("3. Relative change: Normalized change bounded between -1 and 1\n")
cat("4. Absolute change: Simple difference in counts\n")
cat("5. Categorical: Probability of decrease (most interpretable)\n")

cat("\nModel Comparison (by AIC):\n")
print(model_comparison)

# Interpret wind effects
sustained_coef_log <- log_results$estimate[log_results$term == "sustained_wind_centered"]
sustained_pval_log <- log_results$p.value[log_results$term == "sustained_wind_centered"]

sustained_or_cat <- cat_results$estimate[cat_results$term == "sustained_wind_centered"]
sustained_pval_cat <- cat_results$p.value[cat_results$term == "sustained_wind_centered"]

cat("\nKey Wind Effect Findings:\n")
cat("Log Ratio Model - Sustained Wind Coefficient:", round(sustained_coef_log, 4), 
    "(p =", round(sustained_pval_log, 3), ")\n")
cat("Categorical Model - Sustained Wind OR:", round(sustained_or_cat, 3), 
    "(p =", round(sustained_pval_cat, 3), ")\n")

if (sustained_or_cat > 1) {
  cat("- Higher wind associated with INCREASED probability of decrease (supporting hypothesis)\n")
} else {
  cat("- Higher wind associated with DECREASED probability of decrease (contrary to hypothesis)\n")
}

cat("\nFiles created:\n")
cat("- H1_metric_comparison.png: Distribution comparison of different metrics\n")
cat("- H1_improved_diagnostics.png: Model diagnostics for all approaches\n")
cat("- H1_log_ratio_wind_effect.png: Wind effects using log ratio\n")
cat("- H1_decrease_probability.png: Probability of decrease by wind exposure\n")
cat("- H1_model_comparison.csv: AIC comparison of all models\n")
cat("- H1_log_ratio_results.csv: Detailed results for log ratio model\n")
cat("- H1_categorical_results.csv: Odds ratios for categorical model\n")
cat("\nImproved analysis complete!\n")