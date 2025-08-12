# Comprehensive comparison of different response variable transformations
# Focus on model diagnostics, not AIC comparison

library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(performance)
library(here)

set.seed(42)

# Create output directory
results_dir <- here("results", "H1_transformation_comparison")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# Load data
cat("Loading data...\n")
df <- readr::read_rds(here("results", "H1_interval_30min_terms_prepared.rds"))

# Calculate all transformations
cat("Calculating response variable transformations...\n")
df <- df %>%
  mutate(
    # 1. Absolute change
    absolute_change = abundance_index_t - abundance_index_t_minus_1,
    
    # 2. Relative change 
    relative_change = case_when(
      abundance_index_t_minus_1 == 0 & abundance_index_t == 0 ~ 0,
      abundance_index_t_minus_1 == 0 ~ 1,
      abundance_index_t == 0 ~ -1,
      TRUE ~ (abundance_index_t - abundance_index_t_minus_1) / 
             (abundance_index_t + abundance_index_t_minus_1)
    ),
    
    # 3. Log ratio
    log_ratio = log((abundance_index_t + 1) / (abundance_index_t_minus_1 + 1)),
    
    # 4. Proportional change (with handling of edge cases)
    prop_change = case_when(
      abundance_index_t_minus_1 == 0 ~ NA_real_,
      TRUE ~ (abundance_index_t - abundance_index_t_minus_1) / abundance_index_t_minus_1
    ),
    
    # 5. Winsorized proportional change
    prop_change_wins = case_when(
      is.na(prop_change) ~ NA_real_,
      prop_change > 2 ~ 2,
      prop_change < -1 ~ -1,
      TRUE ~ prop_change
    ),
    
    # 6. Square root transformation of absolute change
    sqrt_abs_change = sign(absolute_change) * sqrt(abs(absolute_change)),
    
    # 7. Arcsinh transformation (like log but handles negatives and zeros)
    arcsinh_change = asinh((abundance_index_t - abundance_index_t_minus_1) / 
                           (abundance_index_t_minus_1 + 1))
  ) %>%
  # Prepare predictors
  mutate(
    sustained_wind_c = scale(sustained_minutes_above_2ms, center = TRUE, scale = FALSE)[,1],
    gust_wind_c = scale(gust_minutes_above_2ms, center = TRUE, scale = FALSE)[,1],
    ambient_temp_c = scale(ambient_temp, center = TRUE, scale = FALSE)[,1],
    sunlight_c = scale(sunlight_exposure_prop, center = TRUE, scale = FALSE)[,1],
    view_id = as.factor(view_id),
    labeler = as.factor(labeler)
  ) %>%
  # Filter to complete cases
  filter(
    !is.na(sustained_wind_c),
    !is.na(gust_wind_c),
    !is.na(ambient_temp_c),
    !is.na(sunlight_c)
  )

cat("Complete cases for analysis:", nrow(df), "\n\n")

# Fit models with different transformations
cat("Fitting models with different response transformations...\n")

formula_base <- ~ sustained_wind_c + gust_wind_c + ambient_temp_c + sunlight_c + 
                  (1|view_id) + (1|labeler)

models <- list()

# 1. Relative change
cat("1. Relative change model...\n")
models$relative <- glmmTMB(
  relative_change ~ sustained_wind_c + gust_wind_c + ambient_temp_c + sunlight_c + 
                    (1|view_id) + (1|labeler),
  data = df
)

# 2. Log ratio
cat("2. Log ratio model...\n")
models$log_ratio <- glmmTMB(
  log_ratio ~ sustained_wind_c + gust_wind_c + ambient_temp_c + sunlight_c + 
              (1|view_id) + (1|labeler),
  data = df
)

# 3. Proportional change (winsorized)
cat("3. Winsorized proportional change model...\n")
df_prop <- df %>% filter(!is.na(prop_change_wins))
models$prop_wins <- glmmTMB(
  prop_change_wins ~ sustained_wind_c + gust_wind_c + ambient_temp_c + sunlight_c + 
                     (1|view_id) + (1|labeler),
  data = df_prop
)

# 4. Square root of absolute change
cat("4. Square root transformation model...\n")
models$sqrt_abs <- glmmTMB(
  sqrt_abs_change ~ sustained_wind_c + gust_wind_c + ambient_temp_c + sunlight_c + 
                    (1|view_id) + (1|labeler),
  data = df
)

# 5. Arcsinh transformation
cat("5. Arcsinh transformation model...\n")
models$arcsinh <- glmmTMB(
  arcsinh_change ~ sustained_wind_c + gust_wind_c + ambient_temp_c + sunlight_c + 
                   (1|view_id) + (1|labeler),
  data = df
)

# 6. Raw absolute change (for comparison)
cat("6. Raw absolute change model...\n")
models$absolute <- glmmTMB(
  absolute_change ~ sustained_wind_c + gust_wind_c + ambient_temp_c + sunlight_c + 
                    (1|view_id) + (1|labeler),
  data = df
)

# Create diagnostic plots for all models
cat("\n=== CREATING DIAGNOSTIC PLOTS ===\n")

# Function to create comprehensive diagnostics
create_diagnostics <- function(model, model_name, data_used) {
  cat("Diagnostics for:", model_name, "\n")
  
  # Create a subplot for this model
  png(here(results_dir, paste0("diagnostics_", model_name, ".png")), 
      width = 14, height = 10, units = "in", res = 150)
  
  par(mfrow = c(2, 4), mar = c(4, 4, 3, 2))
  
  # 1. Q-Q plot of raw residuals
  res <- residuals(model)
  qqnorm(res, main = paste(model_name, ": Q-Q Plot"))
  qqline(res, col = "red")
  
  # 2. Residuals vs Fitted
  plot(fitted(model), res,
       main = paste(model_name, ": Residuals vs Fitted"),
       xlab = "Fitted values", ylab = "Residuals")
  abline(h = 0, col = "red")
  lines(lowess(fitted(model), res), col = "blue", lwd = 2)
  
  # 3. Scale-Location plot
  sqrt_abs_res <- sqrt(abs(res))
  plot(fitted(model), sqrt_abs_res,
       main = paste(model_name, ": Scale-Location"),
       xlab = "Fitted values", ylab = "√|Residuals|")
  lines(lowess(fitted(model), sqrt_abs_res), col = "blue", lwd = 2)
  
  # 4. Histogram of residuals
  hist(res, breaks = 30, main = paste(model_name, ": Residual Distribution"),
       xlab = "Residuals", col = "lightblue")
  curve(dnorm(x, mean = mean(res), sd = sd(res)) * length(res) * diff(hist(res, plot = FALSE)$breaks)[1],
        add = TRUE, col = "red", lwd = 2)
  
  # 5. Residuals vs Wind predictor
  plot(data_used$sustained_wind_c, res,
       main = paste(model_name, ": Residuals vs Wind"),
       xlab = "Sustained Wind (centered)", ylab = "Residuals")
  abline(h = 0, col = "red")
  lines(lowess(data_used$sustained_wind_c, res), col = "blue", lwd = 2)
  
  # 6. Residuals vs Temperature
  plot(data_used$ambient_temp_c, res,
       main = paste(model_name, ": Residuals vs Temperature"),
       xlab = "Temperature (centered)", ylab = "Residuals")
  abline(h = 0, col = "red")
  lines(lowess(data_used$ambient_temp_c, res), col = "blue", lwd = 2)
  
  # 7. ACF plot for temporal autocorrelation
  acf(res, main = paste(model_name, ": Autocorrelation"))
  
  # 8. Response vs Fitted (to check linearity)
  response_var <- as.formula(model$call$formula)[[2]]
  response_vals <- data_used[[as.character(response_var)]]
  plot(fitted(model), response_vals,
       main = paste(model_name, ": Response vs Fitted"),
       xlab = "Fitted values", ylab = "Observed values")
  abline(0, 1, col = "red")
  
  dev.off()
  
  # Return diagnostic statistics
  return(list(
    shapiro_p = shapiro.test(sample(res, min(5000, length(res))))$p.value,
    residual_sd = sd(res),
    r_squared = cor(fitted(model), response_vals)^2
  ))
}

# Run diagnostics for each model
diagnostic_stats <- list()

diagnostic_stats$relative <- create_diagnostics(models$relative, "relative_change", df)
diagnostic_stats$log_ratio <- create_diagnostics(models$log_ratio, "log_ratio", df)
diagnostic_stats$prop_wins <- create_diagnostics(models$prop_wins, "prop_winsorized", df_prop)
diagnostic_stats$sqrt_abs <- create_diagnostics(models$sqrt_abs, "sqrt_absolute", df)
diagnostic_stats$arcsinh <- create_diagnostics(models$arcsinh, "arcsinh", df)
diagnostic_stats$absolute <- create_diagnostics(models$absolute, "absolute", df)

# DHARMa diagnostics for more sophisticated checks
cat("\n=== DHARMA DIAGNOSTICS ===\n")

dharma_results <- list()
for (model_name in names(models)) {
  cat("DHARMa diagnostics for:", model_name, "\n")
  
  # Simulate residuals
  sim_res <- simulateResiduals(models[[model_name]], n = 1000)
  
  # Create DHARMa diagnostic plots
  png(here(results_dir, paste0("dharma_", model_name, ".png")),
      width = 12, height = 8, units = "in", res = 150)
  
  par(mfrow = c(2, 3))
  
  # Standard DHARMa plots
  plotQQunif(sim_res, main = paste(model_name, ": DHARMa Q-Q"))
  plotResiduals(sim_res, main = paste(model_name, ": DHARMa Residuals"))
  
  # Test for outliers
  outlier_test <- testOutliers(sim_res)
  
  # Test for dispersion
  dispersion_test <- testDispersion(sim_res)
  
  # Test for zero-inflation (if applicable)
  zero_test <- testZeroInflation(sim_res)
  
  # Uniformity test
  ks_test <- testUniformity(sim_res)
  
  # Plot tests
  hist(sim_res$scaledResiduals, breaks = 20, main = paste(model_name, ": Scaled Residuals"))
  
  # Residuals by group (view_id)
  # Use the correct data frame for this model
  data_for_model <- if(model_name == "prop_wins") df_prop else df
  if ("view_id" %in% names(data_for_model)) {
    plotResiduals(sim_res, data_for_model$view_id, main = "Residuals by View")
  }
  
  dev.off()
  
  dharma_results[[model_name]] <- list(
    outliers_p = outlier_test$p.value,
    dispersion_p = dispersion_test$p.value,
    uniformity_p = ks_test$p.value,
    zero_inflation_p = zero_test$p.value
  )
}

# Compile comprehensive comparison
cat("\n=== COMPREHENSIVE COMPARISON ===\n")

comparison_df <- data.frame(
  Model = names(diagnostic_stats),
  Shapiro_p = sapply(diagnostic_stats, function(x) round(x$shapiro_p, 4)),
  Residual_SD = sapply(diagnostic_stats, function(x) round(x$residual_sd, 4)),
  R_squared = sapply(diagnostic_stats, function(x) round(x$r_squared, 4)),
  DHARMa_Outliers_p = sapply(dharma_results, function(x) round(x$outliers_p, 4)),
  DHARMa_Dispersion_p = sapply(dharma_results, function(x) round(x$dispersion_p, 4)),
  DHARMa_Uniformity_p = sapply(dharma_results, function(x) round(x$uniformity_p, 4))
)

print(comparison_df)

# Check for patterns in residuals
cat("\n=== RESIDUAL PATTERN ANALYSIS ===\n")

for (model_name in names(models)) {
  res <- residuals(models[[model_name]])
  
  # Get the correct data frame
  data_for_model <- if(model_name == "prop_wins") df_prop else df
  
  # Check correlation with predictors
  cor_sustained <- cor(res, data_for_model$sustained_wind_c)
  cor_temp <- cor(res, data_for_model$ambient_temp_c)
  
  # Simple heteroscedasticity check - correlation of squared residuals with fitted
  fitted_vals <- fitted(models[[model_name]])
  het_check <- cor(res^2, fitted_vals)
  
  cat("\n", model_name, ":\n")
  cat("  Heteroscedasticity (|r| > 0.1 suggests issues):", round(het_check, 4), "\n")
  cat("  Residual correlation with wind:", round(cor_sustained, 4), "\n")
  cat("  Residual correlation with temp:", round(cor_temp, 4), "\n")
}

# Extract and compare effect estimates
cat("\n=== EFFECT ESTIMATES COMPARISON ===\n")

effects_comparison <- bind_rows(
  lapply(names(models), function(model_name) {
    broom.mixed::tidy(models[[model_name]], effects = "fixed") %>%
      filter(term == "sustained_wind_c") %>%
      mutate(
        model = model_name,
        significant = p.value < 0.05
      ) %>%
      select(model, estimate, std.error, p.value, significant)
  })
)

print(effects_comparison)

# Save results
write_csv(comparison_df, here(results_dir, "diagnostic_comparison.csv"))
write_csv(effects_comparison, here(results_dir, "effects_comparison.csv"))

cat("\n=== SUMMARY ===\n")
cat("Best models based on residual diagnostics:\n")
cat("1. Highest Shapiro p-value (normality):", 
    comparison_df$Model[which.max(comparison_df$Shapiro_p)], "\n")
cat("2. Best DHARMa uniformity:", 
    comparison_df$Model[which.max(comparison_df$DHARMa_Uniformity_p)], "\n")
cat("3. Best dispersion test:", 
    comparison_df$Model[which.max(comparison_df$DHARMa_Dispersion_p)], "\n")

cat("\nAll diagnostic plots saved to:", results_dir, "\n")