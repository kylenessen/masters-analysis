#!/usr/bin/env Rscript
# Purpose: Simulation-based power analysis for detecting a wind effect in the 30-minute GAM.
# Note: This script mirrors the former power-analysis chunk from analysis/monarch_gam_analysis.qmd
# and exports a power analysis table to thesis_exports/tables and the thesis repo. It is not
# run by default in the main analysis to keep renders fast.

suppressPackageStartupMessages({
  library(tidyverse)
  library(mgcv)
  library(nlme)
  library(here)
  library(knitr)
})

# Load source data (same as in the QMD)
monarch_data <- readr::read_csv(here("data", "monarch_analysis_lag30min.csv"))

# Prepare model_data consistent with the QMD
model_data <- monarch_data %>%
  filter(
    !is.na(butterfly_difference_cbrt),
    !is.na(total_butterflies_t_lag),
    !is.na(max_gust),
    !is.na(temperature_avg),
    !is.na(butterflies_direct_sun_t_lag),
    !is.na(deployment_id),
    !is.na(deployment_day),
    !is.na(Observer),
    !is.na(observation_order_within_day_t)
  )

# Random/correlation structures (match QMD)
random_structure <- list(deployment_id = ~1, Observer = ~1, deployment_day = ~1)
correlation_structure <- corAR1(form = ~ observation_order_within_day_t | deployment_day)

# Fit a base model (without wind) to simulate from. Adjust if your current best model differs.
base_formula <- as.formula(
  "butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)"
)

message("Fitting base model (no wind)...")
base_fit <- gamm(
  base_formula,
  data = model_data,
  random = random_structure,
  correlation = correlation_structure,
  method = "REML"
)

# Power analysis settings
sd_response <- sd(model_data$butterfly_difference_cbrt, na.rm = TRUE)
effect_sizes_to_test <- c(0.05, 0.1, 0.15, 0.2)  # in SD units
n_sims <- 200  # reduce if you need a quicker run
alpha <- 0.05

message("Starting simulations ... this can take a while.")
base_predictions <- predict(base_fit$gam, newdata = model_data)
residual_sd <- summary(base_fit$gam)$scale

power_results <- list()

for (effect_size_coef in effect_sizes_to_test) {
  current_effect_size <- effect_size_coef * sd_response
  message(sprintf("Effect size = %.2f SD", effect_size_coef))

  p_values <- numeric(n_sims)
  for (i in seq_len(n_sims)) {
    if (i %% 10 == 0) message(sprintf("  %d/%d", i, n_sims))

    simulated_response <- rnorm(nrow(model_data), mean = base_predictions, sd = residual_sd)
    wind_effect <- current_effect_size * scale(model_data$max_gust)
    response_with_effect <- simulated_response + as.vector(wind_effect)

    sim_data <- model_data
    sim_data$sim_response <- response_with_effect

    # Model including wind as a smooth term
    power_model_formula <- as.formula(
      "sim_response ~ s(total_butterflies_t_lag) + s(max_gust) + s(temperature_avg) + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)"
    )

    fit <- tryCatch({
      gamm(
        power_model_formula,
        data = sim_data,
        random = random_structure,
        correlation = correlation_structure,
        method = "REML"
      )
    }, error = function(e) NULL)

    if (!is.null(fit)) {
      p_values[i] <- summary(fit$gam)$s.table["s(max_gust)", "p-value"]
    } else {
      p_values[i] <- NA_real_
    }
  }

  power <- mean(p_values < alpha, na.rm = TRUE)
  power_results[[as.character(effect_size_coef)]] <- power
}

power_df <- tibble(
  `Effect Size (SD units)` = as.numeric(names(power_results)),
  `Power (Proportion)` = unlist(power_results)
) %>%
  mutate(`Power (%)` = paste0(round(`Power (Proportion)` * 100, 1), "%"))

# Export locations
export_dir <- here("thesis_exports")
tab_dir <- file.path(export_dir, "tables")
thesis_results_tab_dir <- here("..", "masters-thesis", "supplemental", "results", "thesis_exports", "tables")
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(thesis_results_tab_dir, recursive = TRUE, showWarnings = FALSE)

# Write CSV and LaTeX
readr::write_csv(power_df, file.path(tab_dir, "power_analysis_table.csv"))
power_kable <- knitr::kable(power_df, format = "latex", booktabs = TRUE,
                            caption = "Estimated power to detect a given effect size for the max_gust term (30-minute GAM)")
writeLines(power_kable, file.path(tab_dir, "power_analysis_table.tex"))

# Mirror to thesis repo
file.copy(file.path(tab_dir, "power_analysis_table.csv"), file.path(thesis_results_tab_dir, "power_analysis_table.csv"), overwrite = TRUE)
file.copy(file.path(tab_dir, "power_analysis_table.tex"), file.path(thesis_results_tab_dir, "power_analysis_table.tex"), overwrite = TRUE)

message("Power analysis exports written to:")
message("- ", file.path(tab_dir, "power_analysis_table.csv"))
message("- ", file.path(tab_dir, "power_analysis_table.tex"))
