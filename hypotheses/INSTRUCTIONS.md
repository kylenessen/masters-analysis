# Monarch Butterfly Abundance Analysis: LLM Implementation Guide

## 🎯 **Project Goal**

The primary objective is to determine if, and how, wind and other environmental factors influence monarch butterfly abundance over various time scales. You will implement a seriest of Generalized Additive Mixed Models (GAMMs) to explore these relationships.

This document outlines the step-by-step process for data preparation, exploratory analysis, model fitting, and reporting. Please follow these instructions precisely, ensuring all outputs are verbose and clearly structured within a Quarto document.

-----

## **Part 1: Setup and Data Loading**

1. **Environment:** Set up an R environment within a Quarto (`.qmd`) file.
2. **Load Libraries:** Begin by loading the necessary libraries.

    ```r
    library(tidyverse)
    library(lubridate)
    library(mgcv)
    library(lme4) # Part of the gamm() backend
    library(corrplot) # For collinearity plots
    ```

3. **Load Data:** Load the four provided CSV files.

    ```r
    counts <- read_csv("data/butterfly_abundance_index.csv")
    deployments <- read_csv("data/deployments.csv")
    temp <- read_csv("data/temperature_data_2023.csv")
    wind <- read_csv("data/wind_all.csv")
    ```

-----

## **Part 2: Initial Data Cleaning and Merging**

The goal of this section is to create a single, clean master data frame that will be the foundation for all subsequent analysis.

1. **Filter and Select `deployments` Data:**

      * Filter the `deployments` data frame to include only deployments where `label_status == "Complete"`.
      * Convert the `view_id` column to a factor (`view_id <- as.factor(view_id)`).
      * Select only the essential columns for merging: `deployment_id`, `view_id`, and `wind_meter_name`.

2. **Parse Timestamps in `counts` Data:**

      * The `image_filename` column contains the timestamp. Create a new `datetime` column by parsing this string. The format is `YYYYMMDDHHMMSS`. Use `lubridate::ymd_hms()`.
      * Example: `str_extract(image_filename, "\\d{14}")` can isolate the numeric part.

3. **Merge Data Frames:**

      * Perform a series of left joins to create the master data frame (`master_df`).
      * Start with the `counts` data (with its new `datetime` column).
      * Join the filtered `deployments` data by `deployment_id`.
      * Join the `temp` data by `image_filename` (or `filename` if the column names differ).
      * The result should be a single tibble containing `datetime`, `total_butterflies`, `butterflies_direct_sun`, `temperature`, `deployment_id`, `view_id`, and `wind_meter_name` for all complete, labeled observations.

-----

## **Part 3: The Lagged Data Preparation Function**

To avoid repeating code, you will create a single, robust function that prepares the analysis-ready data for a given time lag. This function will be the workhorse of the analysis.

**Function Definition:**
Create a function named `prepare_lag_data` with the following signature:
`prepare_lag_data(master_df, wind_df, lag_minutes)`

**Function Steps:**

1. **Create Time-Shifted Data Frame:**

      * Create two copies of `master_df`: `df_t` and `df_t_lag`.
      * In `df_t_lag`, rename columns to reflect the lag (e.g., `total_butterflies` becomes `abundance_t_minus_1`, `datetime` becomes `datetime_t_minus_1`, etc.).

2. **Perform Sliding Window Join:**

      * Join `df_t` with `df_t_lag` to create observation pairs. The join condition is critical: an observation at time `t` must match an observation at time `t-lag_minutes`.
      * `final_df <- left_join(df_t, df_t_lag, by = join_by(view_id, datetime == datetime_t_minus_1 + minutes(lag_minutes)))`
      * This pairs each photo with the photo taken exactly `lag_minutes` before it, within the same camera view.

3. **Filter Invalid Pairs:**

      * Remove rows with `NA` in `abundance_t_minus_1` (these are pairs where no photo existed at the exact lag time).
      * Remove rows where **both** `total_butterflies` (at time *t*) and `abundance_t_minus_1` are equal to 0. These rows provide no information on butterfly movement.

4. **Calculate Interval Predictors:**

      * This is the most intensive step. Use `purrr::map` or a row-wise operation (`rowwise()`) on `final_df` to calculate the environmental metrics for the interval between `datetime_t_minus_1` and `datetime`.
      * For **each row** in `final_df`:
          * **A. Wind Metrics:**
              * Filter `wind_df` for rows where `wind_meter_name` matches and `time` is between `datetime_t_minus_1` and `datetime`.
              * From this subset, calculate: `mean_wind_speed`, `max_wind_speed`, `sd_wind_speed`, `gust_factor` (mean of `gust` / mean of `speed`), and `minutes_above_2mps` (count of minutes where speed \> 2).
          * **B. Temperature Metrics:**
              * Filter `master_df` for rows where `view_id` matches and `datetime` is between `datetime_t_minus_1` and `datetime`.
              * From this subset, calculate: `mean_temp`, `max_temp`, `min_temp`, `sd_temp`.
          * **C. Sunlight Metrics:**
              * Use the same subset from the previous step (temperature).
              * Calculate the **overall sunlight proportion**: `sum(butterflies_direct_sun) / sum(total_butterflies)`. Handle division by zero by returning `NA` or `0` if the denominator is zero.
              * Also calculate sensitivity metrics for the proportion within the interval: `max_prop`, `min_prop`, `sd_prop`.
          * **D. Time Delta:**
              * Calculate the actual time difference `difftime(datetime, datetime_t_minus_1, units = "mins")`. This column will serve as a quality check.

5. **Return Data Frame:**

      * The function should return the final, clean data frame with all calculated predictors, ready for modeling.

-----

## **Part 4: Analysis and Modeling**

You will now execute the analysis for each time lag. Each time lag should have its own dedicated, clearly marked section in the Quarto document.

**For each time lag (30 min, 1 hour, 2 hours, 4 hours):**

1. **Create a Top-Level Heading:** e.g., `## 30-Minute Lag Analysis`.
2. **Prepare Data:** Call your `prepare_lag_data` function.
    `data_30m <- prepare_lag_data(master_df, wind, lag_minutes = 30)`
3. **Exploratory Data Analysis (EDA):**
      * Print the `glimpse()` and `summary()` of the prepared data frame (`data_30m`).
      * Generate and display a collinearity plot for all predictor variables. Use `cor()` to create the matrix and `corrplot()` to visualize it.
4. **Define Model Set:**
      * Create a named list of model formulas to test. The models should range from simple (one predictor) to complex (multiple predictors and/or interactions).
      * **CRITICAL:** Every model formula **must** include `abundance_t_minus_1` as a predictor to account for the baseline count.
5. **Iterate and Fit Models:**
      * Loop through your list of formulas.
      * Inside the loop, for each formula:
          * Create a subheading for the model, e.g., `### Model 3: Wind Speed + Temperature`.
          * Fit the GAMM using `mgcv::gamm()`.
          * **Model Specification:**
              * `response ~ formula_terms`
              * `family = nb()` (Negative Binomial distribution).
              * `random = list(view_id = ~1)` for the random effect.
              * `correlation = corAR1(form = ~ 1 | view_id)` to model residual autocorrelation.
          * **Outputs (be verbose):**
              * Print the full `summary()` of the fitted model object (`gamm_model$gam`).
              * Display the diagnostic plots using `gam.check(gamm_model$gam)`.
          * **Store Results:** Save the model object and its AIC value.
6. **Model Comparison:**
      * At the end of the section for the time lag, present a clean, sorted `data.frame` or `kable` showing the model formula and its corresponding AIC value. This allows for direct comparison of model performance.

-----

## **Part 5: Special Case - "Full Day" Analysis**

This requires a separate data preparation script, not the function from Part 3.

1. **Create a Top-Level Heading:** `## Full Day Lag Analysis`
2. **Data Preparation:**
      * Group `master_df` by `view_id` and `date(datetime)`.
      * For each group, find the row corresponding to the **first** observation with `total_butterflies > 0`. This is your `t-1` observation.
      * Find the row corresponding to the **last** observation of that day. This is your `t` observation.
      * Calculate the time delta. Keep only pairs where the delta is greater than 4 hours.
      * Calculate all the environmental and sunlight predictors over this entire (long) interval.
3. **Modeling:** Follow the same EDA, model fitting, and comparison steps outlined in Part 4.

-----

## **Final Reporting Instructions**

* **Structure:** The final Quarto document should be self-contained and easy to read from top to bottom. Use headings and horizontal rules to create clear visual separation between sections.
* **Interpretation:** Do **not** use the word "significant." Instead, when discussing results, refer to effect sizes, confidence intervals, and the explanatory power from the model summaries. The goal is to report the model outputs, not to make definitive causal claims.
* **Code:** Display all R code chunks and their complete outputs. Do not hide anything. The goal is a fully reproducible and transparent record of the analysis.
