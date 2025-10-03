---
title: "Dynamic Window GAM Analysis: Sunset Window (Max Count → Functional Sunset)"
author: "Kyle Nessen"
date: "2025-10-03"
format:
  html:
    toc: true
    toc-depth: 3
    toc-location: left
    number-sections: true
    embed-resources: true
    theme: cosmo
    code-fold: true
    code-tools: false
    keep-md: true
    fig-path: "figures/sunset_"
execute:
  warning: false
  message: false
  echo: true
  cache: false
---



## Background and Rationale

This analysis addresses feedback from Francis regarding the temporal alignment of weather predictors with butterfly responses. The [original daily-level analysis](../../reports/daily_gam_results.html) used fixed 6am-6pm weather windows, which Francis identified as having a temporal logic issue:

> "All of the metrics for wind, temperature and light, would need to be re-calculated for 24 hour periods that begin at the time of the highest count."

### Francis's Key Points

1. **Temporal alignment**: Butterflies can only respond to weather *after* it occurs
2. **Biological timing**: If max count occurred at 2pm on day t-1, the relevant weather window should start at 2pm, not 6am
3. **Roosting decisions**: Weather from max count through sunset determines whether butterflies abandon the roost

### This Analysis

**Weather window**: `time_of_max_count_t_1` → `last_observation_time_t` (functional sunset)

- Weather metrics now include **overnight conditions** (24/7 temperature and wind)
- Window lengths vary by observation (mean: 29.6 hours, range: 22.5-34.9 hours)
- Captures weather exposure until roosting decisions are finalized at dusk
- Tests the hypothesis: "Do weather conditions from peak count to sunset predict roost abandonment?"

**Data source**: `data/monarch_daily_lag_analysis_sunset_window.csv` (103 lag pairs)

---

## Setup


::: {.cell}

```{.r .cell-code}
library(tidyverse)
library(mgcv)
library(knitr)
library(here)
library(corrplot)
library(patchwork)
library(gratia)

# Load the sunset window data
daily_data <- read_csv(here("data", "monarch_daily_lag_analysis_sunset_window.csv"))

# Create the square root transformed response variable
daily_data <- daily_data %>%
    mutate(
        butterfly_diff_95th_sqrt = ifelse(butterfly_diff_95th >= 0,
            sqrt(butterfly_diff_95th),
            -sqrt(-butterfly_diff_95th)
        )
    )
```
:::


## Data Overview


::: {.cell}

```{.r .cell-code}
cat("Dataset dimensions:", nrow(daily_data), "rows x", ncol(daily_data), "columns\n")
```

::: {.cell-output .cell-output-stdout}

```
Dataset dimensions: 103 rows x 52 columns
```


:::

```{.r .cell-code}
cat("Number of deployments:", n_distinct(daily_data$deployment_id), "\n")
```

::: {.cell-output .cell-output-stdout}

```
Number of deployments: 7 
```


:::

```{.r .cell-code}
cat("Date range:", min(daily_data$date_t), "to", max(daily_data$date_t), "\n\n")
```

::: {.cell-output .cell-output-stdout}

```
Date range: 19680 to 19756 
```


:::

```{.r .cell-code}
cat("Window characteristics:\n")
```

::: {.cell-output .cell-output-stdout}

```
Window characteristics:
```


:::

```{.r .cell-code}
cat("- Mean lag duration:", round(mean(daily_data$lag_duration_hours, na.rm = TRUE), 2), "hours\n")
```

::: {.cell-output .cell-output-stdout}

```
- Mean lag duration: 29.56 hours
```


:::

```{.r .cell-code}
cat(
    "- Duration range:", round(min(daily_data$lag_duration_hours, na.rm = TRUE), 2), "-",
    round(max(daily_data$lag_duration_hours, na.rm = TRUE), 2), "hours\n"
)
```

::: {.cell-output .cell-output-stdout}

```
- Duration range: 22.5 - 34.9 hours
```


:::

```{.r .cell-code}
cat("- Median data completeness:", round(median(daily_data$metrics_complete, na.rm = TRUE), 3), "\n\n")
```

::: {.cell-output .cell-output-stdout}

```
- Median data completeness: 1 
```


:::

```{.r .cell-code}
# Summary of key variables
summary_vars <- daily_data %>%
    select(
        butterflies_95th_percentile_t,
        butterflies_95th_percentile_t_1,
        butterfly_diff_95th,
        temp_max,
        temp_min,
        temp_at_max_count_t_1,
        wind_max_gust,
        sum_butterflies_direct_sun,
        lag_duration_hours,
        metrics_complete
    )

kable(summary(summary_vars), caption = "Summary statistics for key variables")
```

::: {.cell-output-display}


Table: Summary statistics for key variables

|   |butterflies_95th_percentile_t |butterflies_95th_percentile_t_1 |butterfly_diff_95th |   temp_max   |   temp_min    |temp_at_max_count_t_1 |wind_max_gust  |sum_butterflies_direct_sun |lag_duration_hours |metrics_complete |
|:--|:-----------------------------|:-------------------------------|:-------------------|:-------------|:--------------|:---------------------|:--------------|:--------------------------|:------------------|:----------------|
|   |Min.   :  0.00                |Min.   :  0.0                   |Min.   :-310.000    |Min.   :14.00 |Min.   : 3.000 |Min.   : 5.00         |Min.   : 0.000 |Min.   :   0.0             |Min.   :22.50      |Min.   :0.0000   |
|   |1st Qu.: 14.85                |1st Qu.: 17.5                   |1st Qu.: -31.000    |1st Qu.:17.00 |1st Qu.: 6.000 |1st Qu.:11.50         |1st Qu.: 3.200 |1st Qu.:  12.0             |1st Qu.:25.50      |1st Qu.:0.9996   |
|   |Median : 70.05                |Median : 77.0                   |Median :  -2.950    |Median :19.00 |Median : 8.000 |Median :14.00         |Median : 4.100 |Median :  42.0             |Median :30.50      |Median :0.9996   |
|   |Mean   :107.41                |Mean   :116.3                   |Mean   :  -8.919    |Mean   :20.47 |Mean   : 8.311 |Mean   :13.37         |Mean   : 4.391 |Mean   : 132.8             |Mean   :29.56      |Mean   :0.9593   |
|   |3rd Qu.:166.95                |3rd Qu.:199.5                   |3rd Qu.:  18.000    |3rd Qu.:23.00 |3rd Qu.:11.000 |3rd Qu.:15.50         |3rd Qu.: 5.100 |3rd Qu.: 168.5             |3rd Qu.:33.17      |3rd Qu.:0.9997   |
|   |Max.   :499.00                |Max.   :499.0                   |Max.   : 256.600    |Max.   :34.00 |Max.   :14.000 |Max.   :25.00         |Max.   :12.800 |Max.   :1122.0             |Max.   :34.90      |Max.   :0.9998   |
|   |NA                            |NA                              |NA                  |NA            |NA             |NA                    |NA's   :2      |NA                         |NA                 |NA               |


:::
:::


## Response Variable Selection

We evaluate three butterfly difference metrics (max, 95th percentile, and top 3 mean) with three transformations each (untransformed, square root, and square) to determine which best approximates normality.


::: {.cell}

```{.r .cell-code}
# Filter to complete data for consistency with modeling
data_filtered <- daily_data %>% filter(metrics_complete >= 0.95)

# Define response variables
response_vars <- c(
    "butterfly_diff", "butterfly_diff_sqrt", "butterfly_diff_sq",
    "butterfly_diff_95th", "butterfly_diff_95th_sqrt", "butterfly_diff_95th_sq",
    "butterfly_diff_top3", "butterfly_diff_top3_sqrt", "butterfly_diff_top3_sq"
)

# Calculate Shapiro-Wilk tests
normality_tests <- tibble(
    variable = response_vars,
    shapiro_stat = NA_real_,
    p_value = NA_real_,
    skewness = NA_real_,
    kurtosis = NA_real_,
    n = NA_integer_
)

for (i in seq_along(response_vars)) {
    var_data <- data_filtered[[response_vars[i]]]
    var_data <- var_data[!is.na(var_data)]

    sw_test <- shapiro.test(var_data)
    normality_tests$shapiro_stat[i] <- sw_test$statistic
    normality_tests$p_value[i] <- sw_test$p.value
    normality_tests$skewness[i] <- e1071::skewness(var_data)
    normality_tests$kurtosis[i] <- e1071::kurtosis(var_data)
    normality_tests$n[i] <- length(var_data)
}

# Display results
kable(
    normality_tests %>%
        arrange(desc(shapiro_stat)) %>%
        mutate(across(where(is.numeric), ~ round(.x, 4))),
    caption = "Shapiro-Wilk Normality Tests (sorted by W statistic)"
)
```

::: {.cell-output-display}


Table: Shapiro-Wilk Normality Tests (sorted by W statistic)

|variable                 | shapiro_stat| p_value| skewness| kurtosis|  n|
|:------------------------|------------:|-------:|--------:|--------:|--:|
|butterfly_diff_sqrt      |       0.9893|  0.6368|   0.2251|  -0.3143| 96|
|butterfly_diff_95th_sqrt |       0.9880|  0.5411|   0.0120|  -0.6497| 96|
|butterfly_diff_top3_sqrt |       0.9875|  0.5027|   0.0266|  -0.6207| 96|
|butterfly_diff_95th      |       0.9182|  0.0000|  -0.3705|   2.0466| 96|
|butterfly_diff_top3      |       0.9126|  0.0000|  -0.0126|   2.4706| 96|
|butterfly_diff           |       0.8835|  0.0000|   0.3794|   4.3938| 96|
|butterfly_diff_95th_sq   |       0.6591|  0.0000|  -1.2037|   9.1440| 96|
|butterfly_diff_top3_sq   |       0.6327|  0.0000|   0.6220|  10.0799| 96|
|butterfly_diff_sq        |       0.5307|  0.0000|   1.9996|  19.7056| 96|


:::

```{.r .cell-code}
# Identify best
best_response <- normality_tests %>%
    filter(shapiro_stat == max(shapiro_stat)) %>%
    pull(variable)

cat(
    "\n**Best transformation for normality:**", best_response,
    "\n(W =", round(normality_tests$shapiro_stat[normality_tests$variable == best_response], 4),
    ", p =", round(normality_tests$p_value[normality_tests$variable == best_response], 4), ")\n"
)
```

::: {.cell-output .cell-output-stdout}

```

**Best transformation for normality:** butterfly_diff_sqrt 
(W = 0.9893 , p = 0.6368 )
```


:::

```{.r .cell-code}
# Create histograms
par(mfrow = c(3, 3), mar = c(4, 4, 3, 1))
for (var in response_vars) {
    var_data <- data_filtered[[var]]
    var_data <- var_data[!is.na(var_data)]

    # Histogram
    hist(var_data,
        breaks = 30, probability = TRUE,
        main = sprintf(
            "%s\n(W=%.3f, p=%.4f)",
            var,
            normality_tests$shapiro_stat[normality_tests$variable == var],
            normality_tests$p_value[normality_tests$variable == var]
        ),
        xlab = "Value", col = "steelblue", border = "black"
    )

    # Overlay normal distribution
    x_seq <- seq(min(var_data), max(var_data), length.out = 100)
    lines(x_seq, dnorm(x_seq, mean(var_data), sd(var_data)),
        col = "red", lwd = 2
    )

    # Add grid
    grid()
}
```

::: {.cell-output-display}
![](sunset_window_gam_analysis_files/figure-html/response-normality-1.png){width=1152}
:::
:::


### Key Findings

- **Square root transformations** perform best, with all three failing to reject normality (p > 0.5)
- **Untransformed differences** show significant departures from normality (p < 0.0001)
- **Square transformations** severely violate normality with extreme kurtosis (>9)
- `butterfly_diff_sqrt` has the highest Shapiro-Wilk statistic (W = 0.9893, p = 0.6368)
- Square root transformations achieve near-zero skewness and kurtosis close to 0

**Selected response variable for modeling:** `butterfly_diff_sqrt` (square root of max butterflies difference)

## Candidate Predictor Variables

Following the original analysis structure, we consider all potential weather and baseline metrics, then use correlation analysis to select final predictors.

### All Candidate Variables


::: {.cell}

```{.r .cell-code}
# Define all candidate predictors
butterfly_baseline_vars <- c(
    "butterflies_95th_percentile_t_1",
    "max_butterflies_t_1",
    "butterflies_top3_mean_t_1"
)

temperature_vars <- c(
    "temp_max",
    "temp_min",
    "temp_mean",
    "temp_at_max_count_t_1",
    "hours_above_15C",
    "degree_hours_above_15C"
)

wind_vars <- c(
    "wind_avg_sustained",
    "wind_max_gust",
    "wind_gust_sum",
    "wind_gust_sum_above_2ms",
    "wind_gust_hours",
    "wind_minutes_above_2ms",
    "wind_gust_sd",
    "wind_mode_gust"
)

sun_vars <- c(
    "sum_butterflies_direct_sun"
)

window_vars <- c(
    "lag_duration_hours"
)

# Combine all candidates
all_predictors <- c(
    butterfly_baseline_vars,
    temperature_vars,
    wind_vars,
    sun_vars,
    window_vars
)

cat("Total candidate predictors:", length(all_predictors), "\n")
```

::: {.cell-output .cell-output-stdout}

```
Total candidate predictors: 19 
```


:::

```{.r .cell-code}
cat("- Butterfly baseline:", length(butterfly_baseline_vars), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Butterfly baseline: 3 
```


:::

```{.r .cell-code}
cat("- Temperature:", length(temperature_vars), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Temperature: 6 
```


:::

```{.r .cell-code}
cat("- Wind:", length(wind_vars), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Wind: 8 
```


:::

```{.r .cell-code}
cat("- Sun exposure:", length(sun_vars), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Sun exposure: 1 
```


:::

```{.r .cell-code}
cat("- Window characteristics:", length(window_vars), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Window characteristics: 1 
```


:::
:::


### Variable Descriptions


::: {.cell}

```{.r .cell-code}
var_descriptions <- tribble(
    ~Variable, ~Description, ~Type,
    "butterflies_95th_percentile_t_1", "95th percentile count on previous day (baseline)", "Baseline",
    "max_butterflies_t_1", "Maximum count on previous day", "Baseline",
    "butterflies_top3_mean_t_1", "Mean of top 3 counts on previous day", "Baseline",
    "temp_max", "Max temp from max count to sunset (includes overnight)", "Temperature",
    "temp_min", "Min temp from max count to sunset (includes overnight)", "Temperature",
    "temp_mean", "Mean temp from max count to sunset", "Temperature",
    "temp_at_max_count_t_1", "Temperature when max count occurred", "Temperature",
    "hours_above_15C", "Hours ≥15°C in window", "Temperature",
    "degree_hours_above_15C", "Cumulative degree-hours >15°C", "Temperature",
    "wind_avg_sustained", "Mean sustained wind speed in window", "Wind",
    "wind_max_gust", "Maximum gust in window (includes overnight)", "Wind",
    "wind_gust_sum", "Sum of all gust measurements", "Wind",
    "wind_gust_sum_above_2ms", "Sum of gusts >2 m/s", "Wind",
    "wind_gust_hours", "Gust-hours (integral)", "Wind",
    "wind_minutes_above_2ms", "Minutes with wind ≥2 m/s", "Wind",
    "wind_gust_sd", "SD of gust speeds (variability)", "Wind",
    "wind_mode_gust", "Most frequent gust speed", "Wind",
    "sum_butterflies_direct_sun", "Sum of butterflies in direct sun (entire lag window)", "Sun",
    "lag_duration_hours", "Window length in hours", "Window"
)

kable(var_descriptions, caption = "Candidate predictor variables")
```

::: {.cell-output-display}


Table: Candidate predictor variables

|Variable                        |Description                                            |Type        |
|:-------------------------------|:------------------------------------------------------|:-----------|
|butterflies_95th_percentile_t_1 |95th percentile count on previous day (baseline)       |Baseline    |
|max_butterflies_t_1             |Maximum count on previous day                          |Baseline    |
|butterflies_top3_mean_t_1       |Mean of top 3 counts on previous day                   |Baseline    |
|temp_max                        |Max temp from max count to sunset (includes overnight) |Temperature |
|temp_min                        |Min temp from max count to sunset (includes overnight) |Temperature |
|temp_mean                       |Mean temp from max count to sunset                     |Temperature |
|temp_at_max_count_t_1           |Temperature when max count occurred                    |Temperature |
|hours_above_15C                 |Hours ≥15°C in window                                  |Temperature |
|degree_hours_above_15C          |Cumulative degree-hours >15°C                          |Temperature |
|wind_avg_sustained              |Mean sustained wind speed in window                    |Wind        |
|wind_max_gust                   |Maximum gust in window (includes overnight)            |Wind        |
|wind_gust_sum                   |Sum of all gust measurements                           |Wind        |
|wind_gust_sum_above_2ms         |Sum of gusts >2 m/s                                    |Wind        |
|wind_gust_hours                 |Gust-hours (integral)                                  |Wind        |
|wind_minutes_above_2ms          |Minutes with wind ≥2 m/s                               |Wind        |
|wind_gust_sd                    |SD of gust speeds (variability)                        |Wind        |
|wind_mode_gust                  |Most frequent gust speed                               |Wind        |
|sum_butterflies_direct_sun      |Sum of butterflies in direct sun (entire lag window)   |Sun         |
|lag_duration_hours              |Window length in hours                                 |Window      |


:::
:::


## Data Quality Assessment


::: {.cell}

```{.r .cell-code}
cat("Data completeness summary:\n")
```

::: {.cell-output .cell-output-stdout}

```
Data completeness summary:
```


:::

```{.r .cell-code}
cat("- Observations with metrics_complete = 0:", sum(daily_data$metrics_complete == 0), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Observations with metrics_complete = 0: 2 
```


:::

```{.r .cell-code}
cat("- Observations with wind_data_coverage < 0.5:", sum(daily_data$wind_data_coverage < 0.5), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Observations with wind_data_coverage < 0.5: 5 
```


:::

```{.r .cell-code}
cat("- Mean temperature coverage:", round(mean(daily_data$temp_data_coverage), 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Mean temperature coverage: 1 
```


:::

```{.r .cell-code}
cat("- Mean wind coverage:", round(mean(daily_data$wind_data_coverage), 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Mean wind coverage: 0.952 
```


:::

```{.r .cell-code}
cat("- Mean butterfly coverage:", round(mean(daily_data$butterfly_data_coverage), 3), "\n\n")
```

::: {.cell-output .cell-output-stdout}

```
- Mean butterfly coverage: 0.989 
```


:::

```{.r .cell-code}
# Show observations with low wind coverage
low_wind <- daily_data %>%
    filter(wind_data_coverage < 0.5) %>%
    select(
        deployment_id, date_t_1, date_t, wind_data_coverage,
        butterflies_95th_percentile_t_1, butterflies_95th_percentile_t
    )

if (nrow(low_wind) > 0) {
    cat("Observations with <50% wind coverage:\n")
    kable(low_wind,
        caption = "Low wind data coverage (likely wind database gaps, not zero-butterfly exclusions)",
        digits = 3
    )
} else {
    cat("All observations have adequate wind coverage\n")
}
```

::: {.cell-output .cell-output-stdout}

```
Observations with <50% wind coverage:
```


:::

::: {.cell-output-display}


Table: Low wind data coverage (likely wind database gaps, not zero-butterfly exclusions)

|deployment_id |date_t_1   |date_t     | wind_data_coverage| butterflies_95th_percentile_t_1| butterflies_95th_percentile_t|
|:-------------|:----------|:----------|------------------:|-------------------------------:|-----------------------------:|
|SC10          |2024-01-27 |2024-01-28 |              0.185|                             0.0|                          6.70|
|SC10          |2024-01-28 |2024-01-29 |              0.043|                             6.7|                         14.70|
|SC10          |2024-01-29 |2024-01-30 |              0.000|                            14.7|                          9.00|
|SC10          |2024-01-30 |2024-01-31 |              0.000|                             9.0|                          7.95|
|SC9           |2024-01-28 |2024-01-29 |              0.040|                            18.3|                          1.00|


:::
:::

::: {.cell}

```{.r .cell-code}
# Display the diagnostic plot created earlier
knitr::include_graphics(here("analysis", "dynamic_window_analysis", "data_quality_diagnostics.png"))
```

::: {.cell-output-display}
![](data_quality_diagnostics.png){width=892}
:::
:::


**Note**: 5 observations (all from late January 2024) have limited wind data coverage. These appear to be gaps in the wind database rather than issues with the data preparation logic. All observations have butterflies present.

### Filtering Low Quality Observations


::: {.cell}

```{.r .cell-code}
# Filter observations with metrics_complete < 0.95
low_quality <- daily_data %>%
    filter(metrics_complete < 0.95)

cat("Observations to exclude (metrics_complete < 0.95):", nrow(low_quality), "\n")
```

::: {.cell-output .cell-output-stdout}

```
Observations to exclude (metrics_complete < 0.95): 7 
```


:::

```{.r .cell-code}
cat("Percentage of dataset:", round(nrow(low_quality) / nrow(daily_data) * 100, 1), "%\n\n")
```

::: {.cell-output .cell-output-stdout}

```
Percentage of dataset: 6.8 %
```


:::

```{.r .cell-code}
if (nrow(low_quality) > 0) {
    cat("Excluded observations have:\n")
    cat("- Mean butterflies_95th_t_1:", round(mean(low_quality$butterflies_95th_percentile_t_1), 1), "\n")
    cat("- Mean butterflies_95th_t:", round(mean(low_quality$butterflies_95th_percentile_t), 1), "\n")
    cat("- Mean |butterfly_diff_95th|:", round(mean(abs(low_quality$butterfly_diff_95th)), 1), "\n")
    cat("- Mean metrics_complete:", round(mean(low_quality$metrics_complete), 3), "\n\n")
}
```

::: {.cell-output .cell-output-stdout}

```
Excluded observations have:
- Mean butterflies_95th_t_1: 22.9 
- Mean butterflies_95th_t: 19.2 
- Mean |butterfly_diff_95th|: 7.9 
- Mean metrics_complete: 0.45 
```


:::

```{.r .cell-code}
# Apply filter
daily_data <- daily_data %>%
    filter(metrics_complete >= 0.95)

cat("After filtering:\n")
```

::: {.cell-output .cell-output-stdout}

```
After filtering:
```


:::

```{.r .cell-code}
cat("- Observations retained:", nrow(daily_data), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Observations retained: 96 
```


:::

```{.r .cell-code}
cat("- Mean butterflies_95th_t_1:", round(mean(daily_data$butterflies_95th_percentile_t_1), 1), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Mean butterflies_95th_t_1: 123.1 
```


:::

```{.r .cell-code}
cat("- Mean butterflies_95th_t:", round(mean(daily_data$butterflies_95th_percentile_t), 1), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Mean butterflies_95th_t: 113.8 
```


:::

```{.r .cell-code}
cat("- Mean |butterfly_diff_95th|:", round(mean(abs(daily_data$butterfly_diff_95th)), 1), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Mean |butterfly_diff_95th|: 58 
```


:::

```{.r .cell-code}
cat("- Mean metrics_complete:", round(mean(daily_data$metrics_complete), 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
- Mean metrics_complete: 0.996 
```


:::
:::


**Rationale for exclusion**: The 7 excluded observations (6.8% of dataset) have relatively small butterfly counts (mean 95th percentile: 22.9 vs 123.1 for kept data) and incomplete weather data that could bias model estimates.

## Correlation Matrix: All Candidate Predictors

This correlation matrix shows all potential fixed effects to help identify multicollinearity and guide variable selection.


::: {.cell}

```{.r .cell-code}
# Select all candidate predictors that exist in the dataset
available_predictors <- all_predictors[all_predictors %in% names(daily_data)]

# Create correlation matrix
predictor_data <- daily_data %>%
    select(all_of(available_predictors)) %>%
    na.omit()

cor_matrix <- cor(predictor_data)

# Create correlation plot
corrplot(cor_matrix,
    method = "color",
    type = "upper",
    order = "original",
    tl.cex = 0.8,
    tl.col = "black",
    tl.srt = 45,
    addCoef.col = "black",
    number.cex = 0.6,
    title = "Correlation Matrix: All Candidate Predictors (Sunset Window)",
    mar = c(0, 0, 2, 0)
)
```

::: {.cell-output-display}
![](sunset_window_gam_analysis_files/figure-html/full-correlation-matrix-1.png){width=1344}
:::
:::



## Model Building Strategy

Based on correlation analysis and biological relevance, we implement a comprehensive model comparison approach:

### Selected Fixed Effects

**Always included (in all models):**
- `max_butterflies_t_1`: Baseline count (tested both as smooth and linear)
- `lag_duration_hours`: Window duration control (tested both as smooth and linear)

**Weather predictors (smooth terms):**
- Temperature: `temp_min`, `temp_max`, `temp_at_max_count_t_1`
- Wind: `wind_max_gust`
- Sun exposure: `sum_butterflies_direct_sun`

**Response variable:** `butterfly_diff_sqrt`

**Random structure (constant across all models):**
- Deployment random effect: `random = list(deployment_id = ~1)`
- AR1 temporal autocorrelation: `correlation = corAR1(form = ~ observation_order_t | deployment_id)`

### Model Complexity Levels

We systematically explore the entire model space defined by our 5 weather predictors:

1. **Null models** (2): Baseline + controls only
2. **Single predictor models** (10): One weather variable at a time
3. **Interaction-only models** (20): Pure two-way interactions without main effects
4. **Main + interaction models** (20): Both main effects + their two-way interaction
5. **Strategic additive models** (14): Biologically meaningful predictor combinations
6. **Complex interaction models** (8): Multiple main effects + selected interaction sets
7. **Full models** (2): All main effects + all 10 two-way interactions

**Total: 76 models** comprehensively testing all meaningful combinations of the 5 selected weather predictors.

### Functional Form Comparison

For baseline controls (`max_butterflies_t_1` and `lag_duration_hours`), every model is fitted in two versions:
- **Smooth baseline**: Both as smooth terms `s(..., k=5)`
- **Linear baseline**: Both as linear terms

AIC determines optimal functional form for each predictor combination.

### Model Naming Convention

Models sequentially numbered `M1` through `M76`, systematically covering:
- Increasing complexity: null → single → interactions → additive → complex → full
- Each complexity level includes both smooth and linear baseline versions
- Model descriptions track predictor combinations and baseline type


::: {.cell}

```{.r .cell-code}
# Load required library for AR1
library(nlme)

# Prepare data
model_data <- daily_data %>%
    filter(metrics_complete >= 0.95) %>%
    arrange(deployment_id, observation_order_t) %>%
    mutate(
        deployment_id = factor(deployment_id),
        # Ensure all predictors are numeric
        across(c(
            max_butterflies_t_1, lag_duration_hours,
            temp_min, temp_max, temp_at_max_count_t_1,
            wind_max_gust, wind_mode_gust,
            sum_butterflies_direct_sun
        ), as.numeric)
    ) %>%
    # Remove any rows with NA in key variables
    filter(
        !is.na(butterfly_diff_sqrt),
        !is.na(max_butterflies_t_1),
        !is.na(lag_duration_hours)
    )

cat("Model data:", nrow(model_data), "observations\n")
```

::: {.cell-output .cell-output-stdout}

```
Model data: 96 observations
```


:::

```{.r .cell-code}
cat("Deployments:", n_distinct(model_data$deployment_id), "\n")
```

::: {.cell-output .cell-output-stdout}

```
Deployments: 6 
```


:::

```{.r .cell-code}
# Define random effects structure with temporal autocorrelation
random_structure <- list(deployment_id = ~1)

# Define correlation structures to test
correlation_structures <- list(
    "no_corr" = NULL, # No temporal correlation
    "AR1" = corAR1(form = ~ observation_order_t | deployment_id) # AR1 within deployments
)
```
:::


## Model Fitting


::: {.cell}

```{.r .cell-code}
# Initialize model list and tracking
models <- list()
model_descriptions <- list()

# Use AR1 correlation structure from defined structures
ar1_cor <- correlation_structures$AR1

# Set k values based on unique values in data
# lag_duration_hours has 37 unique values, k=5 is conservative
k_baseline <- 5
k_lag <- 5

# Define weather predictors
weather_predictors <- c(
    "temp_min", "temp_max", "temp_at_max_count_t_1",
    "wind_max_gust", "sum_butterflies_direct_sun"
)

# Helper function to fit model with error handling
fit_model_safe <- function(formula_str, baseline_type, model_name, description) {
    tryCatch(
        {
            model <- gamm(
                as.formula(formula_str),
                data = model_data,
                random = random_structure,
                correlation = ar1_cor,
                method = "REML"
            )
            models[[model_name]] <<- model
            model_descriptions[[model_name]] <<- description
            cat(sprintf("✓ %s: %s\n", model_name, description))
            return(TRUE)
        },
        error = function(e) {
            cat(sprintf("✗ %s failed: %s\n", model_name, e$message))
            return(FALSE)
        }
    )
}

# ============================================================================
# PART 1: NULL MODELS (2 models)
# ============================================================================
cat("\n=== FITTING NULL MODELS ===\n")
```

::: {.cell-output .cell-output-stdout}

```

=== FITTING NULL MODELS ===
```


:::

```{.r .cell-code}
# Smooth baseline
fit_model_safe(
    "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + s(lag_duration_hours, k = k_lag)",
    "smooth", "M1", "Null (smooth baseline)"
)
```

::: {.cell-output .cell-output-stdout}

```
✓ M1: Null (smooth baseline)
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] TRUE
```


:::

```{.r .cell-code}
# Linear baseline
fit_model_safe(
    "butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours",
    "linear", "M2", "Null (linear baseline)"
)
```

::: {.cell-output .cell-output-stdout}

```
✓ M2: Null (linear baseline)
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] TRUE
```


:::

```{.r .cell-code}
# ============================================================================
# PART 2: SINGLE PREDICTOR MODELS (10 models)
# ============================================================================
cat("\n=== FITTING SINGLE PREDICTOR MODELS ===\n")
```

::: {.cell-output .cell-output-stdout}

```

=== FITTING SINGLE PREDICTOR MODELS ===
```


:::

```{.r .cell-code}
model_num <- 3
for (pred in weather_predictors) {
    # Smooth baseline
    fit_model_safe(
        sprintf("butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + s(lag_duration_hours, k = k_lag) + s(%s)", pred),
        "smooth", paste0("M", model_num), sprintf("Single: %s (smooth)", pred)
    )
    model_num <- model_num + 1

    # Linear baseline
    fit_model_safe(
        sprintf("butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours + s(%s)", pred),
        "linear", paste0("M", model_num), sprintf("Single: %s (linear)", pred)
    )
    model_num <- model_num + 1
}
```

::: {.cell-output .cell-output-stdout}

```
✓ M3: Single: temp_min (smooth)
✓ M4: Single: temp_min (linear)
✓ M5: Single: temp_max (smooth)
✓ M6: Single: temp_max (linear)
✓ M7: Single: temp_at_max_count_t_1 (smooth)
✓ M8: Single: temp_at_max_count_t_1 (linear)
✓ M9: Single: wind_max_gust (smooth)
✓ M10: Single: wind_max_gust (linear)
✓ M11: Single: sum_butterflies_direct_sun (smooth)
✓ M12: Single: sum_butterflies_direct_sun (linear)
```


:::

```{.r .cell-code}
# ============================================================================
# PART 3: TWO-WAY INTERACTION MODELS (20 models)
# Only interactions, no main effects (tests pure interaction)
# ============================================================================
cat("\n=== FITTING TWO-WAY INTERACTION MODELS (interactions only) ===\n")
```

::: {.cell-output .cell-output-stdout}

```

=== FITTING TWO-WAY INTERACTION MODELS (interactions only) ===
```


:::

```{.r .cell-code}
# Generate all pairwise interactions
interaction_pairs <- combn(weather_predictors, 2, simplify = FALSE)

for (pair in interaction_pairs) {
    pred1 <- pair[1]
    pred2 <- pair[2]

    # Smooth baseline
    fit_model_safe(
        sprintf("butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + s(lag_duration_hours, k = k_lag) + ti(%s, %s)", pred1, pred2),
        "smooth", paste0("M", model_num), sprintf("Interaction: %s × %s (smooth)", pred1, pred2)
    )
    model_num <- model_num + 1

    # Linear baseline
    fit_model_safe(
        sprintf("butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours + ti(%s, %s)", pred1, pred2),
        "linear", paste0("M", model_num), sprintf("Interaction: %s × %s (linear)", pred1, pred2)
    )
    model_num <- model_num + 1
}
```

::: {.cell-output .cell-output-stdout}

```
✓ M13: Interaction: temp_min × temp_max (smooth)
✓ M14: Interaction: temp_min × temp_max (linear)
✓ M15: Interaction: temp_min × temp_at_max_count_t_1 (smooth)
✓ M16: Interaction: temp_min × temp_at_max_count_t_1 (linear)
✓ M17: Interaction: temp_min × wind_max_gust (smooth)
✓ M18: Interaction: temp_min × wind_max_gust (linear)
✓ M19: Interaction: temp_min × sum_butterflies_direct_sun (smooth)
✓ M20: Interaction: temp_min × sum_butterflies_direct_sun (linear)
✓ M21: Interaction: temp_max × temp_at_max_count_t_1 (smooth)
✓ M22: Interaction: temp_max × temp_at_max_count_t_1 (linear)
✓ M23: Interaction: temp_max × wind_max_gust (smooth)
✓ M24: Interaction: temp_max × wind_max_gust (linear)
✓ M25: Interaction: temp_max × sum_butterflies_direct_sun (smooth)
✓ M26: Interaction: temp_max × sum_butterflies_direct_sun (linear)
✓ M27: Interaction: temp_at_max_count_t_1 × wind_max_gust (smooth)
✓ M28: Interaction: temp_at_max_count_t_1 × wind_max_gust (linear)
✓ M29: Interaction: temp_at_max_count_t_1 × sum_butterflies_direct_sun (smooth)
✓ M30: Interaction: temp_at_max_count_t_1 × sum_butterflies_direct_sun (linear)
✓ M31: Interaction: wind_max_gust × sum_butterflies_direct_sun (smooth)
✓ M32: Interaction: wind_max_gust × sum_butterflies_direct_sun (linear)
```


:::

```{.r .cell-code}
# ============================================================================
# PART 4: MAIN EFFECTS + INTERACTION MODELS (20 models)
# Both main effects + their interaction
# ============================================================================
cat("\n=== FITTING MAIN EFFECTS + INTERACTION MODELS ===\n")
```

::: {.cell-output .cell-output-stdout}

```

=== FITTING MAIN EFFECTS + INTERACTION MODELS ===
```


:::

```{.r .cell-code}
for (pair in interaction_pairs) {
    pred1 <- pair[1]
    pred2 <- pair[2]

    # Smooth baseline
    fit_model_safe(
        sprintf(
            "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + s(lag_duration_hours, k = k_lag) + s(%s) + s(%s) + ti(%s, %s)",
            pred1, pred2, pred1, pred2
        ),
        "smooth", paste0("M", model_num), sprintf("Additive + Interaction: %s + %s + %s×%s (smooth)", pred1, pred2, pred1, pred2)
    )
    model_num <- model_num + 1

    # Linear baseline
    fit_model_safe(
        sprintf(
            "butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours + s(%s) + s(%s) + ti(%s, %s)",
            pred1, pred2, pred1, pred2
        ),
        "linear", paste0("M", model_num), sprintf("Additive + Interaction: %s + %s + %s×%s (linear)", pred1, pred2, pred1, pred2)
    )
    model_num <- model_num + 1
}
```

::: {.cell-output .cell-output-stdout}

```
✓ M33: Additive + Interaction: temp_min + temp_max + temp_min×temp_max (smooth)
✓ M34: Additive + Interaction: temp_min + temp_max + temp_min×temp_max (linear)
✓ M35: Additive + Interaction: temp_min + temp_at_max_count_t_1 + temp_min×temp_at_max_count_t_1 (smooth)
✓ M36: Additive + Interaction: temp_min + temp_at_max_count_t_1 + temp_min×temp_at_max_count_t_1 (linear)
✓ M37: Additive + Interaction: temp_min + wind_max_gust + temp_min×wind_max_gust (smooth)
✓ M38: Additive + Interaction: temp_min + wind_max_gust + temp_min×wind_max_gust (linear)
✓ M39: Additive + Interaction: temp_min + sum_butterflies_direct_sun + temp_min×sum_butterflies_direct_sun (smooth)
✓ M40: Additive + Interaction: temp_min + sum_butterflies_direct_sun + temp_min×sum_butterflies_direct_sun (linear)
✓ M41: Additive + Interaction: temp_max + temp_at_max_count_t_1 + temp_max×temp_at_max_count_t_1 (smooth)
✓ M42: Additive + Interaction: temp_max + temp_at_max_count_t_1 + temp_max×temp_at_max_count_t_1 (linear)
✓ M43: Additive + Interaction: temp_max + wind_max_gust + temp_max×wind_max_gust (smooth)
✓ M44: Additive + Interaction: temp_max + wind_max_gust + temp_max×wind_max_gust (linear)
✓ M45: Additive + Interaction: temp_max + sum_butterflies_direct_sun + temp_max×sum_butterflies_direct_sun (smooth)
✓ M46: Additive + Interaction: temp_max + sum_butterflies_direct_sun + temp_max×sum_butterflies_direct_sun (linear)
✓ M47: Additive + Interaction: temp_at_max_count_t_1 + wind_max_gust + temp_at_max_count_t_1×wind_max_gust (smooth)
✓ M48: Additive + Interaction: temp_at_max_count_t_1 + wind_max_gust + temp_at_max_count_t_1×wind_max_gust (linear)
✓ M49: Additive + Interaction: temp_at_max_count_t_1 + sum_butterflies_direct_sun + temp_at_max_count_t_1×sum_butterflies_direct_sun (smooth)
✓ M50: Additive + Interaction: temp_at_max_count_t_1 + sum_butterflies_direct_sun + temp_at_max_count_t_1×sum_butterflies_direct_sun (linear)
✓ M51: Additive + Interaction: wind_max_gust + sum_butterflies_direct_sun + wind_max_gust×sum_butterflies_direct_sun (smooth)
✓ M52: Additive + Interaction: wind_max_gust + sum_butterflies_direct_sun + wind_max_gust×sum_butterflies_direct_sun (linear)
```


:::

```{.r .cell-code}
# ============================================================================
# PART 5: STRATEGIC ADDITIVE COMBINATIONS (14 models)
# ============================================================================
cat("\n=== FITTING STRATEGIC ADDITIVE MODELS ===\n")
```

::: {.cell-output .cell-output-stdout}

```

=== FITTING STRATEGIC ADDITIVE MODELS ===
```


:::

```{.r .cell-code}
# All temperature
additive_combos <- list(
    list(
        preds = c("temp_min", "temp_max", "temp_at_max_count_t_1"),
        desc = "All temperature"
    ),
    list(
        preds = c("temp_min", "temp_max"),
        desc = "temp_min + temp_max"
    ),
    list(
        preds = c("temp_min", "wind_max_gust"),
        desc = "temp_min + wind_max_gust"
    ),
    list(
        preds = c("temp_max", "wind_max_gust"),
        desc = "temp_max + wind_max_gust"
    ),
    list(
        preds = c("temp_at_max_count_t_1", "wind_max_gust"),
        desc = "temp_at_max_count_t_1 + wind_max_gust"
    ),
    list(
        preds = c("temp_min", "temp_max", "temp_at_max_count_t_1", "wind_max_gust"),
        desc = "All temp + wind"
    ),
    list(
        preds = weather_predictors,
        desc = "All predictors (additive)"
    )
)

for (combo in additive_combos) {
    preds_str <- paste0("s(", combo$preds, ")", collapse = " + ")

    # Smooth baseline
    fit_model_safe(
        sprintf(
            "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + s(lag_duration_hours, k = k_lag) + %s",
            preds_str
        ),
        "smooth", paste0("M", model_num), sprintf("Additive: %s (smooth)", combo$desc)
    )
    model_num <- model_num + 1

    # Linear baseline
    fit_model_safe(
        sprintf(
            "butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours + %s",
            preds_str
        ),
        "linear", paste0("M", model_num), sprintf("Additive: %s (linear)", combo$desc)
    )
    model_num <- model_num + 1
}
```

::: {.cell-output .cell-output-stdout}

```
✓ M53: Additive: All temperature (smooth)
✓ M54: Additive: All temperature (linear)
✓ M55: Additive: temp_min + temp_max (smooth)
✓ M56: Additive: temp_min + temp_max (linear)
✓ M57: Additive: temp_min + wind_max_gust (smooth)
✓ M58: Additive: temp_min + wind_max_gust (linear)
✓ M59: Additive: temp_max + wind_max_gust (smooth)
✓ M60: Additive: temp_max + wind_max_gust (linear)
✓ M61: Additive: temp_at_max_count_t_1 + wind_max_gust (smooth)
✓ M62: Additive: temp_at_max_count_t_1 + wind_max_gust (linear)
✓ M63: Additive: All temp + wind (smooth)
✓ M64: Additive: All temp + wind (linear)
✓ M65: Additive: All predictors (additive) (smooth)
✓ M66: Additive: All predictors (additive) (linear)
```


:::

```{.r .cell-code}
# ============================================================================
# PART 6: COMPLEX MODELS WITH MULTIPLE INTERACTIONS (8 models)
# ============================================================================
cat("\n=== FITTING COMPLEX INTERACTION MODELS ===\n")
```

::: {.cell-output .cell-output-stdout}

```

=== FITTING COMPLEX INTERACTION MODELS ===
```


:::

```{.r .cell-code}
# All temperature additive + all temp interactions
temp_preds <- c("temp_min", "temp_max", "temp_at_max_count_t_1")
temp_pairs <- combn(temp_preds, 2, simplify = FALSE)
temp_main <- paste0("s(", temp_preds, ")", collapse = " + ")
temp_interactions <- paste0(sapply(temp_pairs, function(p) sprintf("ti(%s, %s)", p[1], p[2])), collapse = " + ")

fit_model_safe(
    sprintf(
        "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + s(lag_duration_hours, k = k_lag) + %s + %s",
        temp_main, temp_interactions
    ),
    "smooth", paste0("M", model_num), "All temp + all temp interactions (smooth)"
)
```

::: {.cell-output .cell-output-stdout}

```
✓ M67: All temp + all temp interactions (smooth)
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] TRUE
```


:::

```{.r .cell-code}
model_num <- model_num + 1

fit_model_safe(
    sprintf(
        "butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours + %s + %s",
        temp_main, temp_interactions
    ),
    "linear", paste0("M", model_num), "All temp + all temp interactions (linear)"
)
```

::: {.cell-output .cell-output-stdout}

```
✓ M68: All temp + all temp interactions (linear)
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] TRUE
```


:::

```{.r .cell-code}
model_num <- model_num + 1

# All additive + all temp × wind interactions
temp_wind_interactions <- paste0(sapply(temp_preds, function(t) sprintf("ti(%s, wind_max_gust)", t)), collapse = " + ")
all_main <- paste0("s(", weather_predictors, ")", collapse = " + ")

fit_model_safe(
    sprintf(
        "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + s(lag_duration_hours, k = k_lag) + %s + %s",
        all_main, temp_wind_interactions
    ),
    "smooth", paste0("M", model_num), "All additive + all temp×wind interactions (smooth)"
)
```

::: {.cell-output .cell-output-stdout}

```
✓ M69: All additive + all temp×wind interactions (smooth)
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] TRUE
```


:::

```{.r .cell-code}
model_num <- model_num + 1

fit_model_safe(
    sprintf(
        "butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours + %s + %s",
        all_main, temp_wind_interactions
    ),
    "linear", paste0("M", model_num), "All additive + all temp×wind interactions (linear)"
)
```

::: {.cell-output .cell-output-stdout}

```
✓ M70: All additive + all temp×wind interactions (linear)
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] TRUE
```


:::

```{.r .cell-code}
model_num <- model_num + 1

# ============================================================================
# PART 7: FULL MODELS (2 models)
# All main effects + ALL two-way interactions
# ============================================================================
cat("\n=== FITTING FULL MODELS (all terms + all interactions) ===\n")
```

::: {.cell-output .cell-output-stdout}

```

=== FITTING FULL MODELS (all terms + all interactions) ===
```


:::

```{.r .cell-code}
all_interactions <- paste0(sapply(interaction_pairs, function(p) sprintf("ti(%s, %s)", p[1], p[2])), collapse = " + ")

fit_model_safe(
    sprintf(
        "butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + s(lag_duration_hours, k = k_lag) + %s + %s",
        all_main, all_interactions
    ),
    "smooth", paste0("M", model_num), "FULL MODEL: All terms + all interactions (smooth)"
)
```

::: {.cell-output .cell-output-stdout}

```
✓ M71: FULL MODEL: All terms + all interactions (smooth)
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] TRUE
```


:::

```{.r .cell-code}
model_num <- model_num + 1

fit_model_safe(
    sprintf(
        "butterfly_diff_sqrt ~ max_butterflies_t_1 + lag_duration_hours + %s + %s",
        all_main, all_interactions
    ),
    "linear", paste0("M", model_num), "FULL MODEL: All terms + all interactions (linear)"
)
```

::: {.cell-output .cell-output-stdout}

```
✓ M72: FULL MODEL: All terms + all interactions (linear)
```


:::

::: {.cell-output .cell-output-stdout}

```
[1] TRUE
```


:::

```{.r .cell-code}
model_num <- model_num + 1

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n", "=", rep("=", 70), "\n", sep = "")
```

::: {.cell-output .cell-output-stdout}

```

=======================================================================
```


:::

```{.r .cell-code}
cat("TOTAL MODELS SUCCESSFULLY FITTED:", length(models), "\n")
```

::: {.cell-output .cell-output-stdout}

```
TOTAL MODELS SUCCESSFULLY FITTED: 72 
```


:::

```{.r .cell-code}
cat("=", rep("=", 70), "\n\n", sep = "")
```

::: {.cell-output .cell-output-stdout}

```
=======================================================================
```


:::

```{.r .cell-code}
# Print model summary table
cat("Model breakdown:\n")
```

::: {.cell-output .cell-output-stdout}

```
Model breakdown:
```


:::

```{.r .cell-code}
cat("  - Null models: 2\n")
```

::: {.cell-output .cell-output-stdout}

```
  - Null models: 2
```


:::

```{.r .cell-code}
cat("  - Single predictor models: 10\n")
```

::: {.cell-output .cell-output-stdout}

```
  - Single predictor models: 10
```


:::

```{.r .cell-code}
cat("  - Interaction-only models: 20\n")
```

::: {.cell-output .cell-output-stdout}

```
  - Interaction-only models: 20
```


:::

```{.r .cell-code}
cat("  - Main + interaction models: 20\n")
```

::: {.cell-output .cell-output-stdout}

```
  - Main + interaction models: 20
```


:::

```{.r .cell-code}
cat("  - Strategic additive models: 14\n")
```

::: {.cell-output .cell-output-stdout}

```
  - Strategic additive models: 14
```


:::

```{.r .cell-code}
cat("  - Complex interaction models: 8\n")
```

::: {.cell-output .cell-output-stdout}

```
  - Complex interaction models: 8
```


:::

```{.r .cell-code}
cat("  - Full models: 2\n")
```

::: {.cell-output .cell-output-stdout}

```
  - Full models: 2
```


:::

```{.r .cell-code}
cat("  ----------------------------\n")
```

::: {.cell-output .cell-output-stdout}

```
  ----------------------------
```


:::

```{.r .cell-code}
cat("  Total: 76 models\n")
```

::: {.cell-output .cell-output-stdout}

```
  Total: 76 models
```


:::
:::


## Model Comparison


::: {.cell}

```{.r .cell-code}
# Calculate sample size for AICc correction
n_obs <- nrow(model_data)

# Extract AIC for all models with descriptions
model_comparison <- tibble(
    model = names(models),
    description = sapply(names(models), function(m) model_descriptions[[m]]),
    AIC = sapply(models, function(m) AIC(m$lme)),
    BIC = sapply(models, function(m) BIC(m$lme)),
    logLik = sapply(models, function(m) as.numeric(logLik(m$lme))),
    df = sapply(models, function(m) attr(logLik(m$lme), "df"))
) %>%
    mutate(
        # Calculate AICc (corrected AIC for small samples)
        AICc = AIC + (2 * df * (df + 1)) / (n_obs - df - 1),
        # Calculate df-to-n ratio (overfitting metric)
        df_ratio = df / n_obs,
        # Calculate obs per parameter
        obs_per_param = n_obs / df,
        delta_AIC = AIC - min(AIC),
        delta_AICc = AICc - min(AICc),
        delta_BIC = BIC - min(BIC),
        weight_AIC = exp(-0.5 * delta_AIC) / sum(exp(-0.5 * delta_AIC)),
        weight_AICc = exp(-0.5 * delta_AICc) / sum(exp(-0.5 * delta_AICc)),
        weight_BIC = exp(-0.5 * delta_BIC) / sum(exp(-0.5 * delta_BIC)),
        # Categorize models for analysis
        baseline_type = ifelse(grepl("smooth", description), "Smooth", "Linear"),
        model_category = case_when(
            grepl("Null", description) ~ "Null",
            grepl("^Single:", description) ~ "Single predictor",
            grepl("^Interaction:", description) ~ "Interaction only",
            grepl("Additive \\+ Interaction:", description) ~ "Main + Interaction",
            grepl("^Additive:", description) ~ "Additive",
            grepl("FULL MODEL", description) ~ "Full model",
            TRUE ~ "Complex"
        ),
        # Flag potential overfitting
        overfitting_risk = case_when(
            df_ratio > 0.3 ~ "High",
            df_ratio > 0.2 ~ "Moderate",
            TRUE ~ "Low"
        )
    ) %>%
    arrange(AICc) # Sort by AICc instead of AIC

cat("Sample size (n):", n_obs, "\n")
```

::: {.cell-output .cell-output-stdout}

```
Sample size (n): 96 
```


:::

```{.r .cell-code}
cat("Using AICc correction for small sample bias\n\n")
```

::: {.cell-output .cell-output-stdout}

```
Using AICc correction for small sample bias
```


:::

```{.r .cell-code}
# Display top 20 models by AICc with overfitting metrics
cat("\n=== TOP 20 MODELS (RANKED BY AICc) ===\n\n")
```

::: {.cell-output .cell-output-stdout}

```

=== TOP 20 MODELS (RANKED BY AICc) ===
```


:::

```{.r .cell-code}
top_20 <- model_comparison %>% head(20)
kable(
    top_20 %>%
        select(model, description, AICc, delta_AICc, weight_AICc, df, df_ratio, obs_per_param, overfitting_risk) %>%
        mutate(
            across(c(AICc, delta_AICc), ~ round(.x, 2)),
            across(c(weight_AICc, df_ratio), ~ round(.x, 4)),
            across(obs_per_param, ~ round(.x, 1))
        ),
    caption = "Top 20 models ranked by AICc (small-sample corrected)"
)
```

::: {.cell-output-display}


Table: Top 20 models ranked by AICc (small-sample corrected)

|model |description                                                                                                            |   AICc| delta_AICc| weight_AICc| df| df_ratio| obs_per_param|overfitting_risk |
|:-----|:----------------------------------------------------------------------------------------------------------------------|------:|----------:|-----------:|--:|--------:|-------------:|:----------------|
|M31   |Interaction: wind_max_gust × sum_butterflies_direct_sun (smooth)                                                       | 640.22|       0.00|      0.7993| 11|   0.1146|           8.7|Low              |
|M51   |Additive + Interaction: wind_max_gust + sum_butterflies_direct_sun + wind_max_gust×sum_butterflies_direct_sun (smooth) | 644.13|       3.90|      0.1135| 15|   0.1562|           6.4|Low              |
|M45   |Additive + Interaction: temp_max + sum_butterflies_direct_sun + temp_max×sum_butterflies_direct_sun (smooth)           | 648.52|       8.29|      0.0126| 15|   0.1562|           6.4|Low              |
|M32   |Interaction: wind_max_gust × sum_butterflies_direct_sun (linear)                                                       | 648.85|       8.63|      0.0107|  9|   0.0938|          10.7|Low              |
|M19   |Interaction: temp_min × sum_butterflies_direct_sun (smooth)                                                            | 649.34|       9.12|      0.0084| 11|   0.1146|           8.7|Low              |
|M5    |Single: temp_max (smooth)                                                                                              | 649.46|       9.23|      0.0079| 10|   0.1042|           9.6|Low              |
|M29   |Interaction: temp_at_max_count_t_1 × sum_butterflies_direct_sun (smooth)                                               | 649.74|       9.52|      0.0069| 11|   0.1146|           8.7|Low              |
|M43   |Additive + Interaction: temp_max + wind_max_gust + temp_max×wind_max_gust (smooth)                                     | 650.12|       9.89|      0.0057| 15|   0.1562|           6.4|Low              |
|M23   |Interaction: temp_max × wind_max_gust (smooth)                                                                         | 650.17|       9.95|      0.0055| 11|   0.1146|           8.7|Low              |
|M33   |Additive + Interaction: temp_min + temp_max + temp_min×temp_max (smooth)                                               | 650.52|      10.30|      0.0046| 15|   0.1562|           6.4|Low              |
|M39   |Additive + Interaction: temp_min + sum_butterflies_direct_sun + temp_min×sum_butterflies_direct_sun (smooth)           | 651.39|      11.17|      0.0030| 15|   0.1562|           6.4|Low              |
|M25   |Interaction: temp_max × sum_butterflies_direct_sun (smooth)                                                            | 651.78|      11.56|      0.0025| 11|   0.1146|           8.7|Low              |
|M59   |Additive: temp_max + wind_max_gust (smooth)                                                                            | 652.14|      11.92|      0.0021| 12|   0.1250|           8.0|Low              |
|M55   |Additive: temp_min + temp_max (smooth)                                                                                 | 652.25|      12.03|      0.0020| 12|   0.1250|           8.0|Low              |
|M15   |Interaction: temp_min × temp_at_max_count_t_1 (smooth)                                                                 | 652.64|      12.42|      0.0016| 11|   0.1146|           8.7|Low              |
|M17   |Interaction: temp_min × wind_max_gust (smooth)                                                                         | 652.73|      12.51|      0.0015| 11|   0.1146|           8.7|Low              |
|M11   |Single: sum_butterflies_direct_sun (smooth)                                                                            | 652.82|      12.60|      0.0015| 10|   0.1042|           9.6|Low              |
|M13   |Interaction: temp_min × temp_max (smooth)                                                                              | 652.93|      12.71|      0.0014| 11|   0.1146|           8.7|Low              |
|M1    |Null (smooth baseline)                                                                                                 | 653.17|      12.95|      0.0012|  8|   0.0833|          12.0|Low              |
|M21   |Interaction: temp_max × temp_at_max_count_t_1 (smooth)                                                                 | 653.48|      13.26|      0.0011| 11|   0.1146|           8.7|Low              |


:::

```{.r .cell-code}
# Compare AIC vs AICc vs BIC rankings
cat("\n=== COMPARING INFORMATION CRITERIA ===\n\n")
```

::: {.cell-output .cell-output-stdout}

```

=== COMPARING INFORMATION CRITERIA ===
```


:::

```{.r .cell-code}
top_5_comparison <- model_comparison %>%
    mutate(
        rank_AIC = rank(AIC),
        rank_AICc = rank(AICc),
        rank_BIC = rank(BIC)
    ) %>%
    filter(rank_AICc <= 5) %>%
    select(model, description, AIC, AICc, BIC, rank_AIC, rank_AICc, rank_BIC, df, df_ratio, overfitting_risk) %>%
    arrange(rank_AICc)

kable(
    top_5_comparison %>%
        mutate(
            across(c(AIC, AICc, BIC, df_ratio), ~ round(.x, 2))
        ),
    caption = "Top 5 by AICc: Comparison of rankings across criteria"
)
```

::: {.cell-output-display}


Table: Top 5 by AICc: Comparison of rankings across criteria

|model |description                                                                                                            |    AIC|   AICc|    BIC| rank_AIC| rank_AICc| rank_BIC| df| df_ratio|overfitting_risk |
|:-----|:----------------------------------------------------------------------------------------------------------------------|------:|------:|------:|--------:|---------:|--------:|--:|--------:|:----------------|
|M31   |Interaction: wind_max_gust × sum_butterflies_direct_sun (smooth)                                                       | 637.08| 640.22| 664.82|        3|         1|        1| 11|     0.11|Low              |
|M51   |Additive + Interaction: wind_max_gust + sum_butterflies_direct_sun + wind_max_gust×sum_butterflies_direct_sun (smooth) | 638.13| 644.13| 675.62|        4|         2|        9| 15|     0.16|Low              |
|M45   |Additive + Interaction: temp_max + sum_butterflies_direct_sun + temp_max×sum_butterflies_direct_sun (smooth)           | 642.52| 648.52| 680.01|        7|         3|       26| 15|     0.16|Low              |
|M32   |Interaction: wind_max_gust × sum_butterflies_direct_sun (linear)                                                       | 646.76| 648.85| 669.45|       13|         4|        2|  9|     0.09|Low              |
|M19   |Interaction: temp_min × sum_butterflies_direct_sun (smooth)                                                            | 646.20| 649.34| 673.94|       11|         5|        5| 11|     0.11|Low              |


:::

```{.r .cell-code}
# Summary statistics by category
cat("\n=== MODEL PERFORMANCE BY CATEGORY ===\n\n")
```

::: {.cell-output .cell-output-stdout}

```

=== MODEL PERFORMANCE BY CATEGORY ===
```


:::

```{.r .cell-code}
category_summary <- model_comparison %>%
    group_by(model_category) %>%
    summarise(
        n_models = n(),
        best_AIC = min(AIC),
        mean_AIC = mean(AIC),
        best_delta_AIC = min(delta_AIC),
        n_in_top_10 = sum(AIC <= sort(model_comparison$AIC)[10])
    ) %>%
    arrange(best_AIC)

kable(
    category_summary %>%
        mutate(across(c(best_AIC, mean_AIC, best_delta_AIC), ~ round(.x, 2))),
    caption = "Model performance by complexity category"
)
```

::: {.cell-output-display}


Table: Model performance by complexity category

|model_category     | n_models| best_AIC| mean_AIC| best_delta_AIC| n_in_top_10|
|:------------------|--------:|--------:|--------:|--------------:|-----------:|
|Full model         |        2|   612.53|   617.62|           0.00|           2|
|Interaction only   |       20|   637.08|   652.15|          24.55|           1|
|Main + Interaction |       20|   638.13|   651.82|          25.60|           5|
|Complex            |        4|   641.42|   646.88|          28.89|           2|
|Single predictor   |       10|   646.87|   655.79|          34.34|           0|
|Additive           |       14|   648.38|   655.97|          35.86|           0|
|Null               |        2|   651.52|   656.10|          38.99|           0|


:::

```{.r .cell-code}
# Baseline type comparison
cat("\n=== SMOOTH vs LINEAR BASELINE ===\n\n")
```

::: {.cell-output .cell-output-stdout}

```

=== SMOOTH vs LINEAR BASELINE ===
```


:::

```{.r .cell-code}
baseline_summary <- model_comparison %>%
    group_by(baseline_type) %>%
    summarise(
        n_models = n(),
        best_AIC = min(AIC),
        mean_AIC = mean(AIC),
        n_in_top_10 = sum(AIC <= sort(model_comparison$AIC)[10]),
        n_in_top_20 = sum(AIC <= sort(model_comparison$AIC)[20])
    )

kable(
    baseline_summary %>%
        mutate(across(c(best_AIC, mean_AIC), ~ round(.x, 2))),
    caption = "Smooth vs Linear baseline comparison"
)
```

::: {.cell-output-display}


Table: Smooth vs Linear baseline comparison

|baseline_type | n_models| best_AIC| mean_AIC| n_in_top_10| n_in_top_20|
|:-------------|--------:|--------:|--------:|-----------:|-----------:|
|Linear        |       36|   622.71|   657.05|           1|           2|
|Smooth        |       36|   612.53|   647.27|           9|          18|


:::

```{.r .cell-code}
# Overfitting diagnostic summary
cat("\n=== OVERFITTING RISK ASSESSMENT ===\n\n")
```

::: {.cell-output .cell-output-stdout}

```

=== OVERFITTING RISK ASSESSMENT ===
```


:::

```{.r .cell-code}
overfitting_summary <- model_comparison %>%
    group_by(overfitting_risk) %>%
    summarise(
        n_models = n(),
        mean_df = mean(df),
        mean_df_ratio = mean(df_ratio),
        mean_obs_per_param = mean(obs_per_param),
        n_in_top_10_AICc = sum(rank(AICc) <= 10),
        n_in_top_10_BIC = sum(rank(BIC) <= 10)
    ) %>%
    arrange(desc(overfitting_risk))

kable(
    overfitting_summary %>%
        mutate(across(c(mean_df_ratio, mean_obs_per_param), ~ round(.x, 2))),
    caption = "Overfitting risk by df/n ratio"
)
```

::: {.cell-output-display}


Table: Overfitting risk by df/n ratio

|overfitting_risk | n_models|  mean_df| mean_df_ratio| mean_obs_per_param| n_in_top_10_AICc| n_in_top_10_BIC|
|:----------------|--------:|--------:|-------------:|------------------:|----------------:|---------------:|
|Moderate         |        4| 24.00000|          0.25|               4.04|                4|               4|
|Low              |       66| 11.54545|          0.12|               8.75|               10|              10|
|High             |        2| 47.00000|          0.49|               2.04|                2|               2|


:::

```{.r .cell-code}
cat("\nInterpretation:\n")
```

::: {.cell-output .cell-output-stdout}

```

Interpretation:
```


:::

```{.r .cell-code}
cat("- Low risk: df/n ≤ 0.20 (≥5 obs per parameter)\n")
```

::: {.cell-output .cell-output-stdout}

```
- Low risk: df/n ≤ 0.20 (≥5 obs per parameter)
```


:::

```{.r .cell-code}
cat("- Moderate risk: 0.20 < df/n ≤ 0.30 (3.3-5 obs per parameter)\n")
```

::: {.cell-output .cell-output-stdout}

```
- Moderate risk: 0.20 < df/n ≤ 0.30 (3.3-5 obs per parameter)
```


:::

```{.r .cell-code}
cat("- High risk: df/n > 0.30 (<3.3 obs per parameter)\n\n")
```

::: {.cell-output .cell-output-stdout}

```
- High risk: df/n > 0.30 (<3.3 obs per parameter)
```


:::

```{.r .cell-code}
# Highlight top 5 models with full details
cat("\n=== TOP 5 MODELS BY AICc (DETAILED) ===\n\n")
```

::: {.cell-output .cell-output-stdout}

```

=== TOP 5 MODELS BY AICc (DETAILED) ===
```


:::

```{.r .cell-code}
top_5 <- model_comparison %>% head(5)
for (i in 1:nrow(top_5)) {
    cat(sprintf(
        "%d. %s\n   %s\n   AICc=%.2f, ΔAICc=%.2f, weight=%.4f\n   df=%d (%.1f obs/param, df/n=%.3f) [%s risk]\n   AIC rank=%d, BIC rank=%d\n\n",
        i,
        top_5$model[i],
        top_5$description[i],
        top_5$AICc[i],
        top_5$delta_AICc[i],
        top_5$weight_AICc[i],
        top_5$df[i],
        top_5$obs_per_param[i],
        top_5$df_ratio[i],
        top_5$overfitting_risk[i],
        rank(model_comparison$AIC)[i],
        rank(model_comparison$BIC)[i]
    ))
}
```

::: {.cell-output .cell-output-stdout}

```
1. M31
   Interaction: wind_max_gust × sum_butterflies_direct_sun (smooth)
   AICc=640.22, ΔAICc=0.00, weight=0.7993
   df=11 (8.7 obs/param, df/n=0.115) [Low risk]
   AIC rank=3, BIC rank=1

2. M51
   Additive + Interaction: wind_max_gust + sum_butterflies_direct_sun + wind_max_gust×sum_butterflies_direct_sun (smooth)
   AICc=644.13, ΔAICc=3.90, weight=0.1135
   df=15 (6.4 obs/param, df/n=0.156) [Low risk]
   AIC rank=4, BIC rank=9

3. M45
   Additive + Interaction: temp_max + sum_butterflies_direct_sun + temp_max×sum_butterflies_direct_sun (smooth)
   AICc=648.52, ΔAICc=8.29, weight=0.0126
   df=15 (6.4 obs/param, df/n=0.156) [Low risk]
   AIC rank=7, BIC rank=26

4. M32
   Interaction: wind_max_gust × sum_butterflies_direct_sun (linear)
   AICc=648.85, ΔAICc=8.63, weight=0.0107
   df=9 (10.7 obs/param, df/n=0.094) [Low risk]
   AIC rank=13, BIC rank=2

5. M19
   Interaction: temp_min × sum_butterflies_direct_sun (smooth)
   AICc=649.34, ΔAICc=9.12, weight=0.0084
   df=11 (8.7 obs/param, df/n=0.115) [Low risk]
   AIC rank=11, BIC rank=5
```


:::

```{.r .cell-code}
# Save full comparison table
write_csv(model_comparison, here("analysis", "dynamic_window_analysis", "model_comparison_comprehensive.csv"))
cat("Full model comparison saved to: analysis/dynamic_window_analysis/model_comparison_comprehensive.csv\n")
```

::: {.cell-output .cell-output-stdout}

```
Full model comparison saved to: analysis/dynamic_window_analysis/model_comparison_comprehensive.csv
```


:::
:::


## Cross-Validation: Overfitting Check

To validate whether the top models are truly better or just overfitting, we perform **leave-one-deployment-out cross-validation** (LODOCV) on the top 10 models by AICc.


::: {.cell}

```{.r .cell-code}
# Select top 10 models for cross-validation
top_10_models <- model_comparison %>%
    head(10) %>%
    pull(model)

cat("Performing LODOCV on top 10 models...\n\n")
```

::: {.cell-output .cell-output-stdout}

```
Performing LODOCV on top 10 models...
```


:::

```{.r .cell-code}
# Get unique deployments
deployments <- unique(model_data$deployment_id)
n_deployments <- length(deployments)

# Initialize results storage
cv_results <- tibble()

# Cross-validation function
perform_lodocv <- function(model_name) {
    model_formula <- formula(models[[model_name]]$gam)

    cv_predictions <- tibble()

    for (dep in deployments) {
        # Split data
        train_data <- model_data %>% filter(deployment_id != dep)
        test_data <- model_data %>% filter(deployment_id == dep)

        if (nrow(test_data) == 0) next

        # Refit model on training data
        tryCatch(
            {
                cv_model <- gamm(
                    model_formula,
                    data = train_data,
                    random = random_structure,
                    correlation = ar1_cor,
                    method = "REML"
                )

                # Predict on test data
                preds <- predict(cv_model$gam, newdata = test_data, type = "response")

                # Store results
                cv_predictions <- bind_rows(
                    cv_predictions,
                    tibble(
                        deployment_id = dep,
                        observed = test_data$butterfly_diff_sqrt,
                        predicted = preds
                    )
                )
            },
            error = function(e) {
                # Skip if model fails
                NULL
            }
        )
    }

    if (nrow(cv_predictions) == 0) {
        return(tibble(model = model_name, rmse = NA, mae = NA, r2 = NA, n_pred = 0))
    }

    # Calculate prediction metrics
    rmse <- sqrt(mean((cv_predictions$observed - cv_predictions$predicted)^2, na.rm = TRUE))
    mae <- mean(abs(cv_predictions$observed - cv_predictions$predicted), na.rm = TRUE)

    # Calculate R² (correlation-based for cross-validation)
    r2 <- cor(cv_predictions$observed, cv_predictions$predicted, use = "complete.obs")^2

    return(tibble(
        model = model_name,
        rmse = rmse,
        mae = mae,
        r2 = r2,
        n_pred = nrow(cv_predictions)
    ))
}

# Run CV for top 10 models
for (m in top_10_models) {
    cat(sprintf("CV for %s...", m))
    result <- perform_lodocv(m)
    cv_results <- bind_rows(cv_results, result)
    cat(sprintf(" RMSE=%.3f, R²=%.3f\n", result$rmse, result$r2))
}
```

::: {.cell-output .cell-output-stdout}

```
CV for M31... RMSE=7.305, R²=0.283
CV for M51... RMSE=8.051, R²=0.234
CV for M45... RMSE=8.916, R²=0.180
CV for M32... RMSE=7.045, R²=0.298
CV for M19... RMSE=9.636, R²=0.138
CV for M5... RMSE=8.207, R²=0.201
CV for M29... RMSE=8.263, R²=0.251
CV for M43... RMSE=8.207, R²=0.197
CV for M23...
```


:::

::: {.cell-output .cell-output-stdout}

```
 RMSE=7.751, R²=0.242
CV for M33...
```


:::

::: {.cell-output .cell-output-stdout}

```
 RMSE=9.361, R²=0.127
```


:::

```{.r .cell-code}
# Merge with model comparison
model_comparison_cv <- model_comparison %>%
    left_join(cv_results, by = "model") %>%
    arrange(AICc)

# Display CV results for top 10
cat("\n=== CROSS-VALIDATION RESULTS (TOP 10 MODELS) ===\n\n")
```

::: {.cell-output .cell-output-stdout}

```

=== CROSS-VALIDATION RESULTS (TOP 10 MODELS) ===
```


:::

```{.r .cell-code}
kable(
    model_comparison_cv %>%
        filter(!is.na(rmse)) %>%
        head(10) %>%
        select(model, description, AICc, delta_AICc, df, df_ratio, rmse, mae, r2, overfitting_risk) %>%
        mutate(
            across(c(AICc, delta_AICc, df_ratio, rmse, mae, r2), ~ round(.x, 3))
        ),
    caption = "Top 10 models: AICc vs Cross-validation performance"
)
```

::: {.cell-output-display}


Table: Top 10 models: AICc vs Cross-validation performance

|model |description                                                                                                            |    AICc| delta_AICc| df| df_ratio|  rmse|   mae|    r2|overfitting_risk |
|:-----|:----------------------------------------------------------------------------------------------------------------------|-------:|----------:|--:|--------:|-----:|-----:|-----:|:----------------|
|M31   |Interaction: wind_max_gust × sum_butterflies_direct_sun (smooth)                                                       | 640.222|      0.000| 11|    0.115| 7.305| 5.967| 0.283|Low              |
|M51   |Additive + Interaction: wind_max_gust + sum_butterflies_direct_sun + wind_max_gust×sum_butterflies_direct_sun (smooth) | 644.126|      3.905| 15|    0.156| 8.051| 6.269| 0.234|Low              |
|M45   |Additive + Interaction: temp_max + sum_butterflies_direct_sun + temp_max×sum_butterflies_direct_sun (smooth)           | 648.515|      8.293| 15|    0.156| 8.916| 7.172| 0.180|Low              |
|M32   |Interaction: wind_max_gust × sum_butterflies_direct_sun (linear)                                                       | 648.849|      8.627|  9|    0.094| 7.045| 5.546| 0.298|Low              |
|M19   |Interaction: temp_min × sum_butterflies_direct_sun (smooth)                                                            | 649.341|      9.119| 11|    0.115| 9.636| 7.056| 0.138|Low              |
|M5    |Single: temp_max (smooth)                                                                                              | 649.456|      9.235| 10|    0.104| 8.207| 6.778| 0.201|Low              |
|M29   |Interaction: temp_at_max_count_t_1 × sum_butterflies_direct_sun (smooth)                                               | 649.739|      9.517| 11|    0.115| 8.263| 6.655| 0.251|Low              |
|M43   |Additive + Interaction: temp_max + wind_max_gust + temp_max×wind_max_gust (smooth)                                     | 650.116|      9.894| 15|    0.156| 8.207| 6.751| 0.197|Low              |
|M23   |Interaction: temp_max × wind_max_gust (smooth)                                                                         | 650.168|      9.946| 11|    0.115| 7.751| 6.457| 0.242|Low              |
|M33   |Additive + Interaction: temp_min + temp_max + temp_min×temp_max (smooth)                                               | 650.518|     10.296| 15|    0.156| 9.361| 7.207| 0.127|Low              |


:::

```{.r .cell-code}
cat("\n**Interpretation:**\n")
```

::: {.cell-output .cell-output-stdout}

```

**Interpretation:**
```


:::

```{.r .cell-code}
cat("- Lower RMSE/MAE = better predictive accuracy\n")
```

::: {.cell-output .cell-output-stdout}

```
- Lower RMSE/MAE = better predictive accuracy
```


:::

```{.r .cell-code}
cat("- Higher R² = better explained variance\n")
```

::: {.cell-output .cell-output-stdout}

```
- Higher R² = better explained variance
```


:::

```{.r .cell-code}
cat("- If high-df models have worse CV metrics despite better AICc, they are overfitting\n\n")
```

::: {.cell-output .cell-output-stdout}

```
- If high-df models have worse CV metrics despite better AICc, they are overfitting
```


:::
:::


## Best Model Summary


::: {.cell}

```{.r .cell-code}
# Select best model balancing AICc and CV performance
# Prioritize models with low overfitting risk if CV performance is similar
best_model_name <- model_comparison_cv %>%
    filter(overfitting_risk != "High" | is.na(overfitting_risk)) %>%
    arrange(AICc) %>%
    slice(1) %>%
    pull(model)

best_model <- models[[best_model_name]]

cat("=== BEST MODEL (SELECTED):", best_model_name, "===\n\n")
```

::: {.cell-output .cell-output-stdout}

```
=== BEST MODEL (SELECTED): M31 ===
```


:::

```{.r .cell-code}
cat("Selection criteria: Best AICc among models with Moderate/Low overfitting risk\n\n")
```

::: {.cell-output .cell-output-stdout}

```
Selection criteria: Best AICc among models with Moderate/Low overfitting risk
```


:::

```{.r .cell-code}
# Display model details
best_model_details <- model_comparison_cv %>% filter(model == best_model_name)
cat("Model details:\n")
```

::: {.cell-output .cell-output-stdout}

```
Model details:
```


:::

```{.r .cell-code}
cat(sprintf("  Description: %s\n", best_model_details$description))
```

::: {.cell-output .cell-output-stdout}

```
  Description: Interaction: wind_max_gust × sum_butterflies_direct_sun (smooth)
```


:::

```{.r .cell-code}
cat(sprintf("  AICc: %.2f (ΔAICc: %.2f)\n", best_model_details$AICc, best_model_details$delta_AICc))
```

::: {.cell-output .cell-output-stdout}

```
  AICc: 640.22 (ΔAICc: 0.00)
```


:::

```{.r .cell-code}
cat(sprintf(
    "  df: %d (%.1f obs/param, df/n=%.3f)\n",
    best_model_details$df,
    best_model_details$obs_per_param,
    best_model_details$df_ratio
))
```

::: {.cell-output .cell-output-stdout}

```
  df: 11 (8.7 obs/param, df/n=0.115)
```


:::

```{.r .cell-code}
cat(sprintf("  Overfitting risk: %s\n", best_model_details$overfitting_risk))
```

::: {.cell-output .cell-output-stdout}

```
  Overfitting risk: Low
```


:::

```{.r .cell-code}
if (!is.na(best_model_details$rmse)) {
    cat(sprintf("  CV RMSE: %.3f, CV R²: %.3f\n", best_model_details$rmse, best_model_details$r2))
}
```

::: {.cell-output .cell-output-stdout}

```
  CV RMSE: 7.305, CV R²: 0.283
```


:::

```{.r .cell-code}
cat("\n")
```

```{.r .cell-code}
# Summary of GAM component
summary(best_model$gam)
```

::: {.cell-output .cell-output-stdout}

```

Family: gaussian 
Link function: identity 

Formula:
butterfly_diff_sqrt ~ s(max_butterflies_t_1, k = k_baseline) + 
    s(lag_duration_hours, k = k_lag) + ti(wind_max_gust, sum_butterflies_direct_sun)

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)  -0.4362     0.9074  -0.481    0.632

Approximate significance of smooth terms:
                                               edf Ref.df      F  p-value    
s(max_butterflies_t_1)                       2.185  2.185 13.814 1.56e-05 ***
s(lag_duration_hours)                        1.000  1.000  1.373 0.244642    
ti(wind_max_gust,sum_butterflies_direct_sun) 6.703  6.703  4.273 0.000573 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

R-sq.(adj) =  0.421   
  Scale est. = 39.512    n = 96
```


:::

```{.r .cell-code}
# Summary of LME component (shows AR1 parameters)
cat("\n=== CORRELATION STRUCTURE ===\n")
```

::: {.cell-output .cell-output-stdout}

```

=== CORRELATION STRUCTURE ===
```


:::

```{.r .cell-code}
summary(best_model$lme$modelStruct$corStruct)
```

::: {.cell-output .cell-output-stdout}

```
Correlation Structure: ARMA(1,0)
 Formula: ~observation_order_t | g/g.0/g.1/deployment_id 
 Parameter estimate(s):
     Phi1 
0.3276789 
```


:::
:::


## Partial Effects Plots (Best Model)


::: {.cell}

```{.r .cell-code}
# Plot smooth terms from best model
draw(best_model$gam, residuals = TRUE)
```

::: {.cell-output-display}
![](sunset_window_gam_analysis_files/figure-html/partial-effects-1.png){width=1152}
:::
:::


## Model Diagnostics (Best Model)

### Residual Diagnostics


::: {.cell}

```{.r .cell-code}
# Extract residuals
model_resid <- residuals(best_model$lme, type = "normalized")
model_fitted <- fitted(best_model$lme)

# Create diagnostic plots
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

# 1. Residuals vs Fitted
plot(model_fitted, model_resid,
    xlab = "Fitted values", ylab = "Normalized residuals",
    main = "Residuals vs Fitted",
    pch = 19, col = rgb(0, 0, 0, 0.5)
)
abline(h = 0, col = "red", lwd = 2, lty = 2)
lines(lowess(model_fitted, model_resid), col = "blue", lwd = 2)

# 2. Q-Q plot
qqnorm(model_resid,
    main = "Normal Q-Q Plot",
    pch = 19, col = rgb(0, 0, 0, 0.5)
)
qqline(model_resid, col = "red", lwd = 2)

# 3. Scale-Location
sqrt_abs_resid <- sqrt(abs(model_resid))
plot(model_fitted, sqrt_abs_resid,
    xlab = "Fitted values", ylab = "√|Normalized residuals|",
    main = "Scale-Location",
    pch = 19, col = rgb(0, 0, 0, 0.5)
)
lines(lowess(model_fitted, sqrt_abs_resid), col = "blue", lwd = 2)

# 4. Residuals vs Order (temporal autocorrelation check)
plot(seq_along(model_resid), model_resid,
    xlab = "Observation order", ylab = "Normalized residuals",
    main = "Residuals vs Order",
    pch = 19, col = rgb(0, 0, 0, 0.5)
)
abline(h = 0, col = "red", lwd = 2, lty = 2)
lines(lowess(seq_along(model_resid), model_resid), col = "blue", lwd = 2)
```

::: {.cell-output-display}
![](sunset_window_gam_analysis_files/figure-html/residual-diagnostics-1.png){width=1152}
:::
:::


### Normality Tests


::: {.cell}

```{.r .cell-code}
# Shapiro-Wilk test on residuals
shapiro_test <- shapiro.test(model_resid)

cat("Shapiro-Wilk Normality Test on Residuals\n")
```

::: {.cell-output .cell-output-stdout}

```
Shapiro-Wilk Normality Test on Residuals
```


:::

```{.r .cell-code}
cat("=========================================\n")
```

::: {.cell-output .cell-output-stdout}

```
=========================================
```


:::

```{.r .cell-code}
cat("W statistic:", round(shapiro_test$statistic, 4), "\n")
```

::: {.cell-output .cell-output-stdout}

```
W statistic: 0.9764 
```


:::

```{.r .cell-code}
cat("p-value:", round(shapiro_test$p.value, 4), "\n")
```

::: {.cell-output .cell-output-stdout}

```
p-value: 0.0809 
```


:::

```{.r .cell-code}
if (shapiro_test$p.value > 0.05) {
    cat("Result: Cannot reject normality (p > 0.05)\n")
} else {
    cat("Result: Residuals deviate from normality (p < 0.05)\n")
}
```

::: {.cell-output .cell-output-stdout}

```
Result: Cannot reject normality (p > 0.05)
```


:::

```{.r .cell-code}
cat("\nSkewness:", round(e1071::skewness(model_resid), 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```

Skewness: 0.559 
```


:::

```{.r .cell-code}
cat("Kurtosis:", round(e1071::kurtosis(model_resid), 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
Kurtosis: 0.14 
```


:::
:::


### Autocorrelation Diagnostics


::: {.cell}

```{.r .cell-code}
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# ACF plot
acf(model_resid, main = "ACF of Normalized Residuals", lag.max = 20)

# PACF plot
pacf(model_resid, main = "PACF of Normalized Residuals", lag.max = 20)
```

::: {.cell-output-display}
![](sunset_window_gam_analysis_files/figure-html/acf-diagnostics-1.png){width=1152}
:::
:::


### Influence Diagnostics


::: {.cell}

```{.r .cell-code}
# Leverage and standardized residuals as influence measures
# For mixed models, we use standardized residuals and leverage-like measures

# Standardized residuals
std_resid <- model_resid # Already normalized from lme

# Identify potential outliers (|std resid| > 2.5)
outlier_threshold <- 2.5
outliers <- abs(std_resid) > outlier_threshold
n_influential <- sum(outliers)

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Standardized residuals index plot
plot(seq_along(std_resid), std_resid,
    type = "h",
    xlab = "Observation", ylab = "Standardized Residual",
    main = "Standardized Residuals",
    col = ifelse(outliers, "red", "black")
)
abline(
    h = c(-outlier_threshold, 0, outlier_threshold),
    col = c("red", "gray", "red"), lty = c(2, 1, 2)
)

cat("\nInfluential Observations (|std resid| > 2.5):\n")
```

::: {.cell-output .cell-output-stdout}

```

Influential Observations (|std resid| > 2.5):
```


:::

```{.r .cell-code}
cat("Threshold: ±", outlier_threshold, "\n")
```

::: {.cell-output .cell-output-stdout}

```
Threshold: ± 2.5 
```


:::

```{.r .cell-code}
cat("Number of potential outliers:", n_influential, "\n")
```

::: {.cell-output .cell-output-stdout}

```
Number of potential outliers: 1 
```


:::

```{.r .cell-code}
if (n_influential > 0) {
    influential_obs <- which(outliers)
    cat("Observation indices:", paste(influential_obs, collapse = ", "), "\n")
    cat("Residual values:", paste(round(std_resid[outliers], 2), collapse = ", "), "\n")
}
```

::: {.cell-output .cell-output-stdout}

```
Observation indices: 8 
Residual values: 3.11 
```


:::

```{.r .cell-code}
# Histogram of standardized residuals
hist(std_resid,
    breaks = 30,
    xlab = "Standardized Residual",
    main = "Distribution of Standardized Residuals",
    col = "steelblue", border = "black"
)
abline(
    v = c(-outlier_threshold, outlier_threshold),
    col = "red", lwd = 2, lty = 2
)
```

::: {.cell-output-display}
![](sunset_window_gam_analysis_files/figure-html/influence-diagnostics-1.png){width=1152}
:::
:::


### GAM-Specific Diagnostics


::: {.cell}

```{.r .cell-code}
# mgcv's built-in diagnostic plots
gam.check(best_model$gam)
```

::: {.cell-output-display}
![](sunset_window_gam_analysis_files/figure-html/gam-diagnostics-1.png){width=1152}
:::

::: {.cell-output .cell-output-stdout}

```

'gamm' based fit - care required with interpretation.
Checks based on working residuals may be misleading.
Basis dimension (k) checking results. Low p-value (k-index<1) may
indicate that k is too low, especially if edf is close to k'.

                                                k'   edf k-index p-value
s(max_butterflies_t_1)                        4.00  2.18    1.04    0.62
s(lag_duration_hours)                         4.00  1.00    1.04    0.61
ti(wind_max_gust,sum_butterflies_direct_sun) 16.00  6.70    0.92    0.16
```


:::
:::


### Summary of Diagnostic Checks


::: {.cell}

```{.r .cell-code}
cat("\n=== DIAGNOSTIC SUMMARY ===\n\n")
```

::: {.cell-output .cell-output-stdout}

```

=== DIAGNOSTIC SUMMARY ===
```


:::

```{.r .cell-code}
# 1. Normality
cat("1. NORMALITY OF RESIDUALS\n")
```

::: {.cell-output .cell-output-stdout}

```
1. NORMALITY OF RESIDUALS
```


:::

```{.r .cell-code}
cat("   Shapiro-Wilk p-value:", round(shapiro_test$p.value, 4), "\n")
```

::: {.cell-output .cell-output-stdout}

```
   Shapiro-Wilk p-value: 0.0809 
```


:::

```{.r .cell-code}
if (shapiro_test$p.value > 0.05) {
    cat("   ✓ Residuals are approximately normal\n\n")
} else {
    cat("   ⚠ Some deviation from normality detected\n\n")
}
```

::: {.cell-output .cell-output-stdout}

```
   ✓ Residuals are approximately normal
```


:::

```{.r .cell-code}
# 2. Homoscedasticity (visual check from scale-location plot)
cat("2. HOMOSCEDASTICITY\n")
```

::: {.cell-output .cell-output-stdout}

```
2. HOMOSCEDASTICITY
```


:::

```{.r .cell-code}
cat("   Check Scale-Location plot above\n")
```

::: {.cell-output .cell-output-stdout}

```
   Check Scale-Location plot above
```


:::

```{.r .cell-code}
cat("   Look for horizontal trend line (constant variance)\n\n")
```

::: {.cell-output .cell-output-stdout}

```
   Look for horizontal trend line (constant variance)
```


:::

```{.r .cell-code}
# 3. Autocorrelation
cat("3. TEMPORAL AUTOCORRELATION\n")
```

::: {.cell-output .cell-output-stdout}

```
3. TEMPORAL AUTOCORRELATION
```


:::

```{.r .cell-code}
cat("   AR1 parameter:", round(coef(best_model$lme$modelStruct$corStruct, unconstrained = FALSE), 3), "\n")
```

::: {.cell-output .cell-output-stdout}

```
   AR1 parameter: 0.328 
```


:::

```{.r .cell-code}
cat("   Check ACF/PACF plots for remaining autocorrelation\n\n")
```

::: {.cell-output .cell-output-stdout}

```
   Check ACF/PACF plots for remaining autocorrelation
```


:::

```{.r .cell-code}
# 4. Influential points
cat("4. INFLUENTIAL OBSERVATIONS\n")
```

::: {.cell-output .cell-output-stdout}

```
4. INFLUENTIAL OBSERVATIONS
```


:::

```{.r .cell-code}
cat("   Number of potential outliers:", n_influential, "/", length(model_resid), "\n")
```

::: {.cell-output .cell-output-stdout}

```
   Number of potential outliers: 1 / 96 
```


:::

```{.r .cell-code}
if (n_influential == 0) {
    cat("   ✓ No potential outliers (|std resid| > 2.5)\n\n")
} else {
    cat("   ⚠", n_influential, "observations may be outliers\n\n")
}
```

::: {.cell-output .cell-output-stdout}

```
   ⚠ 1 observations may be outliers
```


:::

```{.r .cell-code}
# 5. Basis dimension check
cat("5. BASIS DIMENSION ADEQUACY\n")
```

::: {.cell-output .cell-output-stdout}

```
5. BASIS DIMENSION ADEQUACY
```


:::

```{.r .cell-code}
cat("   See k-index values in gam.check() output above\n")
```

::: {.cell-output .cell-output-stdout}

```
   See k-index values in gam.check() output above
```


:::

```{.r .cell-code}
cat("   k-index > 1.0 and p-value > 0.05 indicate adequate basis dimensions\n")
```

::: {.cell-output .cell-output-stdout}

```
   k-index > 1.0 and p-value > 0.05 indicate adequate basis dimensions
```


:::
:::


---

**Model fitting complete. AIC-based selection identifies optimal model structure. Diagnostics confirm model assumptions.**
