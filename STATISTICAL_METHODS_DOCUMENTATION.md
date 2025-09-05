# Statistical Analysis Methods Documentation
## For Master's Thesis: Disruptive Winds to Overwintering Monarch Butterflies

### Dataset Overview
- **Total observations**: 1,894 lag pairs (after data processing)
- **Deployment-days**: 115 unique deployment-day combinations
- **Sites**: Spring Canyon and UDMH (2023-2024 season only)
- **Temporal resolution**: 30-minute intervals (±5 minute tolerance)
- **Sample includes**: All deployments including SC8

---

## 1. Response Variable Construction

### Lag Analysis Framework
- **Comparison structure**: Butterfly counts at time t compared with time t-30 minutes
- **Tolerance window**: ±5 minutes for matching observations
- **Response variable**: Change in butterfly abundance between consecutive time points

### Transformation Decision
- **Cube root transformation**: Applied to the difference in butterfly counts
- **Formula**: `butterfly_difference_cbrt = sign(difference) × |difference|^(1/3)`
- **Rationale**: 
  - Achieved more normal distribution compared to raw differences
  - Preserves direction of change (increase/decrease)
  - Reduces influence of extreme values while maintaining interpretability
  - Alternative transformations considered: log transformation (excluded due to zero values)

### Data Filtering
- **Zero-pair exclusion**: Removed observation pairs where both time points had zero butterflies
- **Rationale**: These pairs provide no information about butterfly response to environmental conditions
- **Impact**: Reduced dataset from potential ~2,500 pairs to 1,894 meaningful observations

---

## 2. Predictor Variable Selection

### Environmental Variables Included

#### Wind Variables
- **Selected**: `max_gust` - Maximum wind gust speed during 30-minute interval
- **Rationale**: Most directly tests the hypothesis that strong winds disrupt monarchs
- **Excluded wind metrics**:
  - `avg_sustained`: Highly correlated with max_gust (r > 0.9)
  - `mode_gust`: Highly correlated with max_gust (r > 0.9)
  - `gust_sd`: Highly correlated with other wind metrics (r > 0.75)
- **Decision rule**: When correlation > 0.75, retained the most biologically interpretable variable

#### Temperature
- **Selected**: `temperature_avg` - Average temperature between time t and t-lag
- **Calculation**: Mean of temperature at both time points
- **Rationale**: Captures thermal conditions during the transition period

#### Solar Exposure
- **Selected**: `butterflies_direct_sun_t_lag` - Number of butterflies in direct sunlight at previous time point
- **Rationale**: Tests whether solar exposure influences subsequent movement patterns

#### Temporal Variables
- **Selected**: `time_within_day_t` - Minutes since first observation of the day
- **Rationale**: Captures diurnal activity patterns
- **Selected**: `observation_order_within_day_t` - Sequential observation number within day
- **Purpose**: Used for AR1 correlation structure

### Control Variable
- **`total_butterflies_t_lag`**: Total butterfly count at previous time point
- **Dual purpose**:
  1. Controls for baseline abundance effects
  2. Distinguishes proportional vs. absolute change
- **Interpretation when included**: Models test effects on proportional change
- **Interpretation when excluded**: Models test effects on absolute change

### Variables Not Available
The following variables were not measured and therefore could not be included:
- Humidity
- Barometric pressure
- Precipitation
- Wind direction

---

## 3. Model Selection Strategy

### Comprehensive Model Comparison
- **Total models tested**: 47 candidate models
- **Selection criterion**: Akaike Information Criterion (AIC)
- **Comparison metrics**: Delta AIC and AIC weights

### Model Categories Tested

#### Two Fundamental Frameworks
1. **With lag term** (24 models): Include `total_butterflies_t_lag` to test proportional change
2. **Without lag term** (23 models): Exclude lag term to test absolute change

#### Within Each Framework, Tested:
1. **Null models**: Baseline for comparison
2. **Single predictor models**: Each environmental variable alone
3. **Additive models**: Combinations of 2-3 predictors
4. **Interaction models**: 
   - Two-way interactions
   - Three-way interactions
   - All possible combinations
5. **Smooth term models**: Non-linear relationships using GAM smoothers
6. **Mixed models**: Combinations of smooth and linear terms

### Smooth Term Application
- **Variables with smooth terms in candidate models**:
  - `total_butterflies_t_lag` (when included)
  - `max_gust`
  - `temperature_avg`
  - `butterflies_direct_sun_t_lag`
  - `time_within_day_t`
- **Note**: Not all variables received smooth terms in all models; this was part of the model selection process

### Example Model Specifications
```r
# Linear main effects only
M7: butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust + temperature_avg + butterflies_direct_sun_t_lag

# With interactions
M15: butterfly_difference_cbrt ~ total_butterflies_t_lag + max_gust * temperature_avg * butterflies_direct_sun_t_lag

# With smooth terms (best performing models typically had this structure)
M22: butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(temperature_avg) + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)

# Full smooth model
M23: butterfly_difference_cbrt ~ s(total_butterflies_t_lag) + s(max_gust) + s(temperature_avg) + s(butterflies_direct_sun_t_lag) + s(time_within_day_t)
```

---

## 4. Model Structure

### Random Effects Structure
All models included the following random effects:
```r
random = list(deployment_id = ~1, Observer = ~1, deployment_day = ~1)
```
- **Deployment random effect**: Accounts for site-specific variation
- **Observer random effect**: Controls for inter-observer differences in counting
- **Deployment-day random effect**: Accounts for day-to-day variation

### Temporal Autocorrelation
```r
correlation = corAR1(form = ~ observation_order_within_day_t | deployment_day)
```
- **Structure**: First-order autoregressive (AR1)
- **Rationale**: Accounts for temporal dependence between consecutive observations within the same day
- **Grouping**: Applied within each deployment-day
- **Note**: Only AR1 was tested; higher-order correlations not evaluated

### Model Fitting
- **Method**: Restricted Maximum Likelihood (REML)
- **Package**: `mgcv::gamm()` for GAM with mixed effects
- **Convergence**: All 47 models successfully converged

---

## 5. Model Diagnostics and Validation

### Required Diagnostics (per Francis meeting notes)
1. **Residual plots**: Must explicitly discuss linear structure in residuals
2. **Acknowledgment**: Residual patterns indicate some structure not captured by models
3. **Interpretation**: Possibly an artifact of the binning counting strategy

### Model Performance Metrics
- **Best model R²**: Approximately 5.61% (using smooth terms)
- **Baseline R²**: Approximately 2.74% (linear terms only)
- **Interpretation**: Low R² values expected given high natural variability in butterfly behavior

---

## 6. Key Results Summary

### Best Model Selection
- Models with smooth terms consistently outperformed linear models
- Best models included `total_butterflies_t_lag`, indicating proportional change is more appropriate
- Wind (max_gust) appeared in only 1 of top 5 models and was non-significant (p = 0.218)
- Temperature and diurnal patterns emerged as strongest predictors

### Biological Interpretation Framework
- **Wind hypothesis**: Not supported - wind does not appear to be a primary driver of short-term movement
- **Temperature effects**: Non-linear relationships suggest thermal optima
- **Diurnal patterns**: Strong effects consistent with thermoregulatory behavior
- **Solar exposure**: Significant effects on butterfly movement patterns

---

## 7. Statistical Software and Reproducibility

### Software Versions
- **R version**: [To be specified]
- **Key packages**:
  - `mgcv`: GAM fitting
  - `nlme`: Mixed effects components
  - `tidyverse`: Data manipulation
  - `lubridate`: Temporal data handling

### Data Processing Pipeline
1. **Python script** (`data_prep_lag.py`): 
   - Processes JSON deployment files
   - Creates lag pairs
   - Integrates temperature and wind data
   - Exports to `monarch_analysis_lag30min.csv`

2. **R analysis** (`monarch_gam_analysis.qmd`):
   - Loads processed data
   - Fits 47 candidate models
   - Performs AIC comparison
   - Generates diagnostic plots

### Reproducibility Notes
- All code available in GitHub repository
- Random seed not required (no stochastic elements)
- Complete data processing pipeline documented

---

## 8. Justification for Methodological Decisions

### Why GAMMs?
1. **Non-linear relationships**: Ecological responses rarely linear
2. **Flexible framework**: Can test both linear and non-linear hypotheses
3. **Mixed effects**: Properly accounts for nested data structure
4. **Temporal correlation**: Handles autocorrelation within days

### Why 30-minute intervals?
1. **Biological relevance**: Captures meaningful changes in butterfly behavior
2. **Environmental accuracy**: Weather conditions remain relatively stable
3. **Statistical power**: Provides sufficient observations for analysis
4. **Practical constraints**: Balances accuracy with data processing effort

### Why Include Models With and Without Lag Term?
1. **Exploratory approach**: Uncertain a priori whether proportional or absolute change more appropriate
2. **Biological uncertainty**: Both interpretations potentially meaningful
3. **Model selection**: Let AIC determine which framework better explains data
4. **Result**: Models with lag term performed better, suggesting proportional change more relevant

---

## Notes for Methods Writer

### Critical Points to Emphasize
1. The cube root transformation is applied to the DIFFERENCE, not total abundance
2. Wind was tested comprehensively but not significant
3. Model selection was systematic and hypothesis-driven
4. Low R² values are expected and do not indicate poor model performance
5. AR1 structure controls for within-day temporal autocorrelation only

### Potential Limitations to Acknowledge
1. Residual structure suggests some patterns not captured
2. Counting strategy (binning) may contribute to residual patterns
3. 2D photographic analysis provides conservative estimates
4. Partial days included in analysis (not all deployment-days are complete)

### Key Statistics
- Sample size: 1,894 observations
- Deployment-days: 115
- Models tested: 47
- Correlation threshold for exclusion: 0.75
- Lag interval: 30 minutes (±5 minute tolerance)
- Best model includes: smooth terms for all predictors except when testing linear hypotheses