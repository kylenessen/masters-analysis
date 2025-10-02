# Dynamic Window Analysis - Implementation Summary

**Date:** October 1, 2025
**Script:** `data_prep_dynamic_windows.py`

## Purpose

This analysis addresses Francis's feedback about temporal alignment of weather predictors. The original analysis used fixed 6am-6pm windows, which didn't align with the biological timing of butterfly responses to weather.

## Key Changes from Original Analysis

### 1. **Dynamic Time Windows**
Instead of fixed 6am-6pm windows, weather metrics are calculated from:
- **Start:** Time when maximum butterfly count occurred on day t-1
- **End:** Either +24 hours OR last observation time on day t (functional sunset)

### 2. **Overnight Weather Included**
Weather metrics now include **24/7 data** (temperature and wind during nighttime hours)
- Previous: Only daytime (6am-6pm)
- Now: Full 24-hour coverage including overnight conditions

### 3. **Functional Sunset**
Last observation time on each day serves as "functional sunset" - the time when it became too dark for observers to count butterflies

## Two Analyses Generated

### A. 24-Hour Window (`monarch_daily_lag_analysis_24hr_window.csv`)
- Window: `time_of_max_t_1` → `time_of_max_t_1 + 24 hours`
- Fixed duration: Always 24.0 hours
- Tests: "Do weather conditions in the 24 hours after peak count predict next day's abundance?"

**Statistics:**
- 103 lag pairs
- Mean duration: 24.00 hours (SD: 0.00)
- Median data completeness: 0.971

### B. Sunset Window (`monarch_daily_lag_analysis_sunset_window.csv`)
- Window: `time_of_max_t_1` → `last_observation_time_t` (functional sunset)
- Variable duration: 22.5 - 34.9 hours (mean: 29.6 hours)
- Tests: "Do weather conditions from peak count until roosting decisions are made predict next day's abundance?"

**Statistics:**
- 103 lag pairs
- Mean duration: 29.56 hours (SD: 3.79)
- Median data completeness: 1.000

## Key Metric Differences

Comparing mean values across all observations:

| Metric | 24hr Window | Sunset Window | % Difference |
|--------|-------------|---------------|--------------|
| temp_max_t_1 | 19.64°C | 20.47°C | +4.2% |
| temp_min_t_1 | 8.36°C | 8.31°C | -0.6% |
| wind_max_gust_t_1 | 4.20 m/s | 4.39 m/s | +4.6% |
| sum_butterflies_direct_sun_t_1 | 102.27 | 132.80 | +29.8% |

**Sunset windows capture:**
- More daytime observations (longer window → more daylight hours)
- Slightly higher maximum temperatures
- Slightly higher wind gusts
- ~30% more sun exposure data

## New Columns Added

All columns from the original analysis PLUS:

- `lag_duration_hours`: Length of the weather window in hours
- `metrics_complete`: Overall data completeness (0-1 scale, geometric mean)
- `temp_data_coverage`: Proportion of expected temperature observations present
- `wind_data_coverage`: Proportion of expected wind observations present
- `butterfly_data_coverage`: Proportion of expected butterfly observations present
- `window_start`: Timestamp when weather window begins
- `window_end`: Timestamp when weather window ends

## Data Completeness

### Temperature Coverage
- Mean: 100% for both analyses
- Temperature data is nearly complete across all windows

### Wind Coverage
- 24hr: 95.4% complete
- Sunset: 95.2% complete
- Minor gaps in wind data but very high overall coverage

### Overall Metrics Completeness
- 24hr: 93.7% (mean), 97.1% (median)
- Sunset: 95.9% (mean), 100% (median)

Sunset windows generally have higher completeness because they align with actual observation periods.

## Biological Interpretation

### Why Sunset Window is Preferred
Francis argues (and I agree) that the sunset window is most biologically appropriate:

1. **Roosting decisions occur by sunset** - butterflies must choose where to spend the night before dark
2. **Response timing** - butterflies can only respond to weather *after* it occurs
3. **Measurement alignment** - next day's peak count reflects overnight site fidelity, which is determined by where they roosted at sunset

### Test Hypothesis
"Do weather conditions experienced between peak abundance and roosting time predict whether butterflies abandon the roost?"

If wind/temperature cause abandonment, we should see:
- **Negative relationship** between wind speed and next day's count
- Effect should be clearer with properly aligned time windows

## Next Steps

1. **Run GAM analysis** on both datasets (same model structure as original)
2. **Compare results:**
   - Do time-aligned windows show clearer wind effects?
   - Does wind now show the predicted negative relationship?
   - Are 24hr and sunset results similar (robustness check)?
3. **Report to Francis:**
   - Sunset window as primary analysis
   - 24hr window as sensitivity check
   - Interpretation of any differences

## Technical Notes

- Night butterfly observations excluded from counts (spurious data)
- Sun exposure calculated only during daylight hours (isNight=False)
- Temperature and wind include overnight periods (24/7 coverage)
- Data filtered to days with 15-25 photos (consistent observation effort)
- Consecutive days only (non-consecutive day pairs excluded)

## Files Generated

```
data/monarch_daily_lag_analysis_24hr_window.csv    (103 rows × 52 columns)
data/monarch_daily_lag_analysis_sunset_window.csv  (103 rows × 52 columns)
```

Both files ready for GAM analysis in R using the same model structure as before.
