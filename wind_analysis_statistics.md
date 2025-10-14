# Wind Effect Analysis Statistics

## Summary of Linear Regression Results

This document summarizes the statistical relationship between maximum wind speed and butterfly abundance changes for both the 30-minute interval analysis and the sunset-to-sunset (site fidelity) analysis.

---

## 30-Minute Interval Analysis

**Data:** Paired observations at 30-minute intervals
**Sample Size:** 1,894 observations
**Response Variable:** Butterfly abundance change (untransformed)
**Predictor Variable:** Maximum wind speed (m/s)

### Statistical Results:
- **Pearson Correlation (r):** 0.041
- **R-squared:** 0.0017 (0.17% of variance explained)
- **Regression Slope:** 1.19 butterflies per m/s (SE = 0.66)
- **P-value:** 0.073

### Interpretation:
No statistically significant linear relationship between wind speed and butterfly abundance change at the 30-minute timescale (p = 0.073). The correlation is negligible (r = 0.041), suggesting wind speed alone does not predict short-term butterfly movement patterns.

---

## Sunset-to-Sunset (Site Fidelity) Analysis

**Data:** Maximum cluster counts compared between consecutive sunset periods
**Sample Size:** 101 observations
**Response Variable:** Butterfly abundance change (95th percentile difference)
**Predictor Variable:** Maximum wind speed (m/s)

### Statistical Results:
- **Pearson Correlation (r):** 0.115
- **R-squared:** 0.013 (1.3% of variance explained)
- **Regression Slope:** 5.34 butterflies per m/s (SE = 4.63)
- **P-value:** 0.252

### Interpretation:
No statistically significant linear relationship between wind speed and day-to-day butterfly abundance change (p = 0.252). The correlation remains weak (r = 0.115), indicating wind speed alone does not predict site fidelity patterns between consecutive days.

---

## Comparison of Analyses

| Analysis | N | Correlation (r) | R² | Slope ± SE | P-value |
|----------|---|-----------------|-----|------------|---------|
| 30-minute intervals | 1,894 | 0.041 | 0.002 | 1.19 ± 0.66 | 0.073 |
| Sunset-to-sunset | 101 | 0.115 | 0.013 | 5.34 ± 4.63 | 0.252 |

### Key Findings:
1. **Neither analysis shows a significant linear relationship** between wind speed and butterfly abundance changes
2. **Weak correlations** in both cases (r < 0.12) suggest minimal linear association
3. **Large standard errors** relative to slope estimates indicate high uncertainty
4. The sunset analysis shows a slightly stronger correlation but with much higher variability due to smaller sample size
5. **Wind speed alone explains < 2% of variance** in butterfly abundance changes at both temporal scales

### Statistical Note:
These univariate analyses examine only the simple linear relationship between wind and butterfly changes. The GAM models in the full analysis incorporate non-linear relationships and interactions with other environmental variables, which may reveal more complex wind effects not captured by simple correlation.

---

*Generated: October 2025*
*Data source: 2023-2024 monarch overwintering season field observations*