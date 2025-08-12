# H2 Analysis Summary: Wind as a General Disruptive Force

## Research Question
Does wind exposure, regardless of specific threshold values, act as a disruptive force that reduces monarch butterfly abundance at overwintering roost sites?

## Key Findings

### Primary Result: **H2 Not Supported**
- **No significant wind effects**: None of the wind characteristics (sustained winds, gusts, wind variability) showed statistically significant effects on butterfly abundance (all p > 0.05)
- **Minimal effect sizes**: Rate ratios remained close to 1.0, indicating practically negligible impacts even when non-significant trends were observed
- **Robust across models**: Results consistent across different model specifications and wind metrics

### Wind Metrics Comparison
We tested three different wind characteristics:
1. **Sustained wind exposure**: Minutes with sustained winds > 2 m/s
2. **Gust wind exposure**: Minutes with wind gusts > 2 m/s  
3. **Wind variability**: Difference between gust and sustained wind exposure

**Finding**: No wind metric substantially outperformed others or revealed significant effects.

### What Actually Matters: Temperature
- **Highly significant positive effect** (p < 0.001) in all models
- **Larger effect sizes** than any wind metric
- **Consistent direction** across all specifications
- Confirms H1 finding that thermal regulation dominates over wind exposure

## Statistical Approach

### Method: Negative Binomial GLMMs
- **Response**: Raw abundance counts (avoiding H1's proportional change issues)
- **Family**: Negative binomial to handle overdispersion and zero-inflation
- **Random effects**: Camera location (`view_id`) and labeler identity
- **Temporal control**: Lagged abundance term to control autocorrelation
- **Sample size**: 1,648 observations across multiple sites

### Model Performance
- **Pseudo R²**: ~10% (similar to H1, indicating most variation remains unexplained)
- **Good diagnostics**: DHARMa residuals showed appropriate behavior
- **Temporal independence**: Autocorrelation successfully controlled with lagged term

## Biological Interpretation

### Monarch Resilience
The lack of support for H2 suggests:
1. **Robust roosting behavior**: Monarch clusters are more wind-resistant than hypothesized
2. **Microhabitat selection**: Butterflies likely choose well-protected locations within roosts
3. **Behavioral adaptation**: Individual monarchs may adjust position within clusters during wind events
4. **Scale effects**: Wind impacts may occur at different temporal/spatial scales than measured

### Conservation Implications
- **Thermal habitat priority**: Temperature regulation appears more critical than wind shelter for roost quality
- **Climate resilience**: Populations may be more robust to changing wind patterns than assumed
- **Habitat management**: Focus should be on maintaining thermal refugia rather than just wind breaks

## Connection to H1 Findings

H2 analysis confirms and extends H1 results:
- **H1**: No support for 2 m/s threshold → **Confirmed robust**
- **H2**: No general wind disruption effects → **New evidence**
- **Convergent evidence**: Multiple approaches pointing to minimal wind effects

## Limitations

1. **Wind measurement**: Single-point measurements may not capture spatial wind variation within roosts
2. **Temporal resolution**: 30-minute intervals may miss immediate behavioral responses or longer-term effects
3. **Response variable**: 2D image counts may not capture 3D cluster reorganization
4. **Environmental interactions**: Wind effects may depend on other unmeasured conditions

## Technical Quality

### Strengths
- **Appropriate statistical framework**: Count models handle data structure properly
- **Comprehensive wind metrics**: Multiple characteristics tested systematically
- **Robust diagnostics**: Model assumptions verified with DHARMa residuals
- **Proper random effects**: Controls for site and observer variation

### Model Validation
- **Residual behavior**: No major departures from assumptions
- **Overdispersion**: Appropriately handled by negative binomial family
- **Temporal autocorrelation**: Successfully controlled with lagged abundance

## Next Steps

### For H3 Analysis
H2's null findings set up H3 (intensity scaling) by establishing that:
- Linear wind relationships are minimal at best
- If wind effects exist, they likely involve non-linear or threshold patterns
- Temperature consistently dominates environmental effects
- Count-based modeling approach works well

### Research Implications
- **Null results are scientifically valuable**: Challenge conventional assumptions about monarch wind sensitivity
- **Focus research elsewhere**: Other environmental factors likely more important
- **Methodological success**: Framework works well for monarch behavioral ecology questions

## Bottom Line

**H2 Hypothesis: "Wind acts as a disruptive force on monarch abundance"**
**Conclusion: Not Supported**

This analysis provides strong evidence that wind exposure, measured across multiple characteristics and intensities, does not significantly affect monarch butterfly abundance at 30-minute time scales. Temperature effects remain dominant, suggesting monarch conservation should prioritize thermal habitat quality over wind protection.

The consistent null finding across H1 and H2, using different analytical approaches, strengthens confidence that monarchs are more resilient to wind exposure than commonly assumed in the literature.