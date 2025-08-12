# H3 Analysis Summary: Wind Effects Scale with Intensity

## Research Question
Do wind effects on monarch butterfly abundance increase proportionally with wind intensity, showing dose-response relationships that might be missed by simple threshold or linear approaches?

## Key Findings

### Primary Result: **H3 Not Supported**
- **No dose-response relationships detected**: Multiple analytical approaches revealed no significant scaling of wind effects with intensity
- **Linear models perform best**: Simple linear relationships performed as well as complex non-linear alternatives
- **Consistent null findings**: Results robust across polynomial models, GAMs, threshold sensitivity, and interaction terms

### Analytical Approaches Tested

#### 1. Polynomial Models
- **Quadratic effects**: No significant wind² terms
- **Cubic effects**: No significant wind³ terms  
- **Model comparison**: Linear models performed as well as higher-order polynomials

#### 2. Generalized Additive Models (GAMs)
- **Flexible smoothing**: Allowed for any non-linear wind relationships
- **Smooth function**: Wind smooth terms were non-significant
- **Visual inspection**: Smooth curves remained essentially flat around zero effect

#### 3. Threshold Sensitivity Analysis
- **Multiple thresholds**: Tested alternative wind speed thresholds (1.0 to 3.5 m/s)
- **Consistent results**: No threshold revealed significant wind effects
- **Robustness**: Findings insensitive to threshold selection

#### 4. Intensity Categories
- **Categorical approach**: Compared None/Low/Medium/High wind exposure
- **No step functions**: No significant differences between intensity categories
- **Uniform effects**: All categories showed similar (non-significant) relationships

#### 5. Interaction Models
- **Wind × Temperature**: No significant interactions
- **Wind × Sunlight**: No significant interactions
- **Context independence**: Wind effects don't depend on other environmental conditions

## Statistical Results Summary

### Model Performance
- **Best model**: Simple linear model (same as H1/H2)
- **AIC comparison**: Complex models showed no improvement over linear approaches
- **Effect sizes**: All wind coefficients near zero across all model types
- **P-values**: Consistently non-significant (p > 0.05) across approaches

### Diagnostic Quality
- **GAM residuals**: Appropriate behavior, no systematic patterns
- **DHARMa checks**: Passed overdispersion and normality tests
- **Temporal autocorrelation**: Successfully controlled with lagged terms

## Biological Interpretation

### Absence of Dose-Response
The lack of intensity scaling suggests:

1. **Threshold independence**: Monarch responses don't follow simple dose-response models at any measured wind speed
2. **Behavioral buffering**: Monarchs may actively adjust to wind conditions without changing roost abundance
3. **Non-linear resilience**: If critical thresholds exist, they're beyond our data range (~5 m/s maximum observed)
4. **Scale misalignment**: Meaningful wind effects may occur at different temporal or spatial scales

### Integration with H1 and H2
The consistent findings create a coherent picture:
- **H1**: No 2 m/s threshold effects → **Robust conclusion**
- **H2**: No general wind disruption → **Confirmed**  
- **H3**: No intensity scaling → **Completes the picture**

This convergence across different analytical frameworks strengthens confidence in monarch wind resilience.

### Alternative Explanations
The null findings could indicate:
1. **Measurement limitations**: Single-point wind sensors may miss spatial heterogeneity
2. **Response variable**: 2D counts may miss 3D cluster reorganization
3. **Temporal windows**: Critical effects may occur at seconds-to-minutes or hours-to-days scales
4. **Environmental complexity**: Wind effects may depend on unmeasured factors (humidity, precipitation, cluster size)

## Conservation and Research Implications

### For Monarch Conservation
1. **Habitat priorities**: Thermal regulation appears more critical than wind shelter
2. **Site selection**: Focus on temperature stability rather than wind protection
3. **Climate resilience**: Populations may handle changing wind patterns better than expected
4. **Management strategies**: Wind breaks may be less important than thermal refugia

### For Research Direction
1. **Scale exploration**: Investigate different temporal windows (seconds to days)
2. **Spatial measurement**: Multiple wind sensors within roosts
3. **Behavioral focus**: Direct observation of individual responses to wind
4. **Alternative responses**: Test cluster density/structure rather than just abundance

## Technical Quality Assessment

### Methodological Strengths
1. **Comprehensive approach**: Multiple analytical methods tested systematically
2. **Appropriate models**: Count-based GLMMs handle data structure properly
3. **Robust validation**: Extensive diagnostics confirm model assumptions
4. **Sensitivity analysis**: Results tested across different specifications

### Advanced Techniques
- **GAM smoothing**: Flexible non-parametric approach captured any non-linear patterns
- **Polynomial expansion**: Systematic testing of higher-order relationships
- **Threshold scanning**: Comprehensive sensitivity to threshold choice
- **Interaction exploration**: Context-dependent effects ruled out

### Data Quality
- **Sample size**: 1,648 observations provide adequate power
- **Temporal control**: Autocorrelation properly addressed
- **Random effects**: Site and observer variation controlled
- **Missing data**: Complete cases approach conservative but appropriate

## Connection to Broader Hypothesis Framework

### H4 and H5 Implications
Given H1-H3 null findings:
- **H4** (roost abandonment): May need extreme event focus or different abandonment definitions
- **H5** (site fidelity): Long-term patterns may reveal effects not apparent in short intervals

### Methodological Foundation
H3 establishes:
- **Count-based modeling**: Effective approach for monarch abundance analysis
- **Temporal autocorrelation**: Critical control for time series data
- **Environmental controls**: Temperature consistently important, wind consistently not

## Limitations and Future Work

### Study Limitations
1. **Wind measurement scale**: Single-point measurements at limited locations
2. **Observation period**: Only 30-minute intervals tested systematically
3. **Sample scope**: Two sites, one season may limit generalizability
4. **Response definition**: Abundance counts may miss relevant behavioral changes

### Recommended Extensions
1. **Multi-scale analysis**: Test effects across seconds to days
2. **Spatial arrays**: Multiple wind sensors throughout roosts
3. **Behavioral metrics**: Movement patterns, cluster density, individual positioning
4. **Environmental interactions**: Complex multi-factor relationships

## Bottom Line

**H3 Hypothesis: "Wind effects scale with intensity"**
**Conclusion: Not Supported**

This comprehensive analysis using multiple advanced statistical approaches found no evidence that wind effects on monarch abundance scale with wind intensity. The consistent null findings across H1, H2, and H3 provide strong convergent evidence that monarch overwintering behavior is remarkably resilient to wind exposure within the range of conditions typically encountered.

**Scientific Value**: These null results are scientifically important as they challenge conventional assumptions about monarch environmental sensitivity and suggest conservation efforts should prioritize thermal habitat quality over wind protection.

**Methodological Contribution**: The analytical framework developed here provides a robust foundation for testing environmental effects on monarch behavior and can be adapted for other ecological questions.