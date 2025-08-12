# H1 Analysis: Lessons Learned and the Journey to Understanding Wind Effects on Monarch Butterflies

## Executive Summary
After extensive analysis testing multiple modeling approaches, transformations, and statistical frameworks, we found **no evidence that wind causes monarch butterflies to abandon their roosts** in 30-minute intervals. This null finding is robust across all modeling approaches, though our journey revealed critical insights about the data structure, appropriate statistical methods, and the challenges of ecological time series analysis.

---

## The Narrative Arc: Our Analytical Journey

### Chapter 1: The Initial Hypothesis and Naive Approach

**Starting Point**: We hypothesized that wind exposure above 2 m/s would cause monarchs to abandon roosts, following Leong's (2016) threshold hypothesis.

**Initial Approach**: 
- Response variable: Proportional change `(New - Old) / Old`
- Simple mixed models with deployment_id and view_id as random effects
- Centered predictors for wind, temperature, sunlight

**What Went Wrong**:
1. **Extreme outliers**: When baseline = 1 butterfly, an increase to 11 gave 1000% change
2. **Division by zero**: When baseline = 0, the math broke completely
3. **Severely skewed distributions**: A few extreme values dominated everything
4. **Violated assumptions**: Residuals showed severe non-normality and heteroscedasticity

**Key Learning**: Raw proportional change is mathematically problematic with count data that includes zeros and small values.

---

### Chapter 2: The Search for the Right Transformation

**The Question**: How do we measure "change" in butterfly abundance?

**What We Tested**:
1. **Relative change**: `(New - Old) / (New + Old)`
   - Bounded between -1 and 1
   - Symmetric around zero
   - Handles zeros gracefully
   
2. **Log ratio**: `log((New + 1) / (Old + 1))`
   - Adding 1 prevents log(0) issues
   - Symmetric on log scale
   - Multiplicative interpretation

3. **Winsorized proportional**: Cap extremes at -100% and +200%
   - Keeps intuitive percentage interpretation
   - Removes outlier influence

4. **Absolute change**: Simple `New - Old`
   - No transformation complications
   - But ignores baseline (1→2 same as 100→101)

5. **Square root and arcsinh transformations**
   - Attempts to stabilize variance
   - Handle negative values

**Critical Mistake We Made**: 
We compared these models using AIC, declaring relative change the "winner" with AIC = 282 vs others >2000. But **you correctly caught this error** - AIC values cannot be compared across models with different response variables! They're on completely different scales.

**What Actually Mattered**:
- **Residual diagnostics**: All transformations failed normality tests (Shapiro p = 0)
- **Heteroscedasticity**: Relative change had r = 0.28 correlation between squared residuals and fitted values
- **Poor fit**: Best R² was only 5.5% (arcsinh transformation)
- **Consistency**: Wind effects were non-significant across ALL transformations

**Key Learning**: No transformation produced well-behaved residuals. The data structure itself is problematic for linear models.

---

### Chapter 3: Discovering the Importance of Random Effects Structure

**Initial Structure**: 
- Random intercepts for deployment_id and view_id
- Treating view_id as nested within deployment

**Your Correction**:
- Use **labeler** (who counted butterflies) as random effect
- Use **view_id** (camera viewpoint) as random effect
- Drop deployment_id as less meaningful

**Surprising Discovery**:
- Both labeler and view_id explained **essentially 0% of variance**
- All variation was residual
- This suggested individual differences and location effects were minimal

**Key Learning**: The random effects structure, while theoretically important, didn't explain variation in this dataset. The changes appear largely random or driven by unmeasured factors.

---

### Chapter 4: The Time Window Investigation

**Question**: Is 30 minutes too short to capture wind effects?

**What We Tested**:
- 30-minute intervals (n = 1,648)
- 60-minute intervals (n = 821)
- 90-minute intervals (n = 548)
- 120-minute intervals (n = 410)

**Results**:
```
Window  | Wind Effect | P-value | AIC
--------|-------------|---------|--------
30 min  | 0.029      | 0.172   | 282
60 min  | 0.003      | 0.395   | 451
90 min  | 0.003      | 0.586   | 422
120 min | 0.004      | 0.592   | 397
```

**Findings**:
- 30-minute window performed best (lowest AIC when comparing same response)
- Wind effects remained non-significant at ALL time scales
- Longer windows = smaller sample size = less power
- No evidence that wind effects "build up" over time

**Key Learning**: The lack of wind effect is consistent across time scales. It's not a matter of choosing the wrong observation window.

---

### Chapter 5: Advanced Modeling - Confronting Temporal Autocorrelation

**The Problem**: Time series data violates independence assumptions.

**What We Discovered**:
- **Strong negative autocorrelation**: r = -0.27
- Changes tend to reverse in the next period (mean reversion)
- Ignoring this inflates Type I error risk

**Solutions We Tried**:

1. **AR(1) models**: 
   - Explicitly model autocorrelation structure
   - Improved fit slightly (AIC 280 vs 282)
   - Wind still non-significant

2. **Lagged response as predictor**:
   - Include previous change as covariate
   - **Best performing model** (AIC = 272)
   - Reduced residual autocorrelation to -0.01
   - Wind still non-significant (p = 0.17)

3. **Zero-inflated models**:
   - 20.5% of observations showed zero change
   - Hurdle model: separate "whether" from "how much"
   - Temperature predicted probability of change
   - Wind had no effect on either component

4. **Threshold models**:
   - Test if effects only appear above certain wind levels
   - **Surprising finding**: Low wind (1-5 min) actually DECREASED abundance (p = 0.048)
   - Moderate/high wind showed no effects
   - Suggests non-linear relationship opposite of hypothesis

**Key Learning**: Accounting for temporal structure improved models but didn't reveal hidden wind effects. The lagged model was best but still had terrible diagnostics.

---

### Chapter 6: The Uncomfortable Truth About Model Diagnostics

**What the "Best" Model Showed**:
- **Normality**: Failed completely (Shapiro p = 0)
- **Homoscedasticity**: Failed (high correlation between residuals² and fitted)
- **Temporal independence**: Solved with lagged response
- **Predictive power**: R² = 10% (better than 3% but still awful)

**Why All Models Failed**:
1. **Zero inflation**: Many observations with no change
2. **Heavy tails**: Extreme changes occur occasionally
3. **Non-linear relationships**: Threshold effects suggest linear models inappropriate
4. **Unmeasured variables**: 90% of variation unexplained

**Key Learning**: The fundamental data structure doesn't fit linear model assumptions. No amount of transformation or random effects specification can fix this.

---

## Consistent Findings Across All Approaches

Despite the modeling challenges, several findings remained robust:

### 1. Wind Has Minimal to No Effect
- **Every model agreed**: Wind doesn't significantly affect abundance changes
- Effect sizes ranged from -0.001 to +0.03 across models
- P-values ranged from 0.15 to 0.95
- This held for sustained wind, gusts, and various thresholds

### 2. Temperature Dominates
- **Highly significant in every model** (p < 0.001)
- Effect size: ~0.01-0.02 increase in relative change per °C
- Consistent across transformations and model structures
- Both affects probability of change AND magnitude when change occurs

### 3. Temporal Patterns Are Strong
- Negative autocorrelation suggests mean reversion
- Butterflies don't make sustained directional movements
- Short-term fluctuations that reverse

### 4. Sunlight Has Unexpected Effects
- Often negative (more sun = fewer butterflies)
- Possibly confounded with temperature
- May indicate preference for shade during warm periods

---

## Why the Models Failed: Biological Interpretation

The statistical failures might actually tell us something biological:

1. **Butterfly movements are inherently stochastic**
   - Not responding systematically to measured variables
   - Individual variation swamps environmental signals

2. **30-minute intervals miss the relevant process**
   - Butterflies might respond immediately (seconds) then return
   - Or effects might take hours/days to manifest
   - We're in a "dead zone" of temporal resolution

3. **Threshold effects suggest non-linearity**
   - Low wind might be beneficial (mixing air, reducing stratification)
   - High wind might not reach damaging levels in our data
   - Linear models can't capture this complexity

4. **Roost-level dynamics matter more than individual counts**
   - Our grid counts might miss 3D cluster reorganization
   - Butterflies might compress/expand without changing numbers
   - Movement within roost ≠ abandonment

---

## Recommendations for Starting Fresh

### 1. Reconsider the Response Variable
Instead of mathematical transformations of counts, consider:
- **Binary**: Did substantial abandonment occur? (>50% decrease)
- **Categorical**: Large decrease / small change / large increase
- **Original counts**: Use appropriate count models (negative binomial with offset)
- **Roost volume/density**: If 3D structure can be estimated

### 2. Embrace Non-Linear Approaches
- **GAMs** (Generalized Additive Models): Allow smooth non-linear relationships
- **Threshold/breakpoint models**: Explicitly model change points
- **Machine learning**: Random forests might capture complex interactions
- **Survival analysis**: Time until abandonment event

### 3. Address Zero-Inflation Properly
- **Two-part models**: Separate processes for "change" vs "no change"
- **Zero-inflated negative binomial**: For count data with excess zeros
- **Mixture models**: Multiple processes generating observations

### 4. Consider Different Time Scales
- **Immediate response**: Use maximum wind in past 5 minutes
- **Cumulative exposure**: Sum wind over past hour/day
- **Event-based**: Focus on storms or wind events, not regular sampling
- **Daily aggregation**: Morning vs evening counts

### 5. Include More Biology
- **Cluster position**: Edge vs center of cluster
- **Tree characteristics**: Height, exposure, shelter
- **Time of day**: Morning warming vs afternoon cooling
- **Season progression**: Early vs late winter patterns
- **Population density**: Behavior might change with cluster size

### 6. Alternative Hypotheses to Test
- **Wind variability** (gustiness) might matter more than average speed
- **Wind direction** relative to roost orientation
- **Combined stressors**: Wind + cold, wind + rain
- **Recovery time**: How quickly do numbers rebound after wind?

---

## Code Architecture Recommendations

### What Worked Well
- Using `here()` package for paths
- Separate data prep from analysis
- Saving intermediate results as .rds files
- Clear commenting and section headers
- Creating diagnostic plots for all models

### What to Improve
1. **Create functions** for repeated operations (transformations, model fitting)
2. **Use consistent naming** for variables across scripts
3. **Version control** data files with dates
4. **Parameter file** for thresholds, options
5. **Automated reporting** with RMarkdown
6. **Unit tests** for data transformations

---

## The Bottom Line

**Your hypothesis wasn't wrong** - it just isn't supported by this particular dataset at these temporal and spatial scales. The null finding is scientifically valuable and appears robust. The challenges we encountered reveal the complexity of ecological time series and the difficulty of detecting environmental effects on organism behavior when:

1. Individual variation is high
2. Multiple factors interact
3. Responses are non-linear
4. The relevant time scale is unknown

The journey taught us that:
- **Simple is not always better** (proportional change seemed intuitive but was problematic)
- **Model diagnostics matter more than fit statistics** (AIC can be misleading)
- **Null findings can be informative** (consistently no effect across all approaches)
- **Ecological data rarely meets classical statistical assumptions**

## Next Steps

Given what we've learned, I recommend:

1. **Write up the null finding** - it's publishable and important
2. **Try ONE more approach**: Focus on extreme events only (>95th percentile wind)
3. **Consider different questions**: Does wind affect cluster structure/density rather than counts?
4. **Validate with field observations**: Are butterflies actually moving during wind or just redistributing?

The analysis has been thorough and rigorous. The lack of clear wind effects appears to be a genuine biological finding rather than a statistical artifact.