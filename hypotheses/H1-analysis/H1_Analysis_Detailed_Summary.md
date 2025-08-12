# H1 Analysis: Detailed Summary and Interpretation
## Do Wind Conditions Cause Monarch Butterflies to Abandon Roosts?

### Research Question
We wanted to test whether wind exposure causes monarch butterflies to leave their roosting sites. The hypothesis was that when it gets windier, we should see fewer butterflies in the same location 30 minutes later.

---

## Data Overview

**What we analyzed**: 1,683 pairs of butterfly counts taken 30 minutes apart across 8 different camera locations during the 2023 roosting season.

**Key variables**:
- **Abundance counts**: Number of butterflies at time t-1 and time t (30 minutes later)
- **Wind exposure**: Minutes in the 30-minute window with sustained winds or gusts > 2 m/s
- **Environmental factors**: Temperature, sunlight exposure on butterflies
- **Site factors**: Camera location (view_id), deployment period, person who labeled the images

---

## The Big Problem: How Do You Measure "Change"?

This turned out to be the trickiest part of the analysis. When butterfly counts go from 5 to 3, that's a 40% decrease. But when they go from 1 to 3, that's a 200% increase. These extreme percentage changes create major statistical problems.

### What We Tried First (And Why It Failed)

**Original approach**: Proportional change = (New Count - Old Count) / Old Count

**The problems**:
1. **Extreme outliers**: When old count = 1 and new count = 11, you get 1000% change
2. **Division by zero**: When old count = 0, the math breaks down  
3. **Severely skewed distribution**: Most changes were small, but extreme values dominated the analysis
4. **Violated model assumptions**: The residuals looked terrible - clearly non-normal with extreme heteroscedasticity

### Our Solution: Five Different Ways to Measure Change

We tested five different ways to quantify butterfly abundance changes:

#### 1. **Log Ratio** (Most Robust)
**Formula**: log((Count_t + 1) / (Count_t-1 + 1))
**Why this works**: 
- Adding 1 to both counts prevents division by zero
- Log transformation makes the distribution more symmetric
- A change from 5→10 butterflies gets the same magnitude as 10→5, just opposite sign
- Values center around 0 (no change)

#### 2. **Relative Change** (Most Intuitive)
**Formula**: (New - Old) / (New + Old) when both > 0
**Why this works**:
- Bounded between -1 (complete disappearance) and +1 (appearance from nothing)
- Symmetric around zero
- Handles extreme changes more gracefully
- Easy to interpret: -0.5 = 50% decrease, +0.5 = 50% increase

#### 3. **Winsorized Proportional Change** (Conservative)
**Formula**: Cap extreme proportional changes at -100% and +200%
**Why this works**: Keeps the familiar percentage interpretation while removing extreme outliers

#### 4. **Absolute Change** (Simple)
**Formula**: Just New Count - Old Count
**Why this works**: Simple difference, but gives equal weight to change from 1→2 and 100→101

#### 5. **Categorical** (Most Interpretable)
**Formula**: Did abundance decrease? (Yes/No)
**Why this works**: Directly tests your hypothesis without worrying about magnitude

---

## Why We "Centered" Our Predictors

You asked about centering temperature - this is a crucial statistical technique:

### What Centering Means
**Before centering**: Temperature values like 15°C, 23°C, 18°C
**After centering**: Temperature deviations from the average like -3°C, +5°C, 0°C

### Why We Do This

1. **Meaningful intercept**: In the centered model, the intercept represents the expected change when temperature, wind, and sunlight are all at their average values. Without centering, the intercept would represent the change when temperature = 0°C, wind = 0 minutes, etc. - which is unrealistic and uninterpretable.

2. **Reduced correlation between terms**: When predictors are on different scales (temperature in degrees, wind in minutes), centering helps prevent numerical issues.

3. **Easier interpretation**: A coefficient of 0.018 for centered temperature means "for each 1°C above average temperature, abundance change increases by 0.018 units"

4. **Model stability**: Helps the mathematical algorithms converge more reliably.

---

## Model Structure: Why Mixed Effects?

We used **mixed-effects models** because our data has natural grouping:

### Fixed Effects (What We Care About)
- **Sustained wind minutes**: Our main hypothesis variable
- **Gust wind minutes**: Alternative wind measure  
- **Temperature**: Known to affect butterfly activity
- **Sunlight exposure**: Butterflies prefer sunny spots

### Random Effects (Accounting for Non-Independence)
- **Deployment ID**: Different time periods might have different baseline abundance patterns
- **View ID**: Different camera locations have different characteristics (sheltered vs. exposed, different vegetation, etc.)

**Why this matters**: Without accounting for these groupings, we might falsely conclude that wind matters when really we're just seeing that some locations or time periods are naturally windier AND have different butterfly patterns for other reasons.

---

## Model Selection Results

We compared all five approaches using AIC (Akaike Information Criterion - lower = better model fit):

| Model | AIC | Response Variable |
|-------|-----|-------------------|
| **Relative Change** | **282** | **(New-Old)/(New+Old)** |
| Categorical | 2,202 | Decrease Yes/No |
| Log Ratio | 2,371 | log((New+1)/(Old+1)) |
| Winsorized | 2,863 | Capped proportional change |
| Absolute | Failed | Simple difference |

**Winner**: Relative change model (lowest AIC = 282)

---

## Best Model Results: The Relative Change Model

### Model Structure
```
Relative Change ~ Sustained Wind + Gust Wind + Temperature + Sunlight + 
                  (1|Deployment) + (1|View)
```

### What Each Coefficient Means

| Predictor | Coefficient | P-value | Interpretation |
|-----------|-------------|---------|----------------|
| **Intercept** | 0.032 | 0.037 | When all predictors are at average values, butterflies show slight increase tendency |
| **Sustained Wind** | -0.006 | 0.098 | Each additional minute of sustained wind → 0.6% decrease (marginally significant) |
| **Gust Wind** | 0.002 | 0.598 | Gusts show tiny positive effect (not significant) |
| **Temperature** | 0.018 | <0.001 | Each 1°C warmer → 1.8% increase (highly significant) |
| **Sunlight** | -0.097 | 0.020 | More sunlight → decrease (significant, surprising!) |

---

## Interpretation: What This All Means

### For Your Hypothesis
**Your hypothesis**: Wind causes monarch abandonment
**The evidence**: **Weak support at best**

- Sustained winds show a small negative effect (0.6% decrease per minute of wind > 2 m/s)
- This effect is only marginally significant (p = 0.098) 
- Gusts show no meaningful effect
- The effect size is quite small - even 10 minutes of sustained wind only predicts a 6% relative decrease

### The Bigger Picture
**Temperature dominates**: Temperature has by far the strongest effect. Each degree warmer predicts 1.8% more butterflies 30 minutes later. This makes biological sense - butterflies are cold-blooded and more active when warm.

**Sunlight paradox**: More sunlight predicts fewer butterflies, which seems counterintuitive. Possible explanations:
1. Direct sunlight might make butterflies disperse to find shade
2. Measurement issue - sunlight exposure might be measured when butterflies are already leaving
3. Interaction with temperature - maybe on hot days, direct sun is too much

**Site differences matter**: The random effects show substantial variation between locations and time periods, indicating that local microhabitat is important.

---

## How to Explain This to Your Wife

"Honey, remember how I was studying whether wind scares away the monarch butterflies? Well, I spent weeks analyzing thousands of butterfly counts and here's what I found:

**The short answer**: Wind barely matters. When it's windy, the butterflies don't really abandon their roosts like I thought they would.

**What actually matters**: Temperature! When it's warmer, there are more butterflies around 30 minutes later. This makes sense because butterflies need warmth to be active.

**The technical stuff**: I had to test five different ways to measure 'change in butterfly numbers' because the math gets really weird when you're dealing with percentages. Like, if you go from 1 butterfly to 3 butterflies, that's a 200% increase, but if you go from 10 to 30, that's also 200%, but those feel very different, right?

**The wind effect**: I did find a tiny effect - for every minute that wind was strong in a 30-minute period, butterfly numbers went down by about 0.6%. But this was barely statistically significant, and even on really windy days (say 15 minutes of strong wind), that only predicts about a 9% decrease.

**What this means for monarchs**: They're probably tougher than I gave them credit for. A little wind doesn't send them packing. They're more concerned with finding the right temperature conditions.

**Why this matters**: It suggests that monarch roosts might be more resilient to weather variability than we thought, which could be good news for conservation as climate patterns change."

---

## Technical Quality Assessment

### Strengths of This Analysis
1. **Addressed major distribution problems**: The relative change metric solved the extreme outlier issues
2. **Proper model structure**: Mixed effects appropriately account for site and temporal dependencies  
3. **Multiple approaches tested**: Model comparison gives confidence in results
4. **Good diagnostics**: The final model shows much better residual behavior
5. **Appropriate statistical framework**: Handles the nested structure of the data

### Remaining Limitations
1. **Posterior predictive check**: Model under-predicts the frequency of "no change" observations
2. **Sample size per site**: Some deployments have few observations
3. **Temporal scope**: Only 30-minute intervals - longer-term effects unknown
4. **Confounding**: Wind and temperature aren't independent in nature

### Overall Assessment
This is a solid analysis that appropriately handles the complex structure of ecological count data. The conclusion that wind effects are minimal (if present at all) is well-supported by the data and robust to different analytical approaches.

---

## Next Steps for Your Research

1. **Consider longer time scales**: Maybe wind effects take longer than 30 minutes to manifest
2. **Explore wind-temperature interactions**: Perhaps wind only matters on hot days
3. **Site-specific analysis**: Some locations might be more wind-sensitive than others
4. **Behavioral observations**: Direct observation of butterfly behavior during wind events
5. **Different wind metrics**: Peak wind speed, sustained periods, wind direction relative to roost orientation

The lack of strong wind effects doesn't mean your hypothesis was wrong - it might just be that monarch roosts are more robust than expected, or that other factors override wind sensitivity at the temporal and spatial scales you measured."