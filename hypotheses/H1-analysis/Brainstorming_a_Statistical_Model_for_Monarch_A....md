---
date: '2025-08-11T18:15:55+00:00'
duration_seconds: 1.0
keywords:
- monarch butterfly
- statistical modeling
- abundance
- proportional change
- time series
- wind effects
- master's thesis
llm_service: openrouter
original_filename: DV-2025-08-11-111548.mp3
processed_date: '2025-08-11T18:17:24.121549'
word_count: 512
---
# Brainstorm: Modeling Changes in Monarch Abundance

This is a brainstorm for my master's hypothesis. I'm realizing my proposed models aren't really doing what I thought they were, so I'm beginning again here.

## Core Research Question & Hypothesis

I am interested in changes in monarch abundance between two points in time. I'm exploring the idea that when overwintering butterflies are exposed to winds above certain thresholds, they will leave that site.

The thinking is that there are two plausible explanations for why monarchs abandon their roosts that I can comment on: wind speed and temperature. Monarchs warm rapidly in direct sunlight, so maybe that's why they're moving.

## Response Variable: Proportional Change in Abundance

I'm trying to think through my response variable more carefully. I'm interested in a change in butterfly abundance, and I think the proportional change is the right metric. It has a nice normalizing effect across my different counters and view angles, and it's really what we care about: does a small or large proportion leave?

I want the change to be positive and negative to have directionality. I want to be able to see very quickly if a higher proportion of gusty minutes leads to a negative, positive, or no effect on the change in monarch abundance.

### Calculation Formula

I've decided on a formula for the proportional change. First, you calculate the difference, then divide by the original count:

`(New Count - Original Count) / Original Count`

### Interpretation

*   If the number increases, you get a positive number.
*   If the number decreases, you get a negative number. The value is capped at -1, which represents a 100% decrease (complete abandonment).
*   If the number stays the same, the result is zero, representing no change.

This is a nice proportional change metric. The interpretation is quite nice because it's just a percentage. If I get a significant predictor and it's -0.8, that's an 80% decrease. I like this proportional metric; it makes a lot more sense.

## Predictor Variables

1.  **Wind:** Minutes above a certain threshold for sustained winds and gusts.
2.  **Temperature:** The average ambient temperature during the observation period.
3.  **Sunlight Exposure:** The proportion of butterflies in direct sunlight during the two periods.

## Methodological Considerations & Open Questions

1.  **Time Interval:** The maximum resolution I have is 30 minutes. 
    *   **Question:** Is it double-dipping to calculate different time intervals within the same dataset? For example, I could calculate all the changes at 30-minute increments, and then do the same for 1-hour, 2-hour, and 4-hour increments. You're getting different response variables that may appear independent, but they are coming from the same source of information.
    *   **Decision (for now):** I'm not sure if 30 minutes is a good window or if I should be looking at longer intervals. For now, to keep it clean, I think I will just use the 30-minute interval.

2.  **Autocorrelation:** Because this is time-series data, there needs to be some kind of autocorrelation effect in the model, maybe an AR(1) structure or something else. I'm stressed about this.