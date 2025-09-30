---
date: '2025-09-30T19:04:37+00:00'
duration_seconds: 0.5
keywords:
- master's thesis
- daily change analysis
- monarch butterflies
- microclimate hypothesis
- wind effects
- time horizons
- peak counts
- variable selection
llm_service: openrouter
original_filename: DV-2025-09-30-120428.mp3
processed_date: '2025-09-30T19:06:14.569323'
word_count: 611
---
# Brainstorm: Daily Change Analysis for Master's Thesis

This is a brainstorm for the daily change analysis for my master's thesis, intended for a write-up for Francis. The main goal here is to outline my motivation for conducting this analysis so late in the process and to detail the methodology I chose.

## Motivation for the Daily Change Analysis

The impetus for this analysis came from a meeting with Jessica Griffiths. When discussing my project and the microclimate hypothesis, she found it compelling but not entirely convincing. Her skepticism stems from her personal observations at Pismo, where she has seen monarch butterflies change their location in response to wind.

I have also personally observed this phenomenon. For instance, when a south wind directly hit an unprotected cluster at the main aggregation site, the monarchs left that spot and moved to the opposite, leeward side of the tree. Given that Jessica and I have both seen this, and considering that Liang has detailed the effects of strong wind in several papers, we should treat these observations in good faith.

A lingering concern I had with my original analysis was the lack of consideration for longer time horizons. I undertook this new approach as a gut check to ensure the overall story didn't change dramatically when looking at daily effects.

## Methodological Approach: Using Peak Counts

My rationale for the methodology was to account for different time horizons and potential lagged responses to unfavorable conditions. By taking the highest count between two days (the peak or 95th percentile), I am observing the butterflies at their peak presence.

This approach removes the noise associated with *when* to measure. We've documented daily movement patterns—butterflies leave in the morning and reform clusters in the afternoon. Instead of trying to pinpoint the exact right time to measure, this method uses the peak count as a strong signal of whether they stayed. 

- If the butterflies were undisturbed by conditions, the cluster size should remain relatively stable.
- If they were responding to something unfavorable, the cluster would likely be much smaller.

By taking the maximum value, I am controlling for the potential noise caused by measurement timing. If the maximum value is zero, then the count is correctly recorded as zero.

## Variable Selection for Weather Conditions

For weather conditions, I had to select variables that could characterize diurnal patterns over the course of a full day. I generated many potential fixed effects, particularly for wind, and used a correlation matrix to select which ones to include in the final models.

Most wind variables were highly correlated with each other. The standard deviation of wind was an exception and could have been included in a two-parameter model, but I would have needed to choose a variable other than max gust, which had a 0.76 correlation with it. I ultimately chose a simpler approach to make the analysis more directly comparable with our other model. I also excluded threshold-based variables because they were also highly correlated with max gust. The rest of this information is detailed in the main document.

## Justification for the 24-Hour Time Horizon

An important consideration is that we are not tracking individual butterflies; we are conducting counts. A concern with using longer time horizons is the uncertainty this creates. By considering the highest count on one day and observing the response the next day, we are looking at a plausible response window. If we extend the time horizon any longer, we can no longer be reasonably sure that we are observing the same population of butterflies that experienced the initial conditions. The analysis becomes too complicated due to the lack of individual tracking.