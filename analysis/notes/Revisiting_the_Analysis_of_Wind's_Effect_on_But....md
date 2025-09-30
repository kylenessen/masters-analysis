---
date: '2025-09-29T15:44:48+00:00'
duration_seconds: 1.3
keywords:
- data analysis
- butterflies
- wind
- statistical modeling
- weather
- monarchs
llm_service: openrouter
original_filename: DV-2025-09-29-084439.mp3
processed_date: '2025-09-29T15:46:41.312883'
word_count: 888
---
# Revisiting the Analysis of Wind's Effect on Butterfly Movement

## Motivation for the Second Analysis
I recently spoke with Jessica Griffiths about my project, and she wasn't completely convinced by my results because she has seen butterflies move in response to wind, specifically at Pismo. I've also observed this, where butterflies in the normal viewing area will suddenly relocate to the leeward side of a tree during a south wind, presumably to get out of it. Kingston Leong also observed these behaviors, and we shouldn't assume he made it up. Therefore, it's reasonable to think my initial analysis isn't capturing the full picture.

My thought was that if unfavorable conditions occurred the day before, butterflies might persist or leave. If they leave, we should see strong signals of negative changes in their numbers. This approach aims to capture more durable behavioral decisions by removing the influence of short-term diurnal patterns and temperature fluctuations.

## New Methodology
To test this, I created a new analysis with a different response variable.

### Response Variable: Daily Change in Maximum Count
Instead of short-term counts, I used the maximum count for each day as the daily count. The idea is that this should be a more durable and resistant measure of population change. I explored three ways to calculate this:
1.  The straight maximum count.
2.  The 95th percentile count, which is more robust and less sensitive to outliers.
3.  The average of the top three counts.

I then calculated the difference between consecutive days for each metric and tested a square root transformation for normality. The square root of the difference in the 95th percentile count was the most normal, with a score of 0.81, so I used that as my response variable.

### Predictor Variables
I looked at a correlation plot for all the weather metrics I calculated to select predictors that were not highly correlated (using a threshold of 0.75).
*   **Temperature**: Many temperature metrics (min, max, mean) were highly correlated with each other but didn't show up as significant in the models anyway.
*   **Wind**: I tried several ways to look at wind over this longer time horizon. Many wind metrics were highly correlated with the sun. The standard deviation of wind was a candidate, but it had a 0.76 correlation with max gust speed. To keep it simple and biologically interpretable, and consistent with the previous analysis, I chose to use only max gust speed. This was convenient as it allowed me to exclude the other correlated wind metrics.
*   **Sunlight**: I summed all the butterflies observed in direct light as a measure of direct sunlight.

### Model Structure
*   **Fixed Effects**: I included the butterfly count from the previous day as a very significant fixed effect.
*   **Random Effects**: The random effect structure was identical to the first analysis, with a random effect for deployment and a correlation structure within each deployment per day (AR1).
*   **Model Selection**: I tested a large number of models to see what patterns would emerge.

## Results and Interpretation
Because I was looking at the day level, I only had 100 observations, which is far fewer than the first analysis. This had the advantage of making the model diagnostics much more reasonable; many of the counting artifacts in the residuals disappeared, which was nice to see. However, the inferences I can make are weaker.

A consequence of the smaller dataset was that among my top 10 models, six had AIC scores with a delta of less than two, and five had a delta of less than one. While some candidates routinely appeared, the story is not super clear.

### The Confusing Effect of Wind
Wind shows up quite a bit. Depending on the model setup, it is either significant or not. In some early iterations, the best model was just wind. When you control for other factors like the previous day's count and the AR1 structure, the effect of wind is weakened. In my latest model, wind had a p-value of about 0.08.

However, the predicted curve for wind is very stable but also very confusing and nonsensical. It predicts that, all else being equal, the biggest day-to-day decreases in butterfly counts occur at wind speeds of zero. As wind speed picks up, that decrease lessens, and in a certain range, you can actually expect the butterfly count to increase before dropping again at the highest wind speeds.

My concern is whether I am seeing something real or just painting a picture that would be difficult to replicate. The pattern itself feels intuitively wrong. My thought is that if I could add more observations, that curve might just become flatter.

So, the question is: do I even present this? It doesn't necessarily contradict my other results where we found no effect of wind over 30-minute intervals. Here, you could argue there is *some* effect, but trying to understand it is difficult because the curve feels wrong.

## Potential Insight and Next Steps
I think I see what might be happening. There is only one day in the dataset where the previous day's max wind gust was zero. I think that single data point is creating a lot of this weirdness in the model.

My next step is to investigate that data point and see how it affects the results.