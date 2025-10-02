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

1. The straight maximum count.
2. The 95th percentile count, which is more robust and less sensitive to outliers.
3. The average of the top three counts.

I then calculated the difference between consecutive days for each metric and tested a square root transformation for normality. The square root of the difference in the 95th percentile count was the most normal, with a score of 0.81, so I used that as my response variable.

### Predictor Variables

I looked at a correlation plot for all the weather metrics I calculated to select predictors that were not highly correlated (using a threshold of 0.75).

* **Temperature**: Many temperature metrics (min, max, mean) were highly correlated with each other but didn't show up as significant in the models anyway.
* **Wind**: I tried several ways to look at wind over this longer time horizon. Many wind metrics were highly correlated with the sun. The standard deviation of wind was a candidate, but it had a 0.76 correlation with max gust speed. To keep it simple and biologically interpretable, and consistent with the previous analysis, I chose to use only max gust speed. This was convenient as it allowed me to exclude the other correlated wind metrics.
* **Sunlight**: I summed all the butterflies observed in direct light as a measure of direct sunlight.

### Model Structure

* **Fixed Effects**: I included the butterfly count from the previous day as a very significant fixed effect.
* **Random Effects**: The random effect structure was identical to the first analysis, with a random effect for deployment and a correlation structure within each deployment per day (AR1).
* **Model Selection**: I tested a large number of models to see what patterns would emerge.

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

## Transcript Text

Okay, this is me thinking through why I did this second analysis. I saw Jessica Griffiths recently. Talked to her about my project. She wasn't completely convinced by my results, uh, because she has seen butterflies move m in response to wind, specifically at Pismo. And I've also seen that where the butterflies will be in the normal viewing area, we'll get a weird south wind and all those butterflies will move and go and relocate directly on the leeward side of the tree, presumably out of the wind. You have Kingston Leong did observe those things. I don't think we should assume that he made it up. So I think it's reasonable to think that my analysis is not capturing everything. So what I did was I created a similar data set, um, to my original analysis. But what I use as my response instead was the maximum or the 95% tile, 95th percentile, uh, count for each day. And I use that as the daily count. And the idea dia is um, yeah, this should be a more durable, resistant measure of, uh, monarch change. Right. So if they experience a bunch of winds and they leave that and they decide like, I didn't find any evidence of short term movement, right. But maybe, maybe they need a little more time to make that decision or make that move. And so, uh, they, so my, my thought was if there was anything unfavorable or favorable, particularly unfavorable, you know, that happened to the butterflies the day before, like do they persist or did they leave? If they leave, we should see some very strong signals of negative changes. Um. Come on, what are you doing? I lost my train of thought. But I think where I was was at, uh, this daily change. Looking at the highest counts, you know, if there was something that happened which was not good on those butterflies would leave that location in a more durable way. It removes a lot of this diurnal patterns, short term temperature stuff and maybe gets closer to, um, actual behavioral decisions. Uh, I created a bunch of different predictors. I will also calculated the count for butterflies in three different ways. So I took just the straight, what was the max count, took the 95th percentile, which was, you know, a little bit more robust of a estimate, like less sensitive to outliers. And then, um, also took the average of the top three, calculated the difference between days, and then tested transformation. The square root specifically calculated the normality for all of them. Turned out the 95th percentile, square root difference was the most normal. It had a pretty good score. It was like 0.81. And so I used that as my response and then I looked At a correlation plot for all the different weather metrics I calculated, there's a lot, um, basically tried to capture things that were not highly correlated with each other using the Same threshold of 0.75. Uh, temperature had quite a few actually there, like the min max and temperature at max count mean was highly correlated with max. Uh, all those temperature parameters didn't really show up in the models anyway. So whatever. Uh, window tried a bunch of different ways to look at wind again, you know, thinking we got this longer time horizon. But um, wind again was very highly correlated with the sun itself. Um, max gust speed didn't have any. I mean I could have maybe picked a few different wind metrics. Like the standard deviation of wind was one that kind of popped out as a candidate to include, but it had a correlation with max gust of 0.76. And because we use max gust in the other analysis, um, I just went with that to keep it simple, biologically interpretable. And uh, that was kind of convenient because all the other wind metrics with max gust could be excluded. For direct sunlight. I just summed, um, all the butterflies in direct light. I thought that would be an appropriate measure. Uh, and then I included the butterfly count for the previous day as a fixed effect that was very significant. Uh, so the similar structure of the random effect structure was again identical, I think, where, uh, it was by deployment. And then there's a correlation structure within each deployment per day M and then same ideas before just completely spam the number of models that I could test, see what merges. And unlike my first analysis. Well, I guess I should back up and say because I'm looking at the day level. Come on. Going this way because I'm looking at day. I only had a hundred data points, 100 observations, which is far, far fewer. Uh, that had the advantage actually making the model diagnostics like much more reasonable. A lot of that, those counting artifacts just disappeared in the residuals, which was really nice to see. But I think my, um, the inferences I can make are weaker. Um, and I, I think another consequence of that is I had. Of my top 10 models, six all had AIC scores, uh, less than two. I think five of them had less than one. And there was definitely some patterns, some M like candidates that routinely appeared. Then also like story is not super clear. Wind. Wind shows up quite a bit actually. And depending on how you set up the models, it is either significant or not. Some early iterations. The best model was just, just wind. I think when you control everything a little better, you throw in Things like the previous count and the AR1 structure, you can really weaken the effect of wind. Um, I think in my last one, wind had a P value of like 0.08. But the curve is, um, very stable and it's really confusing. So you have. What it predicts is that you see the biggest decreases, uh, day to day, and all things are held equal. Come on. At, uh, wind speeds of zero meters per second. And that decrease lessons and lessons. As the wind speed picks up. And from a certain range you can actually expect butterflies to increase, um, dropping at the highest. And so, uh, I don't, you know, we're seeing something real, or am I painting a picture? That would be very difficult to, uh, replicate is my concern. Yeah, the pattern itself I would consider fairly nonsensical. If I could just add more observations. Would that curve, um, just become more and more flat? That's my thought. That's like a very odd direct sunlight, which is very strong. So I guess the concern here is, do I even present this? Like, I m. Think it, I don't think it necessarily contradicts my other results. We found no effect of wind for 30 minute intervals here. You could argue there is some effect, but trying to understand what that effect is, it intuitively feels wrong. Like, why should you expect that curve? Oh, I think I see what's happening now. M. There's only one day where the previous max day wind gust is zero. Um, I think that's creating a lot of weirdness. Okay, I'm gonna play with that.
