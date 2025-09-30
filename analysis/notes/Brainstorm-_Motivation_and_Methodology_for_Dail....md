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

## Transcript Text

Okay, this is a brainstorm for my daily change analysis for my master's thesis. I'm tasked with giving a write up for Francis. He gave me a great laundry list of, uh, things to include. But what I want to do in this particular brainstorm is talk about, um, what my motivation was for doing this, uh, so late in the game. So basically I met with Jessica Griffiths. We were discussing my project. I asked her how she thought management might change because of this or how she now understands the microclimate hypothesis. And ah, she found it compelling but not completely convincing because, uh, she personally has observed monarch butterflies change location in response to wind at Pismo. And I also have personally personally observed that where they were clustering at the main aggregation site, let's think a south wind came through, um, basically a straight shot on the clusters that it was not very protected. Those monarchs then moved, they left that spot for sure and uh, move to the opposite leeward side of the tree. So I personally observed it, Jessica's personally observed it. I think we should treat Liang's observations in good faith. I'm m sure he has seen the effects of uh, strong wind personally. He's like, detailed that in several papers. Um, and so one of the lingering concerns I had about my analysis was that we weren't considering longer time horizons. And so I took on this approach just to, just as a gut check to make sure the story wasn't totally different. Now why did I do it the way I did it? Um, m. So my, my thought was, okay, there's different time horizons. Maybe there's some lagged response to unfavorable conditions. You. And so if I take the highest count between two days, then what I'm observing is, you know, kind of at their peak. I am removing all the noise about when to measure, uh, all this stuff. You know, there's. We documented movement patterns throughout the day. They leave in the morning, they reform in the afternoon. When exactly do you measure that? We don't, doesn't matter. We're just gonna. The peak or the 95th percentile of uh, the counts as a strong, as a strong signal of whether they, if they stayed right, like so if they were undisturbed by the conditions, then you would expect the cluster to be about the same size or if they were responding to something unfavorable, then you'd expect the cluster, um, to be much smaller or maybe clusters get larger or something. Whatever. Uh, I feel like I'm just removing, I'm controlling for a lot of the potential noise that comes from where do you measure by just taking this maximum value? If the maximum value is zero, then it's zero. Okay, we're good. Uh, I ran the analysis a similar way. Well, okay, there's another thought here. For weather conditions, I had to, like, pick different things because now we're looking at the course of a day. So how do you characterize that? Those diurnal patterns over the course of that day. So I, I, like, produced a ton of, uh, potential fixed effects, you know, a lot of different wind ones. And just use a correlation matrix to help me pick which ones to consider in the final models. Wind highly, highly correlated with itself, with the exception of, uh, standard deviation like that probably could have been thrown into a two wind parameter mix, but I'd have to pick something other than max gust. Max gusted had a 0.76 correlation. And so I just went with the simpler approach, uh, that made it more directly comparable with our other model. I didn't do the threshold stuff because again, that is also highly correlated with max gust. Uh, yeah, And I think the rest of that information is in the document. Oh, there's one important other thought, which was one of my concerns about doing these longer time horizons is that we don't know we're not tracking individuals. We are, uh, instead doing counts. And so by considering the count, the highest count. Oh, God. During the day before, and then seeing how to respond the next day, I feel like that is a plausible response. If you go any longer, then you're no longer tracking. You can't be sure that those butterflies are experiencing those conditions. Um, and if you went even further than that, I think it gets complicated, the lack of knowing who.
