# Airline-Twitter-Sentiment-Analysis
The goal of the project is to gain knowledge of positive, negative, and neutral sentiments based on a data set of 14,640 Tweets directed towards 6 different domestic airlines. From these Tweets, airlines are able to gain valuable insights into how their customers feel about policy, procedures, and experience.
Exploratory Analysis:
Number of Tweets By Airline, Tweets By Airline and Sentiment:
Below is a histogram separating how many Tweets were written about each airline. The most Tweeted airline is United, followed by US Airways and then American. The least Tweeted is Virgin America. When we look at how the data was sorted by sentiment, there is a correlation between number of Tweets, and number of negative reviews. Airlines with more Tweets are perceived more negatively.

Bag Of Words Approach:
Context In Tweets: Words Overall
The below illustrates the most common words found in the Tweets. There are two charts- one with Tweets as is, and another with a feature that removes ‘stop words’ that are common in text in the English language.  We see here that the word “united” shows up on the most frequent words on both charts. This makes sense, since United was the most Tweeted airline. Perhaps United Airlines has issues we need to take a deeper look at. It is also interesting to note that “cancelled” and “delayed” also appeared in frequent words. Intuitively, we can infer that individuals are Tweeting (since access to the platform is on phones) as the problems they experience are happening live.

Use Of Lexicons: Bing, NRC, NRC Emotion, Afinn, Jockers, Sentiment.
The use of lexicons in sentiment analysis is important for this project. Lexicons are an excellent tool to bridge the language and knowledge expressed together. We ran various kinds of Lexicons in R to support the sentiments we inferred above. Note that since these Lexicons are pre-built, there may be varying results that do not support our initial ratios of negative to positive Tweets.
Bing: Based on the sentiment score (note: -1 is negative, 0 is neutral, 1 is positive), there is a correlation from this lexicon. Negative reviews used negative words. Positive reviews used positive words. The neutral was a larger proportion of positive words.

 NRC: This lexicon categorizes the sentiments overall by feelings. It looks like a lot of the negative sentiments that made it to the top board below are mostly Tweets with negative words. What also is interesting is that in the positive variable, observations with negative scores also are using frequent positive terms. This could be due to tone and usage of language.
