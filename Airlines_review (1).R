#1) Exploring data and replacing missing data with NA
airline=read.csv("/Users/zcl/desktop/homework/framework 2/R Project/tweets.csv",
                 stringsAsFactors = F,header = T,na.strings = c("","NA"))
str(airline)

#2)  Removing unnecessary columns
airline1 = subset(airline, select = -c(airline_sentiment_gold,
                                       tweet_coord,negativereason_gold,
                                       negativereason_confidence ) )
str(airline1)

#3) Text Mining
#we replace characters with score and change it into numerica
str_replace_all(airline1$airline_sentiment,c('neutral','positive','negative'),c('0',"1","-1"))
data1 = str_replace_all(airline1$airline_sentiment,'neutral','0')
data2 = str_replace_all(data1,'positive','1')
data3 = str_replace_all(data2,'negative','-1')
score = as.numeric(data3)
airline1 = cbind(score,airline1)
str(airline1)

Topnames = airline1 %>%
  group_by(airline) %>%
  tally(sort = TRUE) 
head(Topnames)

#Distribution of Reviews
library(ggplot2)
library(ggthemes)
library(tidyr)
library(wordcloud)
library(stringr)
ggplot(data = airline1,aes(x = airline_sentiment,fill = airline_sentiment))+
  geom_bar()
ggplot(data = airline1,aes(x = airline,fill = airline))+geom_bar()

#Distribution of Scores
ggplot(data = airline1,aes(x = score))+
  geom_histogram(fill= 'sienna')+
  theme_economist()+
  coord_flip()

mean(nchar(airline1$text))
mean(str_count(string = airline1$text,pattern = '\\S+'))
mean(str_count(string = airline1$text,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"))

#Review Length in words and correlation
cor(str_count(string = airline1$text,pattern = '\\S+'),airline1$score)
cor.test(str_count(string = airline1$text,pattern = '\\S+'),airline1$score)

#Review Length in sentences and correlation
cor(str_count(string = airline1$text,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),airline1$score)
cor.test(str_count(string = airline1$text,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),airline1$score)

#Screaming Reviews and correlation
proportionUpper = str_count(airline1$text,pattern='[A-Z]')/nchar(airline1$text) 
cor(proportionUpper,airline1$score)

#Most Common Words
library(tidytext)
library(dplyr)
airline1%>%
  unnest_tokens(input = text, output = word)%>% 
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>% arrange(desc(count))%>% top_n(25)

#Plot of most Common Words
airline1%>%
  unnest_tokens(input = text, output = word)%>% 
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+ xlab('words')+ coord_flip()

#Most Common Words without stop words
airline1%>%
  unnest_tokens(input = text, output = word)%>% 
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)

#plots
airline1%>%
  unnest_tokens(input = text, output = word)%>% 
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+ xlab('words')+ coord_flip()

#Bing Lexicon
as.data.frame(get_sentiments('bing'))[1:10,]
airline1%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>% 
  inner_join(get_sentiments('bing'))%>% 
  group_by(sentiment)

#Positive and Negative Words in Reviews
airline1%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>% 
  inner_join(get_sentiments('bing'))%>% 
  group_by(sentiment) %>%
  count()

#Plots
airline1%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>% 
  inner_join(get_sentiments('bing'))%>% 
  group_by(sentiment) %>%
  count() %>%
  ggplot(aes(x=sentiment,y = n,fill = sentiment))+
  geom_col()+theme_economist()+
  guides(fill=F)+
  coord_flip()

#Proportion of Positive words in Reviews
airline1%>%
  select(tweet_id,text)%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>% 
  group_by(sentiment) %>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

#see if reviews that have a lot of positive words are rated as helpful
airline1%>%
  select(tweet_id,text,score)%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>% 
  group_by(score,sentiment) %>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

#Plots
airline1%>%
  select(tweet_id,text,score)%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>%
  ungroup() %>%
  inner_join(get_sentiments('bing'))%>% 
  group_by(score,sentiment) %>%
  summarize(n = n()) %>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=score,y = proportion,fill = sentiment))+
  geom_col()+theme_economist()+
  guides(fill=F)+
  coord_flip()

#Correlation between Negative Words and Review helpfulness
airline1%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>% 
  inner_join(get_sentiments('bing'))%>% 
  group_by(tweet_id,score)%>%
  summarize(negative = sum(sentiment=='negative')/n())%>% 
  ungroup()%>%
  summarize(correlation = cor(negative,score))

#NRC Sentiment Polarity Table - Lexicon
library(lexicon)
airline1%>%
  select(tweet_id, text)%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>% 
  inner_join(y = hash_sentiment_nrc,by = c('word'='x'))%>% 
  ungroup()%>%
  group_by(y)%>%
  summarize(count = n())%>%
  ungroup()
#-1  7229, 1 12797. confusing?

#NRC Emotion Lexicon
nrc = read.table(file = 'https://raw.githubusercontent.com/pseudorational/data/master/nrc_lexicon.txt', header = F,
                 col.names = c('word','sentiment','num'), sep = '\t',
                 stringsAsFactors = F)
nrc = nrc[nrc$num!=0,] 
nrc$num = NULL

#Emotions in Reviews
airline1%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>% 
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()

#examine the emotions expressed in the reviews
airline1%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+
  geom_col()+guides(fill=F)+coord_flip()+theme_wsj()

#Ratings of all Reviews based on Emotion Expressed
airline1%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>%
  inner_join(nrc)%>%
  group_by(tweet_id,sentiment,score)%>%
  count()%>%
  group_by(sentiment,score)%>%
  summarize(n = mean(n))%>%
  data.frame()

#Plots
airline1%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>%
  inner_join(nrc)%>%
  group_by(tweet_id,sentiment,score)%>%
  count()%>%
  group_by(sentiment, score)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=score,y=n,fill=score))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()

#afinn Lexicon
afinn = read.table('https://raw.githubusercontent.com/pseudorational/data/master/AFINN-111.txt',
                   header = F,
                   quote="",
                   sep = '\t',
                   col.names = c('word','value'), 
                   encoding='UTF-8',
                   stringsAsFactors = F)

airline1 %>%
  select(tweet_id,text)%>%
  group_by(tweet_id)%>%
  unnest_tokens(output=word,input=text)%>%
  inner_join(afinn)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_wsj()

#Jockers Lexicon
library(lexicon)
head(key_sentiment_jockers)
airline1 %>%
  select(tweet_id,text)%>%
  group_by(tweet_id)%>%
  unnest_tokens(output=word,input=text)%>%
  inner_join(key_sentiment_jockers)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),
            median=median(reviewSentiment),mean=mean(reviewSentiment))

airline1 %>%
  select(tweet_id,text)%>%
  group_by(tweet_id)%>%
  unnest_tokens(output=word,input=text)%>%
  inner_join(key_sentiment_jockers)%>%
  summarize(reviewSentiment = mean(value))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_wsj()

#Senticnet Lexicon
head(hash_sentiment_senticnet)
airline1 %>%
  select(tweet_id,text)%>%
  group_by(tweet_id)%>%
  unnest_tokens(output=word,input=text)%>%
  inner_join(hash_sentiment_senticnet, by = c('word'='x'))%>%
  summarize(reviewSentiment = mean(y))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),
            median=median(reviewSentiment),mean=mean(reviewSentiment))

airline1 %>%
  select(tweet_id,text)%>%
  group_by(tweet_id)%>%
  unnest_tokens(output=word,input=text)%>%
  inner_join(hash_sentiment_senticnet, by = c('word'='x'))%>%
  summarize(reviewSentiment = mean(y))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_wsj()

#wordcloudData
wordcloudData = 
  airline1%>%
  group_by(tweet_id)%>%
  unnest_tokens(output=word,input=text)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()

set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,
          scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

#comparison for wordcloud
wordcloudData = airline%>%
  group_by(tweet_id)%>%
  unnest_tokens(output=word,input=text)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()

rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)

#Prediction
library(tm)
corpus = Corpus(VectorSource(airline1$text))
corpus = tm_map(corpus,FUN = content_transformer(tolower))
corpus = tm_map(corpus,
                FUN = content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*',
                                                                replacement = ' ',x = x)))
corpus = tm_map(corpus,FUN = removePunctuation)
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
corpus = tm_map(corpus,FUN = stripWhitespace)
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(airline1$text))),
                     lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))
corpus = tm_map(corpus,FUN = stemDocument)
corpus[[1]][1]
dtm = DocumentTermMatrix(corpus)
dtm
inspect(dtm[1:10000,'delay'])

xdtm = removeSparseTerms(dtm,sparse = 0.95)
xdtm

xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))
sort(colSums(xdtm),decreasing = T)

