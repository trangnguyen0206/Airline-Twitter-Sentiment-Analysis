#1) Exploring data and replacing missing data with NA
airline=read.csv("/Users/trangnguyen/downloads/tweets.csv",
                 stringsAsFactors = F,header = T,na.strings = c("","NA"))
str(airline)

airline$airline_sentiment.n <- as.numeric(airline$airline_sentiment)

airline$airline_sentiment<- as.numeric(airline$airline_sentiment)
airline$airline_sentiment

#2)  Removing unnecessary columns
airline1 = subset(airline, select = -c(airline_sentiment_gold,
                                       tweet_coord,negativereason_gold,
                                       negativereason_confidence ) )
str(airline1)

#3) Text Mining
#Distribution of Reviews
library(ggplot2)
library(ggthemes)
ggplot(data = airline1,aes(x = airline_sentiment))+geom_bar()
ggplot(data = airline1,aes(x = airline))+geom_bar()

mean(nchar(airline1$text))


library(stringr)
mean(str_count(string = airline1$text,pattern = '\\S+'))
mean(str_count(string = airline1$text,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"))
longest_text = which.max(str_count(string = airline1$text,
                                   pattern = "[A-Za-z,;'\"\\s]+[^.!?]* [.?!]"))
airline1$text[longest_text]

#Review Length in words
cor(str_count(string = airline1$text,pattern = '\\S+'),airline1$airline_sentiment_confidence)
cor.test(str_count(string = airline1$text,pattern = '\\S+'),airline1$airline_sentiment_confidence)

#Review Length in sentences
cor(str_count(string = airline1$text,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),airline1$airline_sentiment_confidence)
cor.test(str_count(string = airline1$text,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),airline1$airline_sentiment_confidence)

#Screaming Reviews
proportionUpper = str_count(airline1$text,pattern='[A-Z]')/nchar(airline1$text) 
cor(proportionUpper,airline1$airline_sentiment_confidence)

#Most Common Words
library(tidytext)
library(dplyr)
airline1%>%
  unnest_tokens(input = text, output = word)%>% 
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>% arrange(desc(count))%>% top_n(25)

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

airline1%>%
  unnest_tokens(input = text, output = word)%>% 
  select(word)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)

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
  select(tweet_id,text,airline_sentiment_confidence)%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>% 
  group_by(airline_sentiment_confidence,sentiment) %>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

airline1%>%
  select(tweet_id,text,airline_sentiment_confidence)%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>%
  ungroup() %>%
  inner_join(get_sentiments('bing'))%>% 
  group_by(airline_sentiment_confidence,sentiment) %>%
  summarize(n = n()) %>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=airline_sentiment_confidence,y = proportion,fill = sentiment))+
  geom_col()+theme_economist()+
  guides(fill=F)+
  coord_flip()

#Correlation between Negative Words and Review helpfulness
airline1%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>% 
  inner_join(get_sentiments('bing'))%>% 
  group_by(tweet_id,airline_sentiment_confidence)%>%
  summarize(negative = sum(sentiment=='negative')/n())%>% 
  ungroup()%>%
  summarize(correlation = cor(negative,airline_sentiment_confidence))

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
  group_by(tweet_id,sentiment,airline_sentiment_confidence)%>%
  count()%>%
  group_by(sentiment, airline_sentiment_confidence)%>%
  summarize(n = mean(n))%>%
  data.frame()

airline1%>%
  group_by(tweet_id)%>%
  unnest_tokens(output = word, input = text)%>%
  inner_join(nrc)%>%
  group_by(tweet_id,sentiment,airline_sentiment_confidence)%>%
  count()%>%
  group_by(sentiment, airline_sentiment_confidence)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=airline_sentiment_confidence,y=n,fill=airline_sentiment_confidence))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()

Topnames = airline1 %>%
  group_by(airline) %>%
  tally(sort = TRUE) 
head(Topnames,5)

airline%>%
  unnest_tokens(input = text, output = word)%>%
  select(word)%>%
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(25)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()

library(wordcloud)
wordcloudData = 
  airline%>%
  group_by(tweet_id)%>%
  unnest_tokens(output=word,input=text)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()


library(wordcloud)
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

library(tidyr)
wordcloudData = 
  airline%>%
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

library(tm)
corpus = Corpus(VectorSource(airline$text))

corpus = tm_map(corpus,FUN = content_transformer(tolower))
corpus[[617]][1]

corpus = tm_map(corpus,
                FUN = content_transformer(FUN = function(x)gsub(pattern = 'http[[:alnum:][:punct:]]*',
                                                                replacement = ' ',x = x)))
corpus[[4607]][1]

corpus = tm_map(corpus,FUN = removePunctuation)
corpus[[617]][1]

corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
corpus[[617]][1]

corpus = tm_map(corpus,FUN = stripWhitespace)
corpus[[617]][1]

dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(airline$text))),
                     lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))

corpus = tm_map(corpus,FUN = stemDocument)
corpus[[617]][1]

dtm = DocumentTermMatrix(corpus)
dtm

dim(dtm)

xdtm = removeSparseTerms(dtm,sparse = 0.95)
xdtm

xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))


sort(colSums(xdtm),decreasing = T)

dtm_tfidf = DocumentTermMatrix(x=corpus,
                               control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf = removeSparseTerms(dtm_tfidf,sparse = 0.95)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))
colnames(xdtm_tfidf) = stemCompletion(x = colnames(xdtm_tfidf),
                                      dictionary = dict_corpus,
                                      type='prevalent')
colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing = T)

library(tidyr); library(dplyr); library(ggplot2); library(ggthemes)
data.frame(term = colnames(xdtm),tf = colMeans(xdtm), tfidf = colMeans(xdtm_tfidf))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key=weighting_method,value=weight,2:3)%>%
  ggplot(aes(x=term,y=weight,fill=weighting_method))+
  geom_col(position='dodge')+
  coord_flip()+
  theme_economist()

airline_data = cbind(airline_sentiment = airline$airline_sentiment,xdtm)
airline_data_tfidf = cbind(airline_sentiment = airline$airline_sentiment,xdtm_tfidf)

set.seed(617)
split = sample(1:nrow(airline_data),size = 0.7*nrow(airline_data))
train1 = airline_data[split,]
test1 = airline_data[-split,]

library(rpart); library(rpart.plot)
tree = rpart(review_rating~,train1)
rpart.plot(tree)

reg1 = lm(sentiment_airline~,train1)
summary(reg1)


install.packages("RSQLite")
install.packages("randomForest")
install.packages("caret")
install.packages("reshape2")
install.packages("tm")
install.packages("Metrics")

set.seed(786)
require(randomForest)
fmla <- formula("airline_sentiment ~ ") #formula object

rftree <- randomForest(fmla, data=train1, importance=TRUE, 
                       ntree=1000, mtry = 36, #sampsize = 50000, 
                       do.trace = TRUE)


