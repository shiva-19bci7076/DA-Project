library("tidyverse")
library("tidytext") # tidy implimentation of NLP methods
library("syuzhet")
library("party")
library("rpart")
library("rpart.plot")
news = read.csv("D:/fake_cleaned_data_1.csv")
news1 = news[sample(1:nrow(news)), ]
news1$type = factor(news1$type)
tts = sample.split(news1,SplitRatio = 0.8)
train = subset(news1,tts == T)
test = subset(news1,tts == F)

#Finding Sentiment of each news
sentiment<-get_nrc_sentiment(train$text)
head(sentiment)

sentiment1<-get_nrc_sentiment(test$text)
head(sentiment1)

#taking only last two columns negative and positive for the analysis
df1<-sentiment[c(9,10)]
df2 = sentiment1[c(9,10)]
#function for normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#normalize negative and positive column for better analysis means the values will lie between 0 and 1
df1$negative<-normalize(df1$negative)
df1$positive<-normalize(df1$positive)

#Combine this with the news dataset
train<-cbind(train,df1)

#finding standard deviations and median of negative and positive columns for each type of news 
neg_sd<-train %>% group_by(type) %>% summarise(neg_sd=sd(negative))
pos_sd<-train %>% group_by(type) %>% summarise(pos_sd=sd(positive))
neg_med<-train %>% group_by(type) %>% summarise(neg_med=median(negative))
pos_med<-train %>% group_by(type) %>% summarise(pos_med=median(positive))

#create dataframes for negative and positive standard deviations and median
dfr2<-data.frame(neg_sd)
dfr1<-data.frame(pos_sd)
dfr3<-data.frame(neg_med)
dfr4<-data.frame(pos_med)

#merging dataframes and taking transpose of t1 we get t2
t1<-merge(dfr1,dfr2)
t2<-t(t1)
t2

#merging dataframes and taking transpose of t4 we get t3
t3<-merge(dfr4,dfr3)
t4<-t(t3)
t4

df2$negative<-normalize(df2$negative)
df2$positive<-normalize(df2$positive)

#Combine this with the news dataset
test<-cbind(test,df2)

#finding standard deviations and median of negative and positive columns for each type of news 
neg_sd1<-test %>% group_by(type) %>% summarise(neg_sd=sd(negative))
pos_sd1<-test %>% group_by(type) %>% summarise(pos_sd=sd(positive))
neg_med1<-test %>% group_by(type) %>% summarise(neg_med=median(negative))
pos_med1<-test %>% group_by(type) %>% summarise(pos_med=median(positive))

#create dataframes for negative and positive standard deviations and median
dfr5<-data.frame(neg_sd1)
dfr6<-data.frame(pos_sd1)
dfr7<-data.frame(neg_med1)
dfr8<-data.frame(pos_med1)

#merging dataframes and taking transpose of t1 we get t2
t5<-merge(dfr5,dfr6)
t6<-t(t5)
t6

#merging dataframes and taking transpose of t4 we get t3
t7<-merge(dfr7,dfr8)
t8<-t(t7)
t8

mod1 = rpart(type ~ domain_rank + spam_score + exc + que + positive + negative
             , train,method = "class")
plot(mod1)
title("Fake News Detection")
text(mod1, use.n = T, all = T, cex = 0.8)
treepred = predict(mod1,test,type = "class")
table(treepred,test$type)
mean(treepred == test$type)

rpart.plot(mod1)
title("Fake News Detection")
