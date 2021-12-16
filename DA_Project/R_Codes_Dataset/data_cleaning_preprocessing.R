library("tidyverse")
library("tidytext") # tidy implimentation of NLP methods
library("syuzhet")
news<-read.csv("fake.csv")
#bs and conspiracy news are also fake
news$type<-gsub("bs","fake",news$type)                 
news$type<-gsub("conspiracy","fake",news$type)          
#while others are real
news$type<-gsub("bias","real",news$type)              
news$type<-gsub("satire","real",news$type)
news$type<-gsub("hate","real",news$type)
news$type<-gsub("junksci","real",news$type)
news$type<-gsub("state","real",news$type)
#Count of type of news that how many are fake and real
news %>% group_by(type) %>% summarise(count=n())
#apply function for finding question marks and exclamations and adding into our dataframe
news$exc <- sapply(news$text, function(x) length(unlist(strsplit(as.character(x), "\\!+")))) #count exclamation
news$que <- sapply(news$text, function(x) length(unlist(strsplit(as.character(x), "\\?+")))) #count question marks
##Count of exclamations in fake and real news
news %>% group_by(type) %>% summarise(exclamations=sum(exc))
#Count of question marks in fake and real news       
news %>% group_by(type) %>% summarise(QuestionMarks=sum(que))
#boxplot for exclamations in fake and real news
boxplot(exc ~ type,news,ylim=c(0,20),ylab="",col=c("red","blue"))
#we can observe that fake news have more exclamations than real news
#boxplot for question marks in fake and real news
boxplot(que ~ type,news,ylim=c(0,20),col=c("red","blue")) 
#we can observe that fake news have more question marks than real
terms<- function(fake, text_column, group_column){
  
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  # get the count of each word in each review
  words <- news %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>%
    ungroup()
  
  # get the number of words per text
  #total_words <- words %>%
  #group_by(!!group_column) %>%
  #summarize(total = sum(n))
  
  # combine the two dataframes we just made
  
  return (words)
}
#store all words per text in different data frame
df<-terms(news,text,type)
#create boxplot for number of words of each type
boxplot(n ~ type,df,log="y",xlab="type",ylab="number of words",col=c("green","pink"))
