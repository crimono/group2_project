library(tm)
library(SnowballC)
library(wordcloud)

tweet <- read.csv("MoodUSA/Data/Tweets_practice2.csv")
v <- paste(tweet$text, collapse=",")

v <- Corpus(VectorSource(v))

v <- tm_map(v, content_transformer(tolower))
v <- tm_map(v, removeWords, stopwords("english"))
