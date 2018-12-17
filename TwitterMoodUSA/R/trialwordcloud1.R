tweet <- read.csv("MoodUSA/Data/Tweets_practice2.csv")
v <- paste(tweet$text, collapse=",")
v <- Corpus(VectorSource(v))
v <- tm_map(v, removeWords, stopwords("english"))


txt <- Corpus(VectorSource(v))
txtCorpus <- tm_map(txt, removePunctuation)
txtCorpus <- tm_map(txtCorpus, removeNumbers)
txtCorpus <- tm_map(txtCorpus, content_transformer(tolower))
txtCorpus <- tm_map(txtCorpus, removeWords, stopwords("english"))
txtCorpus <- tm_map(txtCorpus, stripWhitespace); #inspect(docs[1])
txtCorpus <- tm_map(txtCorpus, stemDocument)




v$`1`

dtm <- TermDocumentMatrix(textCorpus)

dtm <- TermDocumentMatrix(v)
l <- as.matrix(dtm)
v2 <- sort(rowSums(l), decreasing = TRUE)
d <- data.frame(word = names(v2), freq=v2)
head(d, 10)
