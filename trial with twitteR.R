install.packages("twitteR")
library("twitteR")

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- "lazoxbd48sL6QKZeKi3UdhniX"
consumer_secret <- "3qdp7qd0FWB0dbRPpYi7AtaJDJ2Q7P7QDjgxj4h1ksK4nv1q4S"
access_token <- "998572965464805377-qc0rfs6DuhfINJ9qGOYOKL1xw5jVSSm"
access_secret <- "r8spMRTbb1GcRD3bqscISo51Eq6vnNXuKmLEh48EBQqpC"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw <- searchTwitter("#Trump", n=1000, lang=en, since=2018-12-02, until=2018-12-04,
              locale=NULL, geocode = lookup_coords("california"), sinceID=NULL, maxID=NULL,
              resultType=NULL, retryOnRateLimit=0)
#tw <-  twitteR::searchTwitter('#realDonaldTrump + #HillaryClinton', n = 1e4, since = '2016-11-08', retryOnRateLimit = 1e3)
d <-  twitteR::twListToDF(tw)