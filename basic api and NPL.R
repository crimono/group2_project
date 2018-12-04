library(googleway)
dasa
library(rtweet)
library(SentimentAnalysis)
library(plyr)

#state datasetbuilt in r in order to get the senter of each state
#compute the radious and build the geocode string for the twitter download

usa <- as.data.frame(state.x77)
for (i in 1:50){
  usa$x[i] <- state.center$x[i]
  usa$y[i] <- state.center$y[i]
}
usa$Radius <- sqrt(usa$Area/3.14)
usa$Miles <- paste0(usa$Radius , "mi")
usa$geocode <- paste0(usa$y ,",", usa$x,",", usa$Miles)


key <- "AIzaSyBqZJEhN9WdK3q4IakwLz70RimeKAljdjk"
set_key(key = key)
usa$geocode[43]
#Search for up to 18,000 (non-retweeted) tweets.
#Twitter rate limits cap the number of search results returned to 18,000 every 15 minutes.
#To request more than that,
#simply set retryonratelimit = TRUE and rtweet will wait for rate limit resets for you.
#https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html
GOOGLE_MAPS_KEY <- "https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA&key=AIzaSyBqZJEhN9WdK3q4IakwLz70RimeKAljdjk"


rows_per_state <- 1000
twitter_data_group <- NULL
#delete hawaii
rownames(usa)[11]
usa <- usa[-11,]

#solution 1 
for (i in 1:5) {
  
  if(is.null(twitter_data_group)) {
    twitter_data_group<- search_tweets(n = 1000, geocode = usa$geocode[i] ,
                                       lang = "en",
                                       include_rts = FALSE)
    twitter_data_group$state <- rownames(usa)[i]
    
  } else {
    
    tmp <- search_tweets(n = 1000, geocode = usa$geocode[i] ,
                                       lang = "en",
                                       include_rts = FALSE)
    tmp$state <- names(usa)[i]
    
    twitter_data_group <- rbind(twitter_data_group, tmp)
    
  }
  

}



#solution2
twitter_data_group <- list()
for (i in 1:5) {
  
    twitter_data_group[[i]] <- search_tweets(n = 1000, geocode = usa$geocode[i] ,
                                       lang = "en",
                                       token = 
                                       include_rts = FALSE)
    twitter_data_group[[i]]$state <- rownames(usa)[i]
}
twitter_data <- rbind.fill(twitter_data_group)
unique(twitter_data$state)

plot(density(twitter_data$favorite_count))
mean(twitter_data$favorite_count)
sd(twitter_data$favorite_count)
mean(twitter_data$favorite_count)+sd(twitter_data$favorite_count)*3

ts_plot(tw, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


tw <- lat_lng(tw)

## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
with(tw, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

#sentiment analysis
sentiment <- analyzeSentiment(tw$text)
sentiment$SentimentQDAP <= -0.4
sentiment$SentimentQDAP[780]
max(sentiment$SentimentQDAP)
documents <- c("which")
analyzeSentiment(documents)$SentimentQDAP

tw$text[375]

library(sentimentr)


##################################
## random sample for 30 seconds (default)
rt <- stream_tweets("")
#Stream all geo enabled tweets from London for 60 seconds.
## stream tweets from london for 60 seconds
rt <- stream_tweets(lookup_coords("london, uk"), timeout = 60)
#Stream all tweets mentioning realDonaldTrump or Trump for a week.

## stream london tweets for a week (60 secs x 60 mins * 24 hours *  7 days)
stream_tweets(
  "realdonaldtrump,trump",
  timeout = 60 * 60 * 24 * 7,
  file_name = "tweetsabouttrump.json",
  parse = FALSE
)

## read in the data as a tidy tbl data frame
djt <- parse_stream("tweetsabouttrump.json")


