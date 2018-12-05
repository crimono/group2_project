library("rtweet")
library("SentimentAnalysis")
library("plyr")
library("sentimentr")

#state dataset built in r in order to get the center of each state
#compute the radius and build the geocode string for the twitter download

usa <- as.data.frame(state.x77)
for (i in 1:50){
  usa$x[i] <- state.center$x[i]
  usa$y[i] <- state.center$y[i]
}
usa$Radius <- sqrt(usa$Area/3.14)
usa$Miles <- paste0(usa$Radius , "mi")
usa$geocode <- paste0(usa$y ,",", usa$x,",", usa$Miles)

#delete hawaii because almost no tweets there
rownames(usa)[11]
usa <- usa[-11,]

#----------

#Gathering all tweets in one data frame
#solution 1
# rows_per_state <- 1000
# twitter_data_group <- NULL
#
# for (i in 1:5) {
#
#   if(is.null(twitter_data_group)) {
#     twitter_data_group<- search_tweets(n = 1000, geocode = usa$geocode[i] ,
#                                        lang = "en",
#                                        include_rts = FALSE)
#     twitter_data_group$state <- rownames(usa)[i]
#
#   } else {
#
#     tmp <- search_tweets(n = 1000, geocode = usa$geocode[i] ,
#                          lang = "en",
#                          include_rts = FALSE)
#     tmp$state <- names(usa)[i]
#
#     twitter_data_group <- rbind(twitter_data_group, tmp)
#
#   }
#
#
# }


#solution2
twitter_data_group <- list()
for (i in 1:5) {

  twitter_data_group[[i]] <- search_tweets(n = 1000, geocode = usa$geocode[i] ,
                                           lang = "en",
                                           token = NULL,
                                           include_rts = FALSE,
                                           retryonratelimit = FALSE)
  twitter_data_group[[i]]$state <- rownames(usa)[i]
}
twitter_data <- rbind.fill(twitter_data_group)
unique(twitter_data$state)

#----------

# Filtering dataset
  # Keep only the useful columns
twitter_data_filtered <- twitter_data[, c(1, 3, 4, 5, 6, 13)]

  #Take out the favorite_counts larger than 3*sd(favorite_count)
twitter_data_filtered$favorite_count <- 
  fav_limit(as.array(twitter_data_filtered[, 6]))

#----------

# Measure the happiness
happiness_score <- sentiment_by(twitter_data_filtered$text)
twitter_data_filtered$happiness <- happiness_score$ave_sentiment



# Multiply happiness score by favorite_count

tweets <- twitter_data_filtered[rep(row.names(twitter_data_filtered), 
                                    twitter_data_filtered$favorite_count), 
                                1:6]

# Easier to just multiply the score by value of favorite_count but on the other
# hand, if we want to plot the histogram of happiness on Shiny, will be wrong

# subset_tweets <- twitter_data_filtered[c(1, 1674, 3293, 3800), ]
# as.data.frame(subset_tweets)
# 
# expandRows(df, subset_tweets$favorite_count)
# df.expanded <- subset_tweets[rep(row.names(subset_tweets), (subset_tweets$favorite_count), 1:6]


#----------

# Plotting the data on a US map
# tw <- lat_lng(tw)
#
# ## plot state boundaries
# par(mar = c(0, 0, 0, 0))
# maps::map("state", lwd = .25)
# with(tw, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))
