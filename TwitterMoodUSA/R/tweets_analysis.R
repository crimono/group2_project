#' @title Tweets Analysis
#'
#' @description Analyses tweets in every state of the US
#'
#' @param Hashtag: A single string starting with a # and which must correspond
#' to an existing # on Twitter
#' @param ...: Any additional Hastags the user wants to see. The function will
#' look for tweets having all the coresponding hashtags mentionned by the user,
#' not any of them
#'
#' @return A table containing the user name of the twitter user, the text, the
#' number of 'favorites' given to the tweet and the device used to send it
#' @author group2
#' @export
#' @example tweet_analysis(#ClimateChange, #Trump)
tweets_analysis <- function(Hashtag = NULL,...) {
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

  #store data
  twitter_data_group <- list()
  if (is.null(Hashtag)) {
    for (i in 1:49) {

      twitter_data_group[[i]] <- rtweet::search_tweets(n = 40,
                                                       geocode = usa$geocode[i],
                                                       lang = "en",
                                                       token = NULL,
                                                       include_rts = FALSE,
                                                       retryonratelimit = FALSE)
      twitter_data_group[[i]]$state <- rownames(usa)[i]
    }
  }
  else {
    for (i in 1:49) {

      twitter_data_group[[i]] <- rtweet::search_tweets(q = Hashtag,
                                                       n = 40,
                                                       geocode = usa$geocode[i],
                                                       lang = "en",
                                                       token = NULL,
                                                       include_rts = FALSE,
                                                       retryonratelimit = FALSE)
      twitter_data_group[[i]]$state <- rownames(usa)[i]
    }
  }
  twitter_data <- plyr::rbind.fill(twitter_data_group)
  unique(twitter_data$state)



  #----------

  # Filtering dataset
  # Keep only the useful columns
  twitter_data_filtered <- twitter_data[, c(1, 3, 4, 5, 6, 13, 89)]

  #Take out the favorite_counts larger than 3*sd(favorite_count) to avoid having
  # accounts with possibly thousands of favorites to completely bias the
  # analysis
  twitter_data_filtered$favorite_count <-
    fav_limit(as.array(twitter_data_filtered[, 6]))

  #----------

  # Measure the happiness
  # happiness_score <- sentimentr::sentiment_by(twitter_data_filtered$text)
  happiness_score <- sentimentr::sentiment_by(sentimentr::get_sentences
                             (twitter_data_filtered$text))

  twitter_data_filtered$happiness <- happiness_score$ave_sentiment

  # Multiply happiness score by favorite_count

  tweets <- twitter_data_filtered[rep(row.names(twitter_data_filtered),
                                      twitter_data_filtered$favorite_count),
                                  1:8]

  tweets <- as.data.frame(tweets)

  return(tweets)
}
