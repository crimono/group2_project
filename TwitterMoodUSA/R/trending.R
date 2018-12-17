#'@title Get Twitter's trending topics
#'
#'@description This function allows the user to fetch the curernt trending
#'topics on Twitter in 63 cities of the United States or in the whole country by
#'specifying the city or by writing United States as input.
#'
#'@param place: An integer corresponding to the WOEID (Yahoo! Where On Earth ID)
#'of the place you want the trending topics of. To get these WOEID, use the
#'function trends_available() from the rtweet package. This will retrun you a
#'table with the cities you can look up and their corresponding WOEID.
#'@return A vector of variable length, containing the trending topics ordered by
#'decreasing popularity
#'@author group2
#'@export
#'@example trending(23424977) #gives you the trending topics for the US
trending <- function(place) {
  twitter_trends <- rtweet::get_trends(place)
  twitter_trends <-
    twitter_trends[order(twitter_trends$tweet_volume, decreasing = TRUE),]
  twitter_trends <- twitter_trends$trend
  twitter_trends <- as.array(twitter_trends)
  twitter_trends <- twitter_trends[c(seq(1, 10))]
}
