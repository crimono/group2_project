#'@title Average score of the tweets
#'
#'@description This function averages the happiness score of the tweets fetched
#'by the tweets_analysis function and stores it in a matrix with the name of
#'each state and their corresponding average score
#'
#'@param downloaded_tweets: A list of tweets and their characteristics
#'corresponding to the output of the function tweets_analysis
#'@return A matrix of 49 rows and 2 columns, with the first row containing the
#'name of the state and the second the corresponding average happiness score of
#'the tweets published in that state
#' @author group2
#' @export
average_state_score <- function(downloaded_tweets){
  avg_state_score <- matrix(NA, nrow = 49, ncol = 2)
  avg_state_score[, 1] <- rownames(state.x77)[- 11]
  j <- 1
  for (i in rownames(state.x77)[- 11]){
    tweets_by_state <- dplyr::filter(downloaded_tweets, state == i)
    avg_state_score[j , 2] <- mean(tweets_by_state$happiness)
    j <- j+1
  }
  avg_state_score <- as.data.frame(avg_state_score)
  avg_state_score[, 2] <- as.numeric(avg_state_score[, 2])
  avg_state_score[, 2] <-  scales::rescale(avg_state_score[, 2], to = c(0, 100))
  return(avg_state_score)
}


