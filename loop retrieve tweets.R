lookup_coords("california")

# Give values to all the states
devtools::install_github("mkearney/rtweet")
library(rtweet)
twitter_data_grouped <- matrix(NA, ncol = 88, nrow = 50000)
states <- c("Alabama", "Ohio", "California", "Texas", "Alaska")
states_coord <- matrix(NA, ncol = 2, nrow = 50)
for (i in 1:5) {
 
    
}

#------------------------------------------------------------------------------
B <- 0
for (i in 1:5) {
  twitter_data_grouped[(B + 1):(1000+B), ] <- search_tweets(
                                                    q = "#climatechange", 
                                                    n = 1000, 
                                                    geocode = lookup_coords(address = states[i]),
                                                    lang = "en",
                                                    include_rts = FALSE)
  B <- B + 1000
}

#------------------------------------------------------------------------------



#------------------------------------------------------------------------------
pot <- matrix(NA, ncol = 1, nrow = 10)
B <- 0
for (i in 1:5) {
  pot[(B + 1):(2+B), ] <- c("a", "b")
  B <- B + 2
}
pot
