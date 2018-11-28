library(googleway)

library(rtweet)

library(SentimentAnalysis)

#tmap

key <- "AIzaSyBqZJEhN9WdK3q4IakwLz70RimeKAljdjk"
set_key(key = key)

#Search for up to 18,000 (non-retweeted) tweets.
#Twitter rate limits cap the number of search results returned to 18,000 every 15 minutes.
#To request more than that,
#simply set retryonratelimit = TRUE and rtweet will wait for rate limit resets for you.
#https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html
GOOGLE_MAPS_KEY <- "https://maps.googleapis.com/maps/api/geocode/json?address=1600+Amphitheatre+Parkway,+Mountain+View,+CA&key=AIzaSyBqZJEhN9WdK3q4IakwLz70RimeKAljdjk"
tw <- search_tweets(q = "#climatechange", n = 1000, geocode = lookup_coords("california"),
                                    lang = "en",
                                    include_rts = FALSE)

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


