#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(ggforce)
library(tmap)    # for static and interactive maps
library(mapview)
library(sf)
library(maps)
library(wordcloud)
library(memoise)


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  #titlePanel("Happiness of people in the US"),

  # Sidebar with a slider input for number of bins
  #sidebarLayout(

  navbarPage("Mood in the US", id="nav",

             tabPanel("Interactive map",
                      sidebarPanel(
                        numericInput("n_tweets", "Number of tweets:", 1000, 100, 5000),
                        textInput("Hashtag", "#What?:", placeholder = "#"),
                        checkboxInput("legend", "Show legend", TRUE)
                      ),
                      leafletOutput("mymap")
             ),

             tabPanel("Word Cloud",
                      sliderInput("freq",
                                  "Minimum Frequency:",
                                  min = 1,  max = 50, value = 15),
                      sliderInput("max",
                                  "Maximum Number of Words:",
                                  min = 1, max = 300, value = 100),
                      # Show Word Cloud
                      mainPanel(
                        plotOutput("plot")
                      )
             ),

             tabPanel("Data explorer",
                      fluidRow(
                        column(3,
                               selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                        )
                      ),
                      conditionalPanel("false", icon("crosshair"))
             )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  mapStates = map("state", fill = TRUE, plot = FALSE)

  output$mymap <- renderLeaflet({
    leaflet(data = mapStates) %>%
      addTiles()%>%
      addPolygons()
  })


  tweet <- reactive({
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
    for (i in 1:49) {

      twitter_data_group[[i]] <- rtweet::search_tweets(n = 10,
                                                       geocode = usa$geocode[i],
                                                       lang = "en",
                                                       token = NULL,
                                                       include_rts = FALSE,
                                                       retryonratelimit = FALSE)
      twitter_data_group[[i]]$state <- rownames(usa)[i]
    }
    twitter_data <- plyr::rbind.fill(twitter_data_group)
    unique(twitter_data$state)



    #----------

    # Filtering dataset
    # Keep only the useful columns
    twitter_data_filtered <- twitter_data[, c(1, 3, 4, 5, 6, 13)]

    #Take out the favorite_counts larger than 3*sd(favorite_count) to avoid having
    # accounts with possibly thousands of favorites to completely bias the
    # analysis
    twitter_data_filtered$favorite_count <-
      fav_limit(as.array(twitter_data_filtered[, 6]))

    #----------

    # Measure the happiness
    # happiness_score <- sentimentr::sentiment_by(twitter_data_filtered$text)
    happiness_score <- sentimentr::sentiment_by(sentimentr::get_sentences(twitter_data_filtered$text))

    twitter_data_filtered$happiness <- happiness_score$ave_sentiment

    # Multiply happiness score by favorite_count

    tweets <- twitter_data_filtered[rep(row.names(twitter_data_filtered),
                                        twitter_data_filtered$favorite_count),
                                    1:7]


    observe({
      pal <- colorBin(
        palette = "YlOrRd",
        domain = twitter_data$happiness, bins=bins)

      output$mymap <- leafletProxy("mymap", data =twitter_data_filtered) %>%
        clearShapes() %>%
        addPolygons(data = twitter_data_filtered$happiness, fillColor = ~pal(happiness),
                    weight = 2,
                    opacity = 1,
                    color = "white",
                    dashArray = "3",
                    fillOpacity = 0.7,
                    highlight = highlightOptions(weight = 5,
                      color = "#666",
                      dashArray = "",
                      fillOpacity = 0.7,
                      bringToFront = TRUE))%>%
        addLegend(pal = pal, values = ~happiness, opacity = 0.7, title = NULL,
                  position = "bottomright")
    })

  })

  #----------- Word Clous
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)

  output$plot <- renderPlot({
    v <- twitter_data_filtered$text
    wordcloud_rep(names(v), v, scale=c(4,0.5),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"))
  })


}

# Run the application
shinyApp(ui = ui, server = server)

