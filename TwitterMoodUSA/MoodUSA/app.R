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
library(ggplot2)
library(ggforce)
library(maps)
library(wordcloud)
library(memoise)
library(Rcpp)
library(TwitterMoodUSA)
library(rtweet)
library(sentimentr)
library(plyr)
library(dplyr)
library(tm)
library(shinydashboard)
library(RJSONIO)
library(devtools)
library(githubinstall)
library(ECharts2Shiny)
library(geojsonio)
library(DT)


trendingplaces <- as.list(read.csv("Data/Cities for trending topics.csv"))


# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$head(
    tags$style(HTML('
         #sidebar {
                    background-color: #dec4de;
                    }

                    body, label, input, button, select {
                    font-family: "Courier";
                    }')
    )
  ),

  # Application title
  #titlePanel("Happiness of people in the US"),

  # Sidebar with a slider input for number of bins
  #sidebarLayout(

  navbarPage("Mood in the US", id="nav",

             tabPanel("Interactive map",
                      leafletOutput("mymap", width="100%", height="800px"),
                      absolutePanel(fixed = TRUE,
                                    draggable = TRUE, top = 200, right = "auto",
                                    left = 40, bottom = "auto",
                                    width = 330, height = "auto",
                                    selectInput("focus", "See happiness for...",
                                                choices =
                                                  list("All Tweets" = "All Tweets",
                                                       "A specific topic" = "A specific topic")),
                                    conditionalPanel(
                                      condition = "input.focus == 'All Tweets'",
                                      actionButton("action1", label = "Launch New Search")
                                    ),
                                    conditionalPanel(
                                      condition = "input.focus == 'A specific topic'",
                                      radioButtons("TopRes", "Which topic research?",
                                                   choices = list("Get Trending Topics" = 1, "Choose my own topic" = 2),
                                                   selected = 1),
                                      conditionalPanel(
                                        condition = "input.TopRes == 1",
                                        selectInput("GetTrending", "Trends from...",
                                                    choices = (trendingplaces[2] = trendingplaces[1])),
                                        uiOutput("trendingtopics"),
                                        actionButton("action2", label = "Launch New Search")
                                      ),
                                      conditionalPanel(
                                        condition = "input.TopRes == 2",
                                        textInput("text", "How is the US feeling about...", value = "Write your topic or hashtag here"),
                                        actionButton("action3", label = "Launch New Search")
                                      )
                                    )

                      ),
                      absolutePanel(fixed = TRUE,
                                    draggable = TRUE, top = 100, left = "auto",
                                    right = 40, bottom = "auto",
                                    width = 330, height = "auto",
                        selectInput("color", "Which colors do you want to choose?",
                                    choices =
                                      list("Red and Green" = "RdYlGn",
                                           "Blue" = "Blues",
                                           "Green" = "Greens"))
                      ),
                      absolutePanel(fixed = TRUE,
                                    draggable = TRUE, top = 395, left = "auto",
                                    right = 40, bottom = "auto",
                                    width = 330, height = "auto",
                        plotOutput("hist", height = 200)
                      )

             ),

             tabPanel("Word Cloud",
                      sliderInput("freq",
                                  "Minimum Frequency:",
                                  min = 1,  max = 50, value = 15),
                      #sliderInput("max",
                      #            "Maximum Number of Words:",
                      #            min = 1, max = 50, value = 30),
                      # Show Word Cloud
                      mainPanel(
                        loadEChartsLibrary(),
                        tags$div(id="test", style="width:100%;height:500px;"),
                        deliverChart(div_id = "test")
                      )
             ),

             tabPanel("Data explorer",
                      fluidRow(
                        h2("Explore the tweets in a dataframe"),
                        DT::dataTableOutput("mytable")

                      ),
                      conditionalPanel("false", icon("crosshair"))
             )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

  output$trendingtopics <- renderUI({
    topics <- TwitterMoodUSA::trending(input$GetTrending)
    selectInput("trendingnow", "Trending Topics", choices = (topics = topics))
  })

  tweet <- read.csv("Data/tweets4.csv")
  # 
  # # tweet <- TwitterMoodUSA::tweets_analysis()
  # # tweet <- as.data.frame(tweet)

  tweet_overall <- tweet

  observeEvent(input$action1, {
    tweet <- tweet_overall

    avg_happiness <- TwitterMoodUSA::average_state_score(tweet)

    states <- geojsonio::geojson_read("Data/gz_2010_us_040_00_5m.json", what = "sp")
    states <- states[-52, ]
    states <- states[-12, ]
    states <- states[-9, ]
    states$happiness <- avg_happiness$V2

    labels <- sprintf(
      "<strong>%s</strong><br/>Happiness: %g",
      avg_happiness[, 1], states$happiness
    ) %>% lapply(htmltools::HTML)

    observe({
      pal <- colorQuantile(
        palette = input$color,
        domain = states$happiness,
        n = 6)

      output$mymap <- renderLeaflet({

        leaflet(data = states) %>%
          addTiles()%>%
          addPolygons(fillColor = ~pal(states$happiness),
                      weight = 2,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 0.7,
                      highlight = highlightOptions(weight = 5,
                                                   color = "#666",
                                                   dashArray = "",
                                                   fillOpacity = 0.7,
                                                   bringToFront = TRUE),
                      label = labels,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))%>%
          addLegend(pal = pal, values = avg_happiness[, 2], opacity = 0.7,
                    title = NULL, position = "bottomright")
      })
    })


    output$hist <- renderPlot({
      ggplot(data=tweet, aes (tweet$happiness)) +
        geom_histogram(col="black",
                       fill="black",
                       alpha = .3) +
        labs(title="Which is the general mood in the USA?") +
        labs(x="Level of happiness", y="Number of tweets") +
        theme(plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA))
    },bg="transparent")

    v <- paste(tweet$text, collapse=",")
    v <- Corpus(VectorSource(v))
    v <- tm_map(v, content_transformer(tolower))
    v <- tm_map(v, removeNumbers)
    v <- tm_map(v, removeWords, stopwords("english"))
    v <- tm_map(v, removeWords, c("blabla1", "blabla2"))
    v <- tm_map(v, removePunctuation)
    v <- tm_map(v, stripWhitespace)
    dtm <- TermDocumentMatrix(v)
    m <- as.matrix(dtm)
    v2 <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(name = names(v2), value=v2)

    observe({
      d <- d[(d$value >= input$freq),]
      output$plot <- renderWordcloud("test", data = d,
                                     grid_size = 10, sizeRange = c(20, 60))
    })
  })
  #
  observeEvent(input$action2, {
    # # tweet <- TwitterMoodUSA::tweets_analysis(input$trendingtopics)
    tweet <- read.csv("Data/tweets5.csv")

    avg_happiness <- TwitterMoodUSA::average_state_score(tweet)

    states <- geojsonio::geojson_read("Data/gz_2010_us_040_00_5m.json", what = "sp")
    states <- states[-52, ]
    states <- states[-12, ]
    states <- states[-9, ]
    states$happiness <- avg_happiness$V2

    labels <- sprintf(
      "<strong>%s</strong><br/>Happiness: %g",
      avg_happiness[, 1], states$happiness
    ) %>% lapply(htmltools::HTML)

    observe({
      pal <- colorQuantile(
        palette = input$color,
        domain = states$happiness,
        n = 6)

      output$mymap <- renderLeaflet({

        leaflet(data = states) %>%
          addTiles()%>%
          addPolygons(fillColor = ~pal(states$happiness),
                      weight = 2,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 0.7,
                      highlight = highlightOptions(weight = 5,
                                                   color = "#666",
                                                   dashArray = "",
                                                   fillOpacity = 0.7,
                                                   bringToFront = TRUE),
                      label = labels,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))%>%
          addLegend(pal = pal, values = avg_happiness[, 2], opacity = 0.7,
                    title = NULL, position = "bottomright")
      })
    })


    output$hist <- renderPlot({
      ggplot(data=tweet, aes (tweet$happiness)) +
        geom_histogram(col="black",
                       fill="black",
                       alpha = .3) +
        labs(title="Which is the general mood in the USA?") +
        labs(x="Level of happiness", y="Number of tweets") +
        theme(plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA))
    },bg="transparent")

    v <- paste(tweet$text, collapse=",")
    v <- Corpus(VectorSource(v))
    v <- tm_map(v, content_transformer(tolower))
    v <- tm_map(v, removeNumbers)
    v <- tm_map(v, removeWords, stopwords("english"))
    v <- tm_map(v, removeWords, c("blabla1", "blabla2"))
    v <- tm_map(v, removePunctuation)
    v <- tm_map(v, stripWhitespace)
    dtm <- TermDocumentMatrix(v)
    m <- as.matrix(dtm)
    v2 <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(name = names(v2), value=v2)

    observe({
      d <- d[(d$value >= input$freq),]
      output$plot <- renderWordcloud("test", data = d,
                                     grid_size = 10, sizeRange = c(20, 60))
    })
  })

  observeEvent(input$action3, {
    #TwitterMoodUSA::tweets_analysis(input$text)
    tweet <- read.csv("Data/tweets2.csv")

    avg_happiness <- TwitterMoodUSA::average_state_score(tweet)

    # avg_happiness <- read.csv("Data/avgnewtweetsdownload.csv")
    states <- geojsonio::geojson_read("Data/gz_2010_us_040_00_5m.json", what = "sp")
    states <- states[-52, ]
    states <- states[-12, ]
    states <- states[-9, ]
    states$happiness <- avg_happiness$V2

    labels <- sprintf(
      "<strong>%s</strong><br/>Happiness: %g",
      avg_happiness[, 1], states$happiness
    ) %>% lapply(htmltools::HTML)

    observe({
      pal <- colorQuantile(
        palette = input$color,
        domain = states$happiness,
        n = 6)

      output$mymap <- renderLeaflet({

        leaflet(data = states) %>%
          addTiles()%>%
          addPolygons(fillColor = ~pal(states$happiness),
                      weight = 2,
                      opacity = 1,
                      color = "white",
                      dashArray = "3",
                      fillOpacity = 0.7,
                      highlight = highlightOptions(weight = 5,
                                                   color = "#666",
                                                   dashArray = "",
                                                   fillOpacity = 0.7,
                                                   bringToFront = TRUE),
                      label = labels,
                      labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))%>%
          addLegend(pal = pal, values = avg_happiness[, 2], opacity = 0.7,
                    title = NULL, position = "bottomright")
      })
    })


    output$hist <- renderPlot({
      ggplot(data=tweet, aes (tweet$happiness)) +
        geom_histogram(col="black",
                       fill="black",
                       alpha = .3) +
        labs(title="Which is the general mood in the USA?") +
        labs(x="Level of happiness", y="Number of tweets") +
        theme(plot.background = element_rect(fill = "transparent",colour = NA),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA))
    },bg="transparent")

    v <- paste(tweet$text, collapse=",")
    v <- Corpus(VectorSource(v))
    v <- tm_map(v, content_transformer(tolower))
    v <- tm_map(v, removeNumbers)
    v <- tm_map(v, removeWords, stopwords("english"))
    v <- tm_map(v, removeWords, c("blabla1", "blabla2"))
    v <- tm_map(v, removePunctuation)
    v <- tm_map(v, stripWhitespace)
    dtm <- TermDocumentMatrix(v)
    m <- as.matrix(dtm)
    v2 <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(name = names(v2), value=v2)

    observe({
      d <- d[(d$value >= input$freq),]
      output$plot <- renderWordcloud("test", data = d,
                                     grid_size = 10, sizeRange = c(20, 60))
    })
  })

  v <- paste(tweet$text, collapse=",")
  v <- Corpus(VectorSource(v))
  v <- tm_map(v, content_transformer(tolower))
  v <- tm_map(v, removeNumbers)
  v <- tm_map(v, removeWords, stopwords("english"))
  v <- tm_map(v, removeWords, c("blabla1", "blabla2"))
  v <- tm_map(v, removePunctuation)
  v <- tm_map(v, stripWhitespace)
  dtm <- TermDocumentMatrix(v)
  m <- as.matrix(dtm)
  v2 <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(name = names(v2), value=v2)

  avg_happiness <- TwitterMoodUSA::average_state_score(tweet)

  # avg_happiness <- read.csv("Data/avgnewtweetsdownload.csv")
  states <- geojsonio::geojson_read("Data/gz_2010_us_040_00_5m.json", what = "sp")
  states <- states[-52, ]
  states <- states[-12, ]
  states <- states[-9, ]
  states$happiness <- avg_happiness$V2

  labels <- sprintf(
    "<strong>%s</strong><br/>Happiness: %g",
    avg_happiness[, 1], states$happiness
  ) %>% lapply(htmltools::HTML)

  observe({
    pal <- colorQuantile(
      palette = input$color,
      domain = states$happiness,
      n = 6)

  output$mymap <- renderLeaflet({

    leaflet(data = states) %>%
    addTiles()%>%
    addPolygons(fillColor = ~pal(states$happiness),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(weight = 5,
                                             color = "#666",
                                             dashArray = "",
                                             fillOpacity = 0.7,
                                             bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"))%>%
    addLegend(pal = pal, values = avg_happiness[, 2], opacity = 0.7,
              title = NULL, position = "bottomright")
  })
  })


  output$hist <- renderPlot({
    ggplot(data=tweet, aes (tweet$happiness)) +
      geom_histogram(col="black",
                     fill="black",
                     alpha = .3) +
      labs(title="Which is the general mood in the USA?") +
      labs(x="Level of happiness", y="Number of tweets") +
      theme(plot.background = element_rect(fill = "transparent",colour = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = "transparent",colour = NA))
  },bg="transparent")


  #
  # #----------- Word Clous
  # Make the wordcloud drawing predictable during a session


  output$mytable = DT::renderDataTable({
    tweet
  })

  observe({
            d <- d[(d$value >= input$freq),]
  output$plot <- renderWordcloud("test", data = d,
                  grid_size = 10, sizeRange = c(20, 60))
  })

}

# Run the application
shinyApp(ui = ui, server = server)

