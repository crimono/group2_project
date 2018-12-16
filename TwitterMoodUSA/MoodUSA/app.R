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
library(maps)
library(wordcloud)
library(memoise)
library(Rcpp)
library(TwitterMoodUSA)
library(rtweet)
library(sentimentr)
library(plyr)
library(Rcpp)
library(tm)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- fluidPage(

  tags$head(tags$style(
    HTML('
         #sidebar {
         background-color: #dec4de;
         }

         body, label, input, button, select {
         font-family: "Arial";
         }')
  )),

  # Application title
  #titlePanel("Happiness of people in the US"),

  # Sidebar with a slider input for number of bins
  #sidebarLayout(

  navbarPage("Mood in the US", id="nav",

             tabPanel("Interactive map",
                      leafletOutput("mymap", width="1300px", height="600px"),
                      absolutePanel(fixed = TRUE,
                                    draggable = TRUE, top = 200, right = "auto",
                                    left = 40, bottom = "auto",
                                    width = 330, height = "auto",
                                    selectInput("Hashtag", "#What?:",
                                                choices =
                                                  list("#ClimateChange" = "#ClimateChange",
                                                       "choice 2" = "",
                                                       "choice 3" = "")), ### We need the right hashtags
                                    selectInput("color", "Which colors do you want to choose?:",
                                                choices =
                                                  list("Red and Green" = "RdYlGn",
                                                       "Blue" = "Blues",
                                                       "Green" = "Greens")),
                                    plotOutput("hist", height = 200)
                      )
             ),

             tabPanel("Word Cloud",
                      sliderInput("freq",
                                  "Minimum Frequency:",
                                  min = 1,  max = 50, value = 15),
                      sliderInput("max",
                                  "Maximum Number of Words:",
                                  min = 1, max = 50, value = 30),
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
  #tweet <- TwitterMoodUSA::tweets_analysis()
  tweet <- read.csv("Data/Tweets_practice2.csv")

  mapStates = map("state", fill = TRUE, plot = FALSE)

  avg_happiness <- read.csv("Data/Average_tweets_practice2.csv")

  labels <- sprintf(
    "<strong>%s</strong><br/>Happiness: %g",
    avg_happiness[, 1], avg_happiness[, 2]
  ) %>% lapply(htmltools::HTML)

  observe({
    pal <- colorQuantile(
      palette = input$color,
      domain = avg_happiness[, 2],
      n = 6)

  output$mymap <- renderLeaflet({
    leaflet(data = mapStates) %>%
    addTiles()%>%
    addPolygons(fillColor = ~pal(avg_happiness[, 2]),
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
    hist(tweet$happiness,
         breaks = 49,
         main = "Which is the general mood in the USA?",
         xlab = "Level of happiness",
         ylab = "Number of tweets",
         ylim = range(1:100),
         col = 'Blue',
         border = 'white',
         backgroundColor = NA) +
      theme(plot.background = element_blank())
  })


  #
  # #----------- Word Clous
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)

  output$plot <- renderPlot({
    v <- paste(tweet$text, collapse=",")
    wordcloud_rep(v, scale=c(10,2),
                  min.freq = input$freq, max.words=input$max,
                  colors=brewer.pal(8, "Dark2"), lang = "english",
                  excludeWords = c("the",
                                   "got",
                                   "can",
                                   "you",
                                   "and",
                                   "we",
                                   "I'm",
                                   "they",
                                   "she",
                                   "he"))
  })


}

# Run the application
shinyApp(ui = ui, server = server)

