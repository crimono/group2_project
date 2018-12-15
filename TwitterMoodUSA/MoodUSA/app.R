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

  tweet <- TwitterMoodUSA::tweets_analysis()
  mapStates = map("state", fill = TRUE, plot = FALSE)

  output$mymap <- renderLeaflet({
    leaflet(data = mapStates) %>%
      addTiles()%>%
      addPolygons()
  })


  # observe({
    pal <- colorBin(
      palette = "YlOrRd",
      domain = tweet$happiness, bins=bins)

    output$mymap <- leafletProxy("mymap", data = tweet) %>%
      clearShapes() %>%
      addPolygons(data = tweet$happiness, fillColor = ~pal(happiness),
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
    # })
  #
  #
  #
  # #----------- Word Clous
  # # Make the wordcloud drawing predictable during a session
  # wordcloud_rep <- repeatable(wordcloud)
  #
  # output$plot <- renderPlot({
  #   v <- twitter_data_filtered$text
  #   wordcloud_rep(names(v), v, scale=c(4,0.5),
  #                 min.freq = input$freq, max.words=input$max,
  #                 colors=brewer.pal(8, "Dark2"))
  # })


}

# Run the application
shinyApp(ui = ui, server = server)

