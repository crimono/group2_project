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
                        dateRangeInput("date", "Which period you want to look at?:")
                      ),
                      leafletOutput("mymap")
             ),

             tabPanel("Word Cloud",
                      sliderInput("freq",
                                  "Minimum Frequency:",
                                  min = 1,  max = 50, value = 15),
                      sliderInput("max",
                                  "Maximum Number of Words:",
                                  min = 1, max = 300, value = 100)
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

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -96, lat = 37.45, zoom = 4)

  })

}

# Run the application
shinyApp(ui = ui, server = server)

