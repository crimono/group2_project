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
library(RJSONIO)
library(ECharts2Shiny)

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
                      leafletOutput("mymap", width="100%", height="600px"),
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
                                    draggable = TRUE, top = 400, left = "auto",
                                    right = 40, bottom = "auto",
                                    width = 330, height = "auto",
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

  output$trendingtopics <- renderUI({
    topics <- TwitterMoodUSA::trending(input$GetTrending)
    selectInput("trendingnow", "Trending Topics", choices = (topics = topics))
  })

  # tweet <- eventReactive(input$action1, {
  #   # TwitterMoodUSA::tweets_analysis(),
  # })
  #
  # tweet <- eventReactive(input$action2, {
  #   # TwitterMoodUSA::tweets_analysis(input$trendingtopics),
  # })
  #
  # tweet <- eventReactive(input$action3, {
  #   # TwitterMoodUSA::tweets_analysis(input$text),
  # })

  tweet <- read.csv("Data/newtweetsdownload.csv")

  mapStates = map("state", regions = c("alabama",
                                       "alaska",
                                       "arizona",
                                       "arkansas",
                                       "california",
                                       "colorado",
                                       "connecticut",
                                       "delaware",
                                       "florida",
                                       "georgia",
                                       "hawaii",
                                       "idaho",
                                       "illinois",
                                       "indiana",
                                       "iowa",
                                       "kansas",
                                       "kentucky",
                                       "louisiana",
                                       "maine",
                                       "maryland",
                                       "massachusetts:main",
                                       "michigan:north",
                                       "minnesota",
                                       "mississippi",
                                       "missouri",
                                       "montana",
                                       "nebraska",
                                       "nevada",
                                       "new hampshire",
                                       "new jersey",
                                       "new mexico",
                                       "new york:main",
                                       "north carolina:main",
                                       "north dakota",
                                       "ohio",
                                       "oklahoma",
                                       "oregon",
                                       "pennsylvania",
                                       "rhode island",
                                       "south carolina",
                                       "south dakota",
                                       "tennessee",
                                       "texas",
                                       "utah",
                                       "vermont",
                                       "virginia:main",
                                       "washington:main",
                                       "west virginia",
                                       "wisconsin",
                                       "wyoming"), fill = TRUE, plot = FALSE)

  # avg_happiness <- TwitterMoodUSA::average_state_score(tweet)
  avg_happiness <- read.csv("Data/avgnewtweetsdownload.csv")

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
    leaflet(data = mapStates)%>% #data = mapStates) %>%
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
  wordcloud_rep <- repeatable(wordcloud)

  output$plot <- renderPlot({
     v <- paste(tweet$text, collapse=",")
     wordcloud_rep(v, scale=c(6,2),
                   min.freq = input$freq, max.words=input$max,
                   colors=brewer.pal(8, "Dark2"), lang = "english",
                   removeWords = c("the",
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

