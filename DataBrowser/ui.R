library(shiny)
library(plotly)

shinyUI(fluidPage(
     titlePanel("Data Browser"),
     selectInput("Label", label = h3("Label"),
                 choices = names(shinyData),
                 selected = 1,
                 width='100%'),
     selectInput("X", label = h3("X-Axis"),
                 choices = names(shinyData)[sapply(shinyData, is.numeric)],
                 selected = 1,
                 width='100%'),
     selectInput("Y", label = h3("Y-Axis"),
                 choices = names(shinyData)[sapply(shinyData, is.numeric)],
                 selected = 1,
                 width='100%'),
     mainPanel(
          plotlyOutput("plot"), height='5000px'
     )
))