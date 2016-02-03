library(shiny)
library(plotly)

myUI <- shinyUI(fluidPage(
     titlePanel("Data Browser"),
     fluidRow(column(width=12, actionButton("closeButton1", "Click HERE to CLOSE app properly!"))),
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
     ),
     br(),
     br(),
     br(),
     fluidRow(column(width=12, actionButton("closeButton2", "Click HERE to CLOSE app properly!")))
))