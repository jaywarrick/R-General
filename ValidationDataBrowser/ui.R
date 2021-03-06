library(shiny)
library(plotly)
library(data.table)

myUI <- shinyUI(fluidPage(
     titlePanel("Validation Data Browser"),
     fluidRow(column(width=12, actionButton("closeButton1", "Click HERE to CLOSE app properly!"))),
     selectInput("Label", label = h3("Label"),
                 choices = names(shinyData),
                 selected = which(names(shinyData)[sapply(shinyData, is.numeric)] == 'Id'),
                 width='75%'),
     selectInput("X", label = h3("X-Axis"),
                 choices = names(shinyData)[sapply(shinyData, is.numeric)],
                 selected = which(names(shinyData)[sapply(shinyData, is.numeric)] == 'NUCLEARLOCALIZATION'),
                 width='75%'),
     selectInput("Y", label = h3("Y-Axis"),
                 choices = names(shinyData)[sapply(shinyData, is.numeric)],
                 selected = 1,
                 width='75%'),
     mainPanel(
          plotlyOutput("plot"), width='100%'
     ),
     br(),
     br(),
     br(),
     fluidRow(column(width=12, actionButton("closeButton2", "Click HERE to CLOSE app properly!")))
))