library(shiny)
library(plotly)
library(data.table)

myServer <- shinyServer(function(input, output, session) {

     ##### General reactives #####
     observe({
          if(input$closeButton1 > 0)
          {
               stopApp()
          }
     })
     observe({
          if(input$closeButton2 > 0)
          {
               stopApp()
          }
     })

     output$plot <- renderPlotly({

          if(is.data.table(shinyData))
          {
               xlab <- list(title = input$X)
               ylab <- list(title = input$Y)
               plot_ly(mode='markers', x=shinyData[,get(input$X)], y=shinyData[,get(input$Y)], color=shinyData[,get('Class')], text=shinyData[,get(input$Label)]) %>%
                    layout(xaxis = xlab, yaxis = ylab, hovermode="closest")
          }
          else
          {
               xlab <- list(title = input$X)
               ylab <- list(title = input$Y)
               plot_ly(mode='markers', x=shinyData[,input$X], y=shinyData[,get(input$Y)], color=shinyData[,get('Class')], text=shinyData[,get(input$Label)]) %>%
                    layout(xaxis = xlab, yaxis = ylab, hovermode="closest")
          }

          #           # style the xaxis
          #           layout(p, xaxis = list(title = "Ratings", range = c(minx, maxx), autorange = F,
          #                                  autotick = F, tick0 = minx, dtick = size))
     })
})