library(shiny)
library(plotly)


shinyServer(function(input, output) {

     output$plot <- renderPlotly({

          # a simple histogram of movie ratings
          if(is.data.table(shinyData))
          {
               xlab <- list(title = input$X)
               ylab <- list(title = input$Y)
               plot_ly(mode='markers', x=shinyData[,get(input$X)], y=shinyData[,get(input$Y)], text=shinyData[,get(input$Label)]) %>%
                    layout(xaxis = xlab, yaxis = ylab)
          }
          else
          {
               xlab <- list(title = input$X)
               ylab <- list(title = input$Y)
               plot_ly(mode='markers', x=shinyData[,input$X], y=shinyData[,get(input$Y)], text=shinyData[,get(input$Label)]) %>%
                    layout(xaxis = xlab, yaxis = ylab)
          }

          #           # style the xaxis
          #           layout(p, xaxis = list(title = "Ratings", range = c(minx, maxx), autorange = F,
          #                                  autotick = F, tick0 = minx, dtick = size))
     })
})