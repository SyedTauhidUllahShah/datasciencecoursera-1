library(shiny)

computeRoot <- function(x, power, epsilon) {
     if ((x < 0) & (power %% 2 == 0)) return("Nope")
     # can't find even powered root of negative number
     low = min(1, x)
     high = max(1, x)
     ans = (high+low)/2
     while (abs(ans^power - x) > epsilon) {
          if (ans^power < x) {
               low <- ans
               } else {
                    high <- ans
               }
          ans = (high+low)/2
          }
     return(ans)
}

shinyServer(
     function(input, output)  {
          output$base <- renderText({
               input$goButton
               isolate(input$base)
          })
          output$power <- renderText({
               input$goButton
               isolate(input$power)
          })
          output$epsilon <- renderText({
               input$goButton
               isolate(input$epsilon)
          })
          output$answer <- renderText({
               input$goButton
               isolate(computeRoot(input$base,input$power,input$epsilon))
          })
     }
)