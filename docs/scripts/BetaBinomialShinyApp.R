# Example

# Likelihood (data) 
# n: # of coin flips
# x: # of heads out of n flips
# p - probability of the coin landing heads
# x ~ Binom(n, p)
#
# Prior
# a: # of pseudo-successes (heads)
# b: # of pseudo-failures (tails)
# p ~ Beta(a, b)
#
# Bayes theorem
# posterior: f(p|x)
# f(p|x) = f(x|p) f(p) / f(x)
#
# posterior
# p|x ~ Beta(a + x, b + n - x)

## App

library(tidyverse)
library(shiny)

# Define UI for application
ui <- fluidPage(
  title = "Beta-Binomial Shiny App",
  titlePanel("Beta-Binomial Shiny App"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      h4("Data:"),
      #slider
      sliderInput("n", "# of coin flips",
                  min = 0, max = 100, value = 20),
      sliderInput("x", "# of heads",
                  min = 0, max = 100, value = 10),
      h4("Prior:"),
      numericInput("a", "prior # of heads", min = 0,
                   value = 5),
      numericInput("b", "prior # of tails", min = 0,
                   value = 5),
      h4("Optional input:"),
      checkboxInput("options", label = "Show options", value = FALSE),
      conditionalPanel(
        "input.options == true",
      checkboxInput("bw", label = "Black and white theme", value = FALSE),
      sliderInput("lwd", "Line width",
                  min = .25, max = 2.25, value = 1)
      )
    ),
    mainPanel = mainPanel(
      plotOutput("plot")
    )
    
  )
)
# Define server logic
server <- function(input, output) {
  output$plot = 
    renderPlot({
      
      dbinom_rescaled = function(x, size, p) {
        c = integrate(dbinom, lower = 0, upper = 1,
                      x = x, size = size)
        dbinom(x = x, size = size, p = p) / c$value
      }
      
      g = ggplot() +
        xlim(c(0,1)) +
        stat_function(aes(color = "prior"), 
                      fun = dbeta, args = list(shape1 = input$a,
                                         shape2 = input$b), lwd = input$lwd) +
        stat_function(aes(color = "likelihood"), fun = dbinom_rescaled,
                      args = list(x = input$x, 
                                  size = input$n), lwd = input$lwd) +
        stat_function(aes(color = "posterior"), fun = dbeta,
                          args = list(shape1 = input$a + input$x,
                                      shape2 = input$b + input$n - input$x), lwd = input$lwd) +
        labs(x = "p", y = "density", color = "distribution")
        
      if(input$bw) {
        g = g + theme_bw()
      }
      
      g 
    })
}
# Build and run the application
shinyApp(ui = ui, server = server)




