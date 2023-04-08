# Likelihood
#  n - # of coin flips
#  x - # of heads out of n flips
#  p - probability of getting heads
#  x ~ Binom(n, p)
#
# Prior
#  a - # of pseudo-success  (heads)
#  b - # of pseudo-failures (tails)
#  p ~ Beta(a, b)
#
# Bayes Theorem:
# f(p|x) = f(x|p)  f(p)
#          ------   
#           f(x)
#
# Posterior
#  p|x ~ Beta(a+x, b+n-x)
#


## App

library(tidyverse)
library(shiny)

shinyApp(
  ui = fluidPage(
    title = "Beta-Binomial Shiny App", # title of the tab
    titlePanel("Beta-Binomial Shiny App"), # title on the page
    sidebarLayout(
      sidebarPanel = sidebarPanel(
        h4("Likelihood (data):"),
        sliderInput("n", "# of flips", min=1, max=100, value=20),
        sliderInput("x", "# of heads", min=0, max=100, value=10),
        h4("Prior:"),
        numericInput("a", "Prior # of heads", min=0, value=5),
        numericInput("b", "Prior # of tails", min=0, value=5),
        h4("Options:"),
        checkboxInput("options", "Show options", value=FALSE),
        conditionalPanel(
          "input.options",
          checkboxInput("bw", "Use theme_bw()", value=FALSE),
          checkboxInput("facet", "Use faceting", value=FALSE)
        )
      ),
      mainPanel = mainPanel(
        plotOutput("plot"),
        tableOutput("table")
      )
    )
  ),
  server = function(input, output, session) {
    
    observeEvent(input$n, {
      updateSliderInput(session, "x", max = input$n)
    })
    
    d = reactive({
      tibble::tibble(
        p = seq(0, 1, length.out = 1001)
      ) %>%
        mutate(
          prior = dbeta(p, input$a, input$b),
          likelihood = dbinom(input$x, size = input$n, prob = p) %>% {. * sum(.)},
          posterior = dbeta(p, input$a + input$x, input$b + input$n - input$x)
        )
    })
    
    
    output$table = renderTable({
      d()
    })
    
    output$plot = renderPlot({
      df = d() %>%
        pivot_longer(
          -p, names_to = "distribution", values_to = "density"
        )
      
      g = ggplot(df, aes(x=p, y=density, color=distribution)) + 
        geom_line(size=2)
      
      if (input$bw)
        g = g + theme_bw()
      
      if (input$facet)
        g = g + facet_grid(distribution~.)
      
      g
    })
  }
)