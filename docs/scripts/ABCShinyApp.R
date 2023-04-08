# Approximate Bayesian Computation (ABC)

#1. Define a prior and have the ability to sample from it efficiently.
#
#2. Define a data generative process (likelihood) that we can simulate
#
#3. Generate a bunch of parameter values theta* from our prior and 
# use these theta* to generate data draws (simulations)
#
# 4. Filter the prior draws to keep theta* that lead to data 
# that approximately match the observed data
#
#5. Resulting prior draws will be our posterior.

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
      h4("ABC:"),
      numericInput("n_sim", "Number of simulations", min = 0, value = 100000),
      numericInput("n_min", "Minimum number of posterior samples", min = 0,
                   value = 1000),
      h4("Optional input:"),
      checkboxInput("options", label = "Show options", value = FALSE),
      conditionalPanel(
        "input.options == true",
        checkboxInput("bw", label = "Black and white theme", value = FALSE),
        sliderInput("lwd", "Line width",
                    min = .25, max = 2.25, value = 1)
      ),
      h4("Run:"),
      actionButton("run", "Run Simulations")
    ),
    mainPanel = mainPanel(
      plotOutput("plot"),
      textOutput("abc_summary")
    )
    
  )
)
# Define server logic
server <- function(input, output, session) {
  
  observeEvent(
    # watch for a change in a specific input
    input$n, {
      updateSliderInput(session, inputId = "x", max = input$n)
    }
  )
  
  abc_post = reactive({
    validate(need(input$n_min, "Need a value for minimum number of posterior samples."))
    abc_prior = rbeta(input$n_sim, input$a, input$b)
    abc_data_gen = rbinom(input$n_sim, size = input$n, prob = abc_prior)
    abc_prior[abc_data_gen == input$x]
  }) %>%
    bindEvent(input$run)
  
  
  abc_post_dens = reactive({
    # stopifnot(length(abc_post()) >= input$n_min)
    validate(
      need(length(abc_post()) >= input$n_min, 
           "Insufficient number of posterior samples. Increase the # of simulations.")
    )
    abc_dens = density(abc_post())
    tibble(
      p = abc_dens$x,
      density = abc_dens$y
    )
  })
  
  output$abc_summary = renderText({
    glue::glue(
      "Ran {input$n_sim} number generative simulations and obtained {length(abc_post())} ",
      "posterior samples.\nEfficiency of {100 * length(abc_post()) / input$n_sim}")
  })
  
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
        geom_line(data = abc_post_dens(), aes(x = p, y = density, color = "ABC posterior"),
                  lwd = input$lwd) + 
        labs(x = "p", y = "density", color = "distribution")
      
      if(input$bw) {
        g = g + theme_bw()
      }
      
      g 
    })
}
# Build and run the application
shinyApp(ui = ui, server = server)




