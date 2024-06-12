library(shiny)
library(dplyr)

# Function to generate a simple random sample
simple_random_sample <- function(data, sample_size) {
  if (sample_size > nrow(data)) {
    stop("Sample size cannot be larger than the population size.")
  }
  sample_data <- data %>% sample_n(sample_size)
  return(sample_data)
}

shinyServer(function(input, output, session) {
  data <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })

  sample_data <- eventReactive(input$generate, {
    req(data(), input$sample_size)
    simple_random_sample(data(), input$sample_size)
  })

  output$sample_data <- renderTable({
    req(sample_data())
    sample_data()
  })

  output$summary <- renderPrint({
    req(sample_data())
    summary(sample_data())
  })
})
