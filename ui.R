library(shiny)

shinyUI(fluidPage(
  titlePanel("Shiny Application for Sampling Technique Using a Simple Random Sampling"),

  sidebarLayout(
    sidebarPanel(
      fileInput('datafile', 'Choose CSV file', accept = c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      numericInput('sample_size', 'Sample Size', value = 100, min = 1),
      actionButton("generate", "Generate Sample")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Sample Data", tableOutput("sample_data")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
))
