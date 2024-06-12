library(shiny)

# Define UI for application
shinyUI(fluidPage(
  titlePanel("Kabiru Maitama Kura Statistical Analysis App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('datafile', 'Choose CSV file',
                accept = c('text/csv', 
                           'text/comma-separated-values,text/plain', 
                           '.csv')),
      uiOutput("independent_variables_ui"),
      uiOutput("dependent_variables_ui"),
      actionButton("analyze", "Analyze")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Descriptive Statistics", verbatimTextOutput("descriptive_stats")),
        tabPanel("Correlation Analysis", verbatimTextOutput("correlation_analysis")),
        tabPanel("Regression Analysis", verbatimTextOutput("regression_analysis")),
        tabPanel("Reliability Analysis", verbatimTextOutput("reliability_analysis"))
      )
    )
  )
))
