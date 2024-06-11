library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Mediation and Moderation Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload CSV File", accept = ".csv"),
      uiOutput("predictor_ui"),
      uiOutput("mediator_ui"),
      selectInput("outcome", "Select Outcome Variable", choices = NULL),
      uiOutput("moderator_ui"),
      numericInput("nboot", "Number of Bootstrap Simulations", value = 1000, min = 100, step = 100),
      actionButton("run_analysis", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Mediation Analysis", plotOutput("mediationPlot"), verbatimTextOutput("mediationResults")),
        tabPanel("Moderation Analysis", plotOutput("moderationPlot"), verbatimTextOutput("moderationResults")),
        tabPanel("Multi-Group Analysis", verbatimTextOutput("multigroupResults"))
      )
    )
  )
)
