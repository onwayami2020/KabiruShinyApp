library(shiny)

# Define UI for application
shinyUI(fluidPage(
    titlePanel("Process Analysis by Hayes"),
    
    sidebarLayout(
        sidebarPanel(
            fileInput('datafile', 'Choose CSV file',
                      accept = c('text/csv', 
                                 'text/comma-separated-values,text/plain', 
                                 '.csv')),
            uiOutput("dependent_ui"),
            uiOutput("predictors_ui"),
            uiOutput("mediators_ui"),
            uiOutput("moderators_ui"),
            selectInput("process_model", "Select PROCESS Model:", 
                        choices = list(
                            "Model 1" = 1,
                            "Model 4" = 4,
                            "Model 7" = 7,
                            "Model 14" = 14
                        )),
            numericInput("bootstrap", "Number of Bootstrap Samples:", 1000, min = 100, max = 10000),
            actionButton("analyze", "Analyze")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Summary", verbatimTextOutput("summary")),
                tabPanel("Plot", plotOutput("plot"))
            )
        )
    )
))
