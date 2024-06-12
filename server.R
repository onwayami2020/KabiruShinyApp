library(shiny)
library(dplyr)
library(broom)
library(ggplot2)
library(lavaan)
library(semPlot)

# Define server logic
shinyServer(function(input, output, session) {
    
    data <- reactive({
        req(input$datafile)
        read.csv(input$datafile$datapath)
    })
    
    output$dependent_ui <- renderUI({
        req(data())
        selectInput("dependent", "Select Dependent Variable:", choices = names(data()))
    })
    
    output$predictors_ui <- renderUI({
        req(data())
        selectInput("predictors", "Select Predictor(s):", choices = names(data()), multiple = TRUE)
    })
    
    output$mediators_ui <- renderUI({
        req(data())
        selectInput("mediators", "Select Mediator(s):", choices = names(data()), multiple = TRUE)
    })
    
    output$moderators_ui <- renderUI({
        req(data())
        selectInput("moderators", "Select Moderator(s):", choices = names(data()), multiple = TRUE)
    })
    
    analysis <- eventReactive(input$analyze, {
        req(input$dependent, input$predictors, input$mediators, input$moderators, input$process_model)
        
        formula <- ""
        
        # Prepare the formula based on the selected model
        if (input$process_model == 4) {
            # Simple mediation
            formula <- paste(input$mediators[1], "~", input$predictors[1], "\n",
                             input$dependent, "~", input$mediators[1], "+", input$predictors[1])
        } else if (input$process_model == 7) {
            # Moderated mediation
            formula <- paste(input$mediators[1], "~", input$predictors[1], "*", input$moderators[1], "\n",
                             input$dependent, "~", input$mediators[1], "+", input$predictors[1])
        } else if (input$process_model == 14) {
            # Sequential mediation
            formula <- paste(input$mediators[1], "~", input$predictors[1], "\n",
                             input$mediators[2], "~", input$mediators[1], "\n",
                             input$dependent, "~", input$mediators[2], "+", input$predictors[1])
        }
        
        model <- sem(formula, data = data(), se = "bootstrap", bootstrap = input$bootstrap)
        summary(model, fit.measures = TRUE, standardized = TRUE)
    })
    
    output$summary <- renderPrint({
        req(analysis())
        analysis()
    })
    
    output$plot <- renderPlot({
        req(analysis())
        
        semPaths(analysis(), "std", layout = "circle", edge.label.cex = 1.2)
    })
})
