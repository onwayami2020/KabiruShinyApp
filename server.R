library(shiny)
library(dplyr)
library(broom)
library(ggplot2)
library(processR)

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
        
        # Prepare the formula based on the selected model
        if (input$process_model == 1) {
            # Moderation analysis
            results <- processR::process(model = 1, 
                               y = input$dependent, 
                               x = input$predictors[1], 
                               w = input$moderators[1], 
                               data = data(),
                               boot = input$bootstrap)
        } else if (input$process_model == 4) {
            # Simple mediation
            results <- processR::process(model = 4, 
                               y = input$dependent, 
                               x = input$predictors[1], 
                               m = input$mediators[1], 
                               data = data(),
                               boot = input$bootstrap)
        } else if (input$process_model == 7) {
            # Moderated mediation
            results <- processR::process(model = 7, 
                               y = input$dependent, 
                               x = input$predictors[1], 
                               m = input$mediators[1], 
                               w = input$moderators[1], 
                               data = data(),
                               boot = input$bootstrap)
        } else if (input$process_model == 14) {
            # Sequential mediation
            results <- processR::process(model = 14, 
                               y = input$dependent, 
                               x = input$predictors[1], 
                               m = c(input$mediators[1], input$mediators[2]), 
                               data = data(),
                               boot = input$bootstrap)
        }
        
        results
    })
    
    output$summary <- renderPrint({
        req(analysis())
        analysis()$summary
    })
    
    output$plot <- renderPlot({
        req(analysis())
        
        processR::processPlot(analysis())
    })
})
