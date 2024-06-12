library(shiny)
library(dplyr)
library(broom)
library(ggplot2)

# Define server logic
shinyServer(function(input, output, session) {
    
    data <- reactive({
        req(input$datafile)
        read.csv(input$datafile$datapath)
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
        req(input$predictors, input$mediators, input$moderators)
        
        # Perform mediation analysis
        mediation_results <- list()
        for (mediator in input$mediators) {
            for (predictor in input$predictors) {
                model1 <- lm(as.formula(paste(mediator, "~", predictor)), data = data())
                model2 <- lm(as.formula(paste(input$moderators, "~", mediator)), data = data())
                mediation_results[[paste(predictor, mediator, sep = " -> ")]] <- list(
                    model1_summary = summary(model1),
                    model2_summary = summary(model2),
                    model1_tidy = tidy(model1),
                    model2_tidy = tidy(model2)
                )
            }
        }
        
        # Perform moderation analysis
        moderation_results <- list()
        for (moderator in input$moderators) {
            for (predictor in input$predictors) {
                interaction_term <- paste(predictor, "*", moderator, sep = "")
                formula <- as.formula(paste("Y ~", predictor, "+", moderator, "+", interaction_term))
                model <- lm(formula, data = data())
                moderation_results[[paste(predictor, moderator, sep = " * ")]] <- list(
                    model_summary = summary(model),
                    model_tidy = tidy(model)
                )
            }
        }
        
        list(mediation = mediation_results, moderation = moderation_results)
    })
    
    output$summary <- renderPrint({
        req(analysis())
        analysis()
    })
    
    output$plot <- renderPlot({
        req(analysis())
        
        mediation_plots <- list()
        moderation_plots <- list()
        
        for (result in analysis()$mediation) {
            model_tidy <- result$model1_tidy
            p <- ggplot(model_tidy, aes(x = term, y = estimate)) +
                geom_point() +
                geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
                ggtitle("Mediation Analysis")
            mediation_plots <- append(mediation_plots, list(p))
        }
        
        for (result in analysis()$moderation) {
            model_tidy <- result$model_tidy
            p <- ggplot(model_tidy, aes(x = term, y = estimate)) +
                geom_point() +
                geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error)) +
                ggtitle("Moderation Analysis")
            moderation_plots <- append(moderation_plots, list(p))
        }
        
        gridExtra::grid.arrange(grobs = c(mediation_plots, moderation_plots), ncol = 2)
    })
})
