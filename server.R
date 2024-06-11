library(shiny)
library(ggplot2)
library(mediation)
library(lmSupport)
library(boot)

server <- function(input, output, session) {
  data <- reactive({
    req(input$datafile)
    df <- read.csv(input$datafile$datapath)
    updateSelectInput(session, "outcome", choices = names(df))
    updateSelectInput(session, "moderator", choices = names(df))
    df
  })
  
  output$predictor_ui <- renderUI({
    df <- data()
    checkboxGroupInput("predictors", "Select Predictor Variables", choices = names(df))
  })
  
  output$mediator_ui <- renderUI({
    df <- data()
    checkboxGroupInput("mediators", "Select Mediator Variables", choices = names(df))
  })
  
  output$moderator_ui <- renderUI({
    df <- data()
    checkboxGroupInput("moderators", "Select Moderator Variables", choices = names(df))
  })
  
  observeEvent(input$run_analysis, {
    req(input$predictors, input$mediators, input$outcome, input$moderators)
    
    df <- data()
    predictors <- input$predictors
    mediators <- input$mediators
    outcome <- input$outcome
    moderators <- input$moderators
    
    # Mediation Analysis
    mediator_models <- lapply(mediators, function(mediator) {
      lm(as.formula(paste(mediator, "~", paste(predictors, collapse = " + "))), data = df)
    })
    
    outcome_model <- lm(as.formula(paste(outcome, "~", paste(mediators, collapse = " + "), "+", paste(predictors, collapse = " + "))), data = df)
    
    mediation_results <- lapply(1:length(mediators), function(i) {
      mediate(mediator_models[[i]], outcome_model, treat = predictors[1], mediator = mediators[i], boot = TRUE, sims = input$nboot)
    })
    
    output$mediationResults <- renderPrint({
      lapply(mediation_results, summary)
    })
    
    output$mediationPlot <- renderPlot({
      mediation_effects <- sapply(mediation_results, function(res) res$d0)
      mediation_effects <- data.frame(Mediator = mediators, Effect = mediation_effects)
      ggplot(mediation_effects, aes(x = Mediator, y = Effect)) +
        geom_bar(stat = "identity") +
        labs(title = "Mediation Effects", x = "Mediator", y = "Effect Size")
    })
    
    # Moderation Analysis
    moderation_models <- lapply(moderators, function(mod) {
      lm(as.formula(paste(outcome, "~", paste(predictors, collapse = " + "), "*", mod)), data = df)
    })
    
    output$moderationResults <- renderPrint({
      lapply(moderation_models, summary)
    })
    
    output$moderationPlot <- renderPlot({
      mod_results <- lapply(moderation_models, function(mod_model) {
        plot(mod_model)
      })
      do.call(grid.arrange, mod_results)
    })
    
    # Multi-Group Analysis
    multi_group_results <- list()
    for (mod in moderators) {
      groups <- unique(df[[mod]])
      group_results <- lapply(groups, function(group) {
        group_data <- df[df[[mod]] == group, ]
        lm(as.formula(paste(outcome, "~", paste(predictors, collapse = " + "), "+", paste(mediators, collapse = " + "))), data = group_data)
      })
      names(group_results) <- groups
      multi_group_results[[mod]] <- group_results
    }
    
    output$multigroupResults <- renderPrint({
      lapply(multi_group_results, function(group_results) {
        lapply(group_results, summary)
      })
    })
  })
}

shinyApp(ui = ui, server = server)
