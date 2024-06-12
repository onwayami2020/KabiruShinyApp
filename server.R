library(shiny)
library(dplyr)
library(psych)  # Load psych package for reliability analysis

# Define server logic
shinyServer(function(input, output, session) {
  
  data <- reactive({
    req(input$datafile)
    read.csv(input$datafile$datapath)
  })
  
  output$independent_variables_ui <- renderUI({
    req(data())
    selectInput("independent_variables", "Select Independent Variables:", 
                choices = names(data()), multiple = TRUE)
  })
  
  output$dependent_variables_ui <- renderUI({
    req(data())
    selectInput("dependent_variables", "Select Dependent Variables:", 
                choices = names(data()), multiple = TRUE)
  })
  
  analysis <- eventReactive(input$analyze, {
    req(input$independent_variables, input$dependent_variables)
    
    # Descriptive Statistics
    desc_stats <- describe(data()[, input$independent_variables])
    
    # Correlation Analysis
    corr_matrix <- cor(data()[, input$independent_variables])
    
    # Regression Analysis
    regression_models <- lapply(input$dependent_variables, function(dep_var) {
      indep_vars <- input$independent_variables
      regression_model <- lm(data = data(), formula = as.formula(paste(dep_var, "~", paste(indep_vars, collapse = " + "))))
      model_summary <- summary(regression_model)
      model_fit <- list(Rsq = round(summary(regression_model)$r.squared, 3),
                        Adj_Rsq = round(summary(regression_model)$adj.r.squared, 3),
                        F_value = round(summary(regression_model)$fstatistic[1], 3),
                        p_value = round(summary(regression_model)$fstatistic[2], 3))
      list(dependent_variable = dep_var, model_summary = model_summary, model_fit = model_fit)
    })
    
    # Reliability Analysis
    reliability_analysis <- alpha(data()[, input$independent_variables])$total$raw_alpha
    
    list(descriptive_stats = desc_stats,
         correlation_analysis = corr_matrix,
         regression_analysis = regression_models,
         reliability_analysis = reliability_analysis)
  })
  
  output$descriptive_stats <- renderPrint({
    req(analysis())
    analysis()$descriptive_stats
  })
  
  output$correlation_analysis <- renderPrint({
    req(analysis())
    analysis()$correlation_analysis
  })
  
  output$regression_analysis <- renderPrint({
    req(analysis())
    lapply(analysis()$regression_analysis, function(model) {
      paste("Dependent Variable:", model$dependent_variable)
      print(model$model_summary)
      paste("Model Fit:")
      print(model$model_fit)
    })
  })
  
  output$reliability_analysis <- renderPrint({
    req(analysis())
    analysis()$reliability_analysis
  })
  
})
