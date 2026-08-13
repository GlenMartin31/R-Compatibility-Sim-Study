library(shiny)
library(tidyverse)
library(ggh4x)
library(here)

# ------------------------------------------------------------------
# Load results
# ------------------------------------------------------------------

simulation_results_summarised <- read_rds(
  here("Outputs", "simulation_results_summarised.RDS")
)

simulation_results_summarised <- simulation_results_summarised %>%
  dplyr::mutate(ImputationMethod = forcats::fct_recode(ImputationMethod,
                                                       "CCA" = "CCA",
                                                       "MI no Y (refit)" = "MI_noY_newimputation",
                                                       "MI no Y (transported)" = "MI_noY_transportedimputation",
                                                       "MI with Y (refit)" = "MI_withY_newimputation",
                                                       "MI with Y (transported)" = "MI_withY_transportedimputation",
                                                       "RI (refit)" = "RI_newimputation",
                                                       "RI (transported)" = "RI_transportedimputation",
                                                       "All Data Required" = "fullyobserved",
                                                       "Mean Imputation" = "mean",
                                                       "Risk Absent Imputation" ="zero",
                                                       "Pattern Sub-Model" = "observed"))

sim_parameters <- simulation_results_summarised %>%
  dplyr::distinct(
    Y_prev,
    X_categorical,
    R_prev_dev,
    R_prev_deployment,
    beta_x1_dev,
    beta_x2_dev,
    beta_Y_dev,
    beta_x1_deployment,
    beta_x2_deployment,
    beta_Y_deployment,
    rho_X,
    gamma_x1,
    gamma_x2
  )

missingness_lookup <- tibble::tribble(
  ~Mechanism, ~beta_x1, ~beta_x2, ~beta_Y,
  "MCAR", 0, 0, 0,
  "Missingness Dependent on X2 only", 0, 0.5, 0,
  "Missingness Dependent on X1 and X2 only", 0.5, 0.5, 0,
  "Missingness Dependent on Y and X2 only", 0, 0.5, 0.5,
  "Missingness Dependent on X1, X2 and Y", 0.5, 0.5, 0.5
)

# ------------------------------------------------------------------
# Source plotting functions
# ------------------------------------------------------------------

source("PlottingFunctions.R")

# ------------------------------------------------------------------
# User Interface
# ------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Compatibility of Missing Data Handling Methods"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "plot_type",
        "Plot Type",
        choices = c(
          "Absolute Performance",
          "Model Degradation",
          "Validation Bias"
        )
      ),
      
      selectInput(
        "Y_prev",
        "Outcome Prevalence",
        choices = sort(unique(
          simulation_results_summarised$Y_prev
        ))
      ),
      
      checkboxInput(
        "x_categorical",
        "X1 Binary",
        value = FALSE
      ),
      
      selectInput(
        "r_prev_dev",
        "Development Missingness Proportion",
        choices = sort(unique(
          simulation_results_summarised$R_prev_dev
        )),
        selected = 0.5
      ),
      
      selectInput(
        "r_prev_deployment",
        "Deployment Missingness Proportion",
        choices = sort(unique(
          simulation_results_summarised$R_prev_deployment
        )),
        selected = 0.5
      ),
      
      
      selectInput(
        "validation_mech",
        "Development and Validation Data Missingness Mechanism",
        choices = missingness_lookup$Mechanism
      ),
      
      selectInput(
        "deployment_mech",
        "Deployment Data Missingness Mechanism",
        choices = missingness_lookup$Mechanism
      ),
      
      
      selectInput(
        "rho_x",
        "Correlation (rho) between X1 and X2",
        choices = sort(unique(
          simulation_results_summarised$rho_X
        )),
        selected = 0.75
      ),
      
      selectInput(
        "gamma_x1",
        "Predictor X1 Effect on Outcome",
        choices = sort(unique(
          simulation_results_summarised$gamma_x1
        )),
        selected = 0.5
      ),
      selectInput(
        "gamma_x2",
        "Predictor X2 Effect on Outcome",
        choices = sort(unique(
          simulation_results_summarised$gamma_x2
        )),
        selected = 0.5
      ),
      
      conditionalPanel(
        condition = "input.plot_type == 'Absolute Performance'",
        
        selectInput(
          "performance_metric",
          "Performance Metric",
          choices = c(
            "Calibration Intercept",
            "Calibration Slope",
            "Brier",
            "AUC"
          )
        )
      )
    ),
    
    mainPanel(
      plotOutput(
        "main_plot",
        height = "1000px"
      )
    )
  )
)

# ------------------------------------------------------------------
# Server
# ------------------------------------------------------------------

server <- function(input, output, session) {
  
  validation_betas <- reactive({
    missingness_lookup %>%
      dplyr::filter(
        Mechanism == input$validation_mech
      )
  })
  
  deployment_betas <- reactive({
    missingness_lookup %>%
      dplyr::filter(
        Mechanism == input$deployment_mech
      )
  })
  
  output$main_plot <- renderPlot({
    
    req(validation_betas())
    req(deployment_betas())
    
    dev <- validation_betas()
    dep <- deployment_betas()
    
    if(input$plot_type == "Absolute Performance") {
      
      p <- absolute_performance_fnc(df = simulation_results_summarised,
                                    Y_prev_value = as.numeric(input$Y_prev),
                                    X_categorical_value = input$x_categorical,
                                    R_prev_dev_value = as.numeric(input$r_prev_dev),
                                    R_prev_deployment_value = as.numeric(input$r_prev_deployment),
                                    beta_x1_dev_value = dev$beta_x1,
                                    beta_x2_dev_value = dev$beta_x2,
                                    beta_Y_dev_value = dev$beta_Y,
                                    beta_x1_deployment_value = dep$beta_x1,
                                    beta_x2_deployment_value = dep$beta_x2,
                                    beta_Y_deployment_value = dep$beta_Y,
                                    rho_X_value = as.numeric(input$rho_x),
                                    gamma_x1_value = as.numeric(input$gamma_x1),
                                    gamma_x2_value = as.numeric(input$gamma_x2),
                                    performance_metric_to_plot = input$performance_metric)
      
    } else if(input$plot_type == "Model Degradation") {
      
      p <- model_degrdation_fnc(df = simulation_results_summarised, 
                                Y_prev_value = as.numeric(input$Y_prev),
                                X_categorical_value = input$x_categorical,
                                R_prev_dev_value = as.numeric(input$r_prev_dev),
                                R_prev_deployment_value = as.numeric(input$r_prev_deployment),
                                beta_x1_dev_value = dev$beta_x1,
                                beta_x2_dev_value = dev$beta_x2,
                                beta_Y_dev_value = dev$beta_Y,
                                beta_x1_deployment_value = dep$beta_x1,
                                beta_x2_deployment_value = dep$beta_x2,
                                beta_Y_deployment_value = dep$beta_Y,
                                rho_X_value = as.numeric(input$rho_x),
                                gamma_x1_value = as.numeric(input$gamma_x1),
                                gamma_x2_value = as.numeric(input$gamma_x2))
      
    } else {
      
      p <- model_bias_fnc(df = simulation_results_summarised,
                          Y_prev_value = as.numeric(input$Y_prev),
                          X_categorical_value = input$x_categorical,
                          R_prev_dev_value = as.numeric(input$r_prev_dev),
                          R_prev_deployment_value = as.numeric(input$r_prev_deployment),
                          beta_x1_dev_value = dev$beta_x1, 
                          beta_x2_dev_value = dev$beta_x2,
                          beta_Y_dev_value = dev$beta_Y,
                          beta_x1_deployment_value = dep$beta_x1,
                          beta_x2_deployment_value = dep$beta_x2,
                          beta_Y_deployment_value = dep$beta_Y,
                          rho_X_value = as.numeric(input$rho_x),
                          gamma_x1_value = as.numeric(input$gamma_x1),
                          gamma_x2_value = as.numeric(input$gamma_x2))
      
    }
    
    print(p)
  })
}

shinyApp(ui, server)