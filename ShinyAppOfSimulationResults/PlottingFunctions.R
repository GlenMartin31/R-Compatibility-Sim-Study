absolute_performance_fnc <- function(df,
                                     Y_prev_value,
                                     X_categorical_value,
                                     R_prev_dev_value,
                                     R_prev_deployment_value,
                                     beta_x1_dev_value,
                                     beta_x2_dev_value,
                                     beta_Y_dev_value,
                                     beta_x1_deployment_value,
                                     beta_x2_deployment_value,
                                     beta_Y_deployment_value,
                                     rho_X_value,
                                     gamma_x1_value,
                                     gamma_x2_value,
                                     performance_metric_to_plot = c("Calibration Intercept",
                                                                    "Calibration Slope",
                                                                    "Brier",
                                                                    "AUC")) {
  
  performance_metric_to_plot <- as.character(match.arg(performance_metric_to_plot))
  
  #Filter to defined simulation scenario
  df_scenario_results <- df %>%
    dplyr::filter(
      Y_prev == Y_prev_value,
      X_categorical == X_categorical_value,
      R_prev_dev == R_prev_dev_value,
      R_prev_deployment == R_prev_deployment_value,
      beta_x1_dev == beta_x1_dev_value,
      beta_x2_dev == beta_x2_dev_value,
      beta_Y_dev == beta_Y_dev_value,
      beta_x1_deployment == beta_x1_deployment_value,
      beta_x2_deployment == beta_x2_deployment_value,
      beta_Y_deployment == beta_Y_deployment_value,
      rho_X == rho_X_value,
      gamma_x1 == gamma_x1_value,
      gamma_x2 == gamma_x2_value
    )
  
  Deployment_Performance <- df_scenario_results %>%
    #obtain valid development-validation pairs (table 1 of paper):
    dplyr::mutate("combinations" = case_when(
      CPM == "PR_fullyobserved" |
        CPM == "PR_CCA" |
        CPM == "PR_mean" ~ ImputationMethod %in% c("All Data Required",
                                                   "CCA",
                                                   "Mean Imputation",
                                                   "MI with Y (refit)",
                                                   "MI no Y (refit)",
                                                   "RI (refit)"),
      CPM == "PR_RI" ~ ImputationMethod %in% c("All Data Required",
                                               "CCA",
                                               "Mean Imputation",
                                               "MI with Y (refit)",
                                               "MI no Y (refit)",
                                               "RI (transported)",
                                               "RI (refit)"),
      CPM == "PR_MIwithY" ~ ImputationMethod %in% c("All Data Required",
                                                    "CCA",
                                                    "Mean Imputation",
                                                    "MI with Y (transported)",
                                                    "MI with Y (refit)",
                                                    "MI no Y (refit)",
                                                    "RI (refit)"),
      CPM == "PR_MInoY" ~ ImputationMethod %in% c("All Data Required",
                                                  "CCA",
                                                  "Mean Imputation",
                                                  "MI with Y (refit)",
                                                  "MI no Y (transported)",
                                                  "MI no Y (refit)",
                                                  "RI (refit)"),
      
      CPM == "PR_patternsubmodel" ~ ImputationMethod %in% c("All Data Required",
                                                            "CCA",
                                                            "Mean Imputation",
                                                            "MI with Y (refit)",
                                                            "MI no Y (refit)",
                                                            "RI (refit)",
                                                            "Pattern Sub-Model"),
      .default = NA)) %>%
    dplyr::filter(combinations == 1) %>%
    dplyr::select(Simulation_Scenario,
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
                  gamma_x2,
                  CPM,
                  ImputationMethod,
                  CalInt_deployment_Mean,
                  CalSlope_deployment_Mean,
                  AUC_deployment_Mean,
                  Brier_deployment_Mean) %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                        values_to = "Deployment_Performance",
                        names_to = "Performance_Metric") %>%
    dplyr::mutate(Performance_Metric = stringr::str_remove(Performance_Metric,
                                                           "_deployment_Mean"),
                  Performance_Metric = forcats::fct_recode(Performance_Metric,
                                                           "Brier Score" = "Brier",
                                                           "Calibration Intercept" = "CalInt",
                                                           "Calibration Slope" = "CalSlope"))
  
  Validation_Performance <- df_scenario_results %>%
    #obtain valid development-validation pairs (table 1 of paper):
    dplyr::mutate("combinations" = case_when(
      CPM == "PR_fullyobserved" |
        CPM == "PR_CCA" |
        CPM == "PR_mean" ~ ImputationMethod %in% c("All Data Required",
                                                   "CCA",
                                                   "Mean Imputation",
                                                   "MI with Y (refit)",
                                                   "MI no Y (refit)",
                                                   "RI (refit)"),
      CPM == "PR_RI" ~ ImputationMethod %in% c("All Data Required",
                                               "CCA",
                                               "Mean Imputation",
                                               "MI with Y (refit)",
                                               "MI no Y (refit)",
                                               "RI (transported)",
                                               "RI (refit)"),
      CPM == "PR_MIwithY" ~ ImputationMethod %in% c("All Data Required",
                                                    "CCA",
                                                    "Mean Imputation",
                                                    "MI with Y (transported)",
                                                    "MI with Y (refit)",
                                                    "MI no Y (refit)",
                                                    "RI (refit)"),
      CPM == "PR_MInoY" ~ ImputationMethod %in% c("All Data Required",
                                                  "CCA",
                                                  "Mean Imputation",
                                                  "MI with Y (refit)",
                                                  "MI no Y (transported)",
                                                  "MI no Y (refit)",
                                                  "RI (refit)"),
      
      CPM == "PR_patternsubmodel" ~ ImputationMethod %in% c("All Data Required",
                                                            "CCA",
                                                            "Mean Imputation",
                                                            "MI with Y (refit)",
                                                            "MI no Y (refit)",
                                                            "RI (refit)",
                                                            "Pattern Sub-Model"),
      .default = NA)) %>%
    dplyr::filter(combinations == 1) %>%
    dplyr::select(Simulation_Scenario,
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
                  gamma_x2,
                  CPM,
                  ImputationMethod,
                  CalInt_val_Mean,
                  CalSlope_val_Mean,
                  AUC_val_Mean,
                  Brier_val_Mean) %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                        values_to = "Validation_Performance",
                        names_to = "Performance_Metric") %>%
    dplyr::mutate(Performance_Metric = stringr::str_remove(Performance_Metric,
                                                           "_val_Mean"),
                  Performance_Metric = forcats::fct_recode(Performance_Metric,
                                                           "Brier Score" = "Brier",
                                                           "Calibration Intercept" = "CalInt",
                                                           "Calibration Slope" = "CalSlope"))
  performance_plot <- Validation_Performance %>%
    dplyr::left_join(Deployment_Performance,
                     by = c("Simulation_Scenario",
                            "Y_prev",
                            "X_categorical",
                            "R_prev_dev",
                            "R_prev_deployment",
                            "beta_x1_dev",
                            "beta_x2_dev",
                            "beta_Y_dev",
                            "beta_x1_deployment",
                            "beta_x2_deployment",
                            "beta_Y_deployment",
                            "rho_X",
                            "gamma_x1",
                            "gamma_x2",
                            "CPM",
                            "ImputationMethod",
                            "Performance_Metric")) %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("_Performance"),
                        values_to = "Performance",
                        names_to = "Setting") %>%
    dplyr::mutate(CPM = str_remove(CPM, "PR_"),
                  CPM = forcats::fct_recode(CPM,
                                            "CCA" = "CCA",
                                            "RI" = "RI",
                                            "MI no Y" = "MInoY",
                                            "MI with Y" = "MIwithY",
                                            "Fully Observed" = "fullyobserved",
                                            "Mean Imputation" = "mean",
                                            "PSM" = "patternsubmodel"),
                  CPM = forcats::fct_relevel(CPM,
                                             "Fully Observed",
                                             "CCA",
                                             "Mean Imputation",
                                             "RI",
                                             "MI with Y",
                                             "MI no Y",
                                             "PSM"),
                  
                  ImputationMethod = forcats::fct_recode(ImputationMethod,
                                                         "Fully Observed\n Data" = "All Data Required",
                                                         "Mean\n Imputation" = "Mean Imputation",
                                                         "RI\n (refit)" = "RI (refit)",
                                                         "RI\n (transported)" = "RI (transported)",
                                                         "MI with Y\n (refit)" = "MI with Y (refit)",
                                                         "MI with Y\n (transported)" = "MI with Y (transported)",
                                                         "MI no Y\n (refit)" = "MI no Y (refit)",
                                                         "MI no Y\n (transported)" = "MI no Y (transported)",
                                                         "Pattern\n Sub-Model" = "Pattern Sub-Model"),
                  ImputationMethod = forcats::fct_relevel(ImputationMethod,
                                                          "Fully Observed\n Data",
                                                          "CCA",
                                                          "Mean\n Imputation",
                                                          "RI\n (refit)",
                                                          "RI\n (transported)",
                                                          "MI with Y\n (refit)",
                                                          "MI with Y\n (transported)",
                                                          "MI no Y\n (refit)",
                                                          "MI no Y\n (transported)",
                                                          "Pattern\n Sub-Model"),
                  
                  Setting = str_remove(Setting, "_Performance"),
                  Setting = forcats::fct_relevel(Setting,
                                                 "Validation",
                                                 "Deployment")) %>%
    dplyr::filter(Performance_Metric == performance_metric_to_plot) %>%
    ggplot(aes(x = Performance, y = CPM)) +
    geom_point() +
    ggh4x::facet_grid2(Setting ~ ImputationMethod, scales = "fixed") +
    xlab(performance_metric_to_plot) +
    ylab("Developed CPM") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "gray90"),
          panel.spacing.x = unit(0.5, "lines"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          plot.title = element_text(size = 14, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))
  
  if(performance_metric_to_plot == "Calibration Intercept") {
    performance_plot <- performance_plot + 
      geom_vline(xintercept = 0, linetype = "dotted")
  } else if (performance_metric_to_plot == "Calibration Slope") {
    performance_plot <- performance_plot + 
      geom_vline(xintercept = 1, linetype = "dotted")
  }
  performance_plot
}


model_degrdation_fnc <- function(df,
                                 Y_prev_value,
                                 X_categorical_value,
                                 R_prev_dev_value,
                                 R_prev_deployment_value,
                                 beta_x1_dev_value,
                                 beta_x2_dev_value,
                                 beta_Y_dev_value,
                                 beta_x1_deployment_value,
                                 beta_x2_deployment_value,
                                 beta_Y_deployment_value,
                                 rho_X_value,
                                 gamma_x1_value,
                                 gamma_x2_value) {
  
  #Filter to defined simulation scenario
  df_scenario_results <- df %>%
    dplyr::filter(
      Y_prev == Y_prev_value,
      X_categorical == X_categorical_value,
      R_prev_dev == R_prev_dev_value,
      R_prev_deployment == R_prev_deployment_value,
      beta_x1_dev == beta_x1_dev_value,
      beta_x2_dev == beta_x2_dev_value,
      beta_Y_dev == beta_Y_dev_value,
      beta_x1_deployment == beta_x1_deployment_value,
      beta_x2_deployment == beta_x2_deployment_value,
      beta_Y_deployment == beta_Y_deployment_value,
      rho_X == rho_X_value,
      gamma_x1 == gamma_x1_value,
      gamma_x2 == gamma_x2_value
    )
  
  #Extract 'true' predictive performance as the performance of the DGM within
  #the deployment set corresponding to each target estimand
  true_predictive_performance <- df_scenario_results %>%
    dplyr::filter(CPM == "PR_DGM") %>%
    dplyr::select(Simulation_Scenario,
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
                  gamma_x2,
                  ImputationMethod,
                  CalInt_deployment_Mean,
                  CalSlope_deployment_Mean,
                  AUC_deployment_Mean,
                  Brier_deployment_Mean) %>%
    dplyr::mutate(ImputationMethod = forcats::fct_recode(ImputationMethod,
                                                         "E-all" = "All Data Required",
                                                         "E-mean" = "Mean Imputation",
                                                         "E-RI" = "RI (refit)",
                                                         "E-MI" = "MI no Y (refit)",
                                                         "E-PSM" = "Pattern Sub-Model")) %>%
    dplyr::filter(ImputationMethod %in% c("E-all",
                                          "E-mean",
                                          "E-RI",
                                          "E-MI",
                                          "E-PSM")) %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                        values_to = "True_Performance",
                        names_to = "Performance_Metric")
  
  #Extract the predictive performance of each CPM within the 
  #deployment set corresponding to each target estimand 
  CPM_predictive_performance <- df_scenario_results %>%
    dplyr::filter(CPM != "PR_DGM") %>%
    dplyr::select(Simulation_Scenario,
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
                  gamma_x2,
                  CPM,
                  ImputationMethod,
                  CalInt_deployment_Mean,
                  CalSlope_deployment_Mean,
                  AUC_deployment_Mean,
                  Brier_deployment_Mean) %>%
    dplyr::mutate(ImputationMethod = forcats::fct_recode(ImputationMethod,
                                                         "E-all" = "All Data Required",
                                                         "E-mean" = "Mean Imputation",
                                                         "E-RI" = "RI (refit)",
                                                         "E-MI" = "MI no Y (refit)",
                                                         "E-PSM" = "Pattern Sub-Model")) %>%
    dplyr::filter(ImputationMethod %in% c("E-all",
                                          "E-mean",
                                          "E-RI",
                                          "E-MI",
                                          "E-PSM")) %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                        values_to = "Model_Performance",
                        names_to = "Performance_Metric")
  
  #Calculate model degradation:
  ModelDegradation <- CPM_predictive_performance %>%
    dplyr::left_join(true_predictive_performance,
                     by = c("Simulation_Scenario",
                            "Y_prev",
                            "X_categorical",
                            "R_prev_dev",
                            "R_prev_deployment",
                            "beta_x1_dev",
                            "beta_x2_dev",
                            "beta_Y_dev",
                            "beta_x1_deployment",
                            "beta_x2_deployment",
                            "beta_Y_deployment",
                            "rho_X",
                            "gamma_x1",
                            "gamma_x2",
                            "ImputationMethod",
                            "Performance_Metric"))
  ModelDegradation$True_Performance[which(
    ModelDegradation$ImputationMethod == "E-PSM"
  )] <- true_predictive_performance$True_Performance[which(
    true_predictive_performance$ImputationMethod == "E-all"
  )]
  ModelDegradation <- ModelDegradation %>%
    dplyr::mutate("Model_Degradation" = Model_Performance - True_Performance) %>%
    dplyr::mutate(Performance_Metric = stringr::str_remove(Performance_Metric,
                                                           "_deployment_Mean"),
                  Performance_Metric = forcats::fct_recode(Performance_Metric,
                                                           "Brier Score" = "Brier",
                                                           "Calibration Intercept" = "CalInt",
                                                           "Calibration Slope" = "CalSlope"),
                  
                  ImputationMethod = forcats::fct_relevel(ImputationMethod,
                                                          "E-all",
                                                          "E-mean",
                                                          "E-RI",
                                                          "E-MI",
                                                          "E-PSM"))
  
  
  if(X_categorical_value == TRUE) {
    ModelDegradation <- ModelDegradation %>%
      dplyr::mutate(CPM = str_remove(CPM, "PR_"),
                    CPM = forcats::fct_recode(CPM,
                                              "CCA" = "CCA",
                                              "RI" = "RI",
                                              "MI no Y" = "MInoY",
                                              "MI with Y" = "MIwithY",
                                              "Fully Observed" = "fullyobserved",
                                              "Mean Imputation" = "zero",
                                              "PSM" = "patternsubmodel"),
                    CPM = forcats::fct_relevel(CPM,
                                               "Fully Observed",
                                               "CCA",
                                               "Mean Imputation",
                                               "RI",
                                               "MI with Y",
                                               "MI no Y",
                                               "PSM"))
  } else {
    ModelDegradation <- ModelDegradation %>%
      dplyr::mutate(CPM = str_remove(CPM, "PR_"),
                    CPM = forcats::fct_recode(CPM,
                                              "CCA" = "CCA",
                                              "RI" = "RI",
                                              "MI no Y" = "MInoY",
                                              "MI with Y" = "MIwithY",
                                              "Fully Observed" = "fullyobserved",
                                              "Mean Imputation" = "mean",
                                              "PSM" = "patternsubmodel"),
                    CPM = forcats::fct_relevel(CPM,
                                               "Fully Observed",
                                               "CCA",
                                               "Mean Imputation",
                                               "RI",
                                               "MI with Y",
                                               "MI no Y",
                                               "PSM"))
  }
  
  ModelDegradation %>%
    ggplot(aes(x = Model_Degradation, y = CPM)) +
    geom_point() +
    geom_vline(xintercept = 0, linetype = "dotted") +
    ggh4x::facet_grid2(Performance_Metric ~ ImputationMethod, scales = "free_x", independent = "x") +
    xlab("Model Degradation \n (deployment performance - true model performance)") +
    ylab("Developed CPM") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "gray90"),
          panel.spacing.x = unit(0.5, "lines"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          plot.title = element_text(size = 14, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) +
    ggh4x::facetted_pos_scales(
      x = list(
        Performance_Metric == "Calibration Intercept" ~
          scale_x_continuous(
            limits = c(-0.40, 0.40),
            breaks = seq(-0.4, 0.4, by = 0.1)),
        
        Performance_Metric == "Calibration Slope" ~
          scale_x_continuous(
            limits = c(-0.40, 0.40),
            breaks = seq(-0.4, 0.4, by = 0.1)),
        
        Performance_Metric == "AUC" ~ 
          scale_x_continuous(
            limits = c(-0.04, 0.04),
            breaks = seq(-0.04, 0.04, by = 0.01)),
        
        Performance_Metric == "Brier Score" ~
          scale_x_continuous(
            limits = c(-0.02, 0.02),
            breaks = seq(-0.02, 0.02, by = 0.01)))) 
}



model_bias_fnc <- function(df,
                           Y_prev_value,
                           X_categorical_value,
                           R_prev_dev_value,
                           R_prev_deployment_value,
                           beta_x1_dev_value,
                           beta_x2_dev_value,
                           beta_Y_dev_value,
                           beta_x1_deployment_value,
                           beta_x2_deployment_value,
                           beta_Y_deployment_value,
                           rho_X_value,
                           gamma_x1_value,
                           gamma_x2_value) {
  
  #Filter to defined simulation scenario
  df_scenario_results <- df %>%
    dplyr::filter(
      Y_prev == Y_prev_value,
      X_categorical == X_categorical_value,
      R_prev_dev == R_prev_dev_value,
      R_prev_deployment == R_prev_deployment_value,
      beta_x1_dev == beta_x1_dev_value,
      beta_x2_dev == beta_x2_dev_value,
      beta_Y_dev == beta_Y_dev_value,
      beta_x1_deployment == beta_x1_deployment_value,
      beta_x2_deployment == beta_x2_deployment_value,
      beta_Y_deployment == beta_Y_deployment_value,
      rho_X == rho_X_value,
      gamma_x1 == gamma_x1_value,
      gamma_x2 == gamma_x2_value
    )
  
  #remove the DGM model performance as dont need true performance for this part
  df_scenario_results <- df_scenario_results %>%
    dplyr::filter(CPM != "PR_DGM")
  
  
  if(X_categorical_value == FALSE) {
    
    #Extract deployment performance, for each CPM across all estimands
    Deployment_Performance <- df_scenario_results %>%
      dplyr::select(Simulation_Scenario,
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
                    gamma_x2,
                    CPM,
                    ImputationMethod,
                    CalInt_deployment_Mean,
                    CalSlope_deployment_Mean,
                    AUC_deployment_Mean,
                    Brier_deployment_Mean) %>%
      dplyr::mutate(Estimand = forcats::fct_recode(ImputationMethod,
                                                   "E_all" = "All Data Required",
                                                   "E_mean" = "Mean Imputation",
                                                   "E_RI" = "RI (refit)",
                                                   "E_MI" = "MI no Y (refit)",
                                                   "E_PSM" = "Pattern Sub-Model")) %>%
      dplyr::select(-ImputationMethod) %>%
      dplyr::filter(Estimand %in% c("E_all",
                                    "E_mean",
                                    "E_RI",
                                    "E_MI",
                                    "E_PSM")) %>%
      tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                          values_to = "Deployment_Performance",
                          names_to = "Performance_Metric") %>%
      dplyr::mutate(Performance_Metric = stringr::str_remove(Performance_Metric,
                                                             "_deployment_Mean"),
                    Performance_Metric = forcats::fct_recode(Performance_Metric,
                                                             "Brier Score" = "Brier",
                                                             "Calibration Intercept" = "CalInt",
                                                             "Calibration Slope" = "CalSlope")) %>%
      tidyr::pivot_wider(id_cols = c(Simulation_Scenario,
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
                                     gamma_x2,
                                     CPM,
                                     Performance_Metric),
                         names_from = "Estimand",
                         values_from = "Deployment_Performance")
    
    
    #Extract validation performance, for each CPM across all estimands
    Validation_Performance <- df_scenario_results %>%
      #obtain valid development-validation pairs (table 1 of paper):
      dplyr::mutate("combinations" = case_when(
        CPM == "PR_fullyobserved" |
          CPM == "PR_CCA" |
          CPM == "PR_mean" ~ ImputationMethod %in% c("All Data Required",
                                                     "CCA",
                                                     "Mean Imputation",
                                                     "MI with Y (refit)",
                                                     "MI no Y (refit)",
                                                     "RI (refit)"),
        CPM == "PR_RI" ~ ImputationMethod %in% c("All Data Required",
                                                 "CCA",
                                                 "Mean Imputation",
                                                 "MI with Y (refit)",
                                                 "MI no Y (refit)",
                                                 "RI (transported)",
                                                 "RI (refit)"),
        CPM == "PR_MIwithY" ~ ImputationMethod %in% c("All Data Required",
                                                      "CCA",
                                                      "Mean Imputation",
                                                      "MI with Y (transported)",
                                                      "MI with Y (refit)",
                                                      "MI no Y (refit)",
                                                      "RI (refit)"),
        CPM == "PR_MInoY" ~ ImputationMethod %in% c("All Data Required",
                                                    "CCA",
                                                    "Mean Imputation",
                                                    "MI with Y (refit)",
                                                    "MI no Y (transported)",
                                                    "MI no Y (refit)",
                                                    "RI (refit)"),
        
        CPM == "PR_patternsubmodel" ~ ImputationMethod %in% c("All Data Required",
                                                              "CCA",
                                                              "Mean Imputation",
                                                              "MI with Y (refit)",
                                                              "MI no Y (refit)",
                                                              "RI (refit)",
                                                              "Pattern Sub-Model"),
        .default = NA)) %>%
      dplyr::filter(combinations == 1) %>%
      dplyr::select(Simulation_Scenario,
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
                    gamma_x2,
                    CPM,
                    ImputationMethod,
                    CalInt_val_Mean,
                    CalSlope_val_Mean,
                    AUC_val_Mean,
                    Brier_val_Mean) %>%
      dplyr::rename(ValidationImputationMethod = ImputationMethod) %>%
      tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                          values_to = "Validation_Performance",
                          names_to = "Performance_Metric") %>%
      dplyr::mutate(Performance_Metric = stringr::str_remove(Performance_Metric,
                                                             "_val_Mean"),
                    Performance_Metric = forcats::fct_recode(Performance_Metric,
                                                             "Brier Score" = "Brier",
                                                             "Calibration Intercept" = "CalInt",
                                                             "Calibration Slope" = "CalSlope"))
    
    ModelBias <- Validation_Performance %>%
      dplyr::left_join(Deployment_Performance,
                       by = c("Simulation_Scenario",
                              "Y_prev",
                              "X_categorical",
                              "R_prev_dev",
                              "R_prev_deployment",
                              "beta_x1_dev",
                              "beta_x2_dev",
                              "beta_Y_dev",
                              "beta_x1_deployment",
                              "beta_x2_deployment",
                              "beta_Y_deployment",
                              "rho_X",
                              "gamma_x1",
                              "gamma_x2",
                              "CPM",
                              "Performance_Metric")) %>%
      dplyr::mutate("Eall_Model_Bias" = Validation_Performance - E_all,
                    "Emean_Model_Bias" = Validation_Performance - E_mean,
                    "ERI_Model_Bias" = Validation_Performance - E_RI,
                    "EMI_Model_Bias" = Validation_Performance - E_MI,
                    "EPSM_Model_Bias" = Validation_Performance - E_PSM) %>%
      dplyr::mutate(CPM = str_remove(CPM, "PR_"),
                    CPM = forcats::fct_recode(CPM,
                                              "CCA \n developed CPM" = "CCA",
                                              "RI \n developed CPM" = "RI",
                                              "MI no Y \n developed CPM" = "MInoY",
                                              "MI with Y \n developed CPM" = "MIwithY",
                                              "Fully Observed \n developed CPM" = "fullyobserved",
                                              "Mean Imputation \n developed CPM" = "mean",
                                              "PSM \n developed CPM" = "patternsubmodel"),
                    CPM = forcats::fct_relevel(CPM,
                                               "Fully Observed \n developed CPM",
                                               "CCA \n developed CPM",
                                               "Mean Imputation \n developed CPM",
                                               "RI \n developed CPM",
                                               "MI with Y \n developed CPM",
                                               "MI no Y \n developed CPM",
                                               "PSM \n developed CPM"),
                    
                    ValidationImputationMethod = forcats::fct_recode(ValidationImputationMethod,
                                                                     "Fully Observed Data" = "All Data Required"),
                    ValidationImputationMethod = forcats::fct_relevel(ValidationImputationMethod,
                                                                      "Fully Observed Data",
                                                                      "CCA",
                                                                      "Mean Imputation",
                                                                      "RI (refit)",
                                                                      "RI (transported)",
                                                                      "MI with Y (refit)",
                                                                      "MI with Y (transported)",
                                                                      "MI no Y (refit)",
                                                                      "MI no Y (transported)",
                                                                      "Pattern Sub-Model")) 
    
  } else {
    
    #Extract deployment performance, for each CPM across all estimands
    Deployment_Performance <- df_scenario_results %>%
      dplyr::select(Simulation_Scenario,
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
                    gamma_x2,
                    CPM,
                    ImputationMethod,
                    CalInt_deployment_Mean,
                    CalSlope_deployment_Mean,
                    AUC_deployment_Mean,
                    Brier_deployment_Mean) %>%
      dplyr::mutate(Estimand = forcats::fct_recode(ImputationMethod,
                                                   "E_all" = "All Data Required",
                                                   "E_mean" = "Risk Absent Imputation",
                                                   "E_RI" = "RI (refit)",
                                                   "E_MI" = "MI no Y (refit)",
                                                   "E_PSM" = "Pattern Sub-Model")) %>%
      dplyr::select(-ImputationMethod) %>%
      dplyr::filter(Estimand %in% c("E_all",
                                    "E_mean",
                                    "E_RI",
                                    "E_MI",
                                    "E_PSM")) %>%
      tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                          values_to = "Deployment_Performance",
                          names_to = "Performance_Metric") %>%
      dplyr::mutate(Performance_Metric = stringr::str_remove(Performance_Metric,
                                                             "_deployment_Mean"),
                    Performance_Metric = forcats::fct_recode(Performance_Metric,
                                                             "Brier Score" = "Brier",
                                                             "Calibration Intercept" = "CalInt",
                                                             "Calibration Slope" = "CalSlope")) %>%
      tidyr::pivot_wider(id_cols = c(Simulation_Scenario,
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
                                     gamma_x2,
                                     CPM,
                                     Performance_Metric),
                         names_from = "Estimand",
                         values_from = "Deployment_Performance")
    
    
    #Extract validation performance, for each CPM across all estimands
    Validation_Performance <- df_scenario_results %>%
      #obtain valid development-validation pairs (table 1 of paper):
      dplyr::mutate("combinations" = case_when(
        CPM == "PR_fullyobserved" |
          CPM == "PR_CCA" |
          CPM == "PR_zero" ~ ImputationMethod %in% c("All Data Required",
                                                     "CCA",
                                                     "Risk Absent Imputation",
                                                     "MI with Y (refit)",
                                                     "MI no Y (refit)",
                                                     "RI (refit)"),
        CPM == "PR_RI" ~ ImputationMethod %in% c("All Data Required",
                                                 "CCA",
                                                 "Risk Absent Imputation",
                                                 "MI with Y (refit)",
                                                 "MI no Y (refit)",
                                                 "RI (transported)",
                                                 "RI (refit)"),
        CPM == "PR_MIwithY" ~ ImputationMethod %in% c("All Data Required",
                                                      "CCA",
                                                      "Risk Absent Imputation",
                                                      "MI with Y (transported)",
                                                      "MI with Y (refit)",
                                                      "MI no Y (refit)",
                                                      "RI (refit)"),
        CPM == "PR_MInoY" ~ ImputationMethod %in% c("All Data Required",
                                                    "CCA",
                                                    "Risk Absent Imputation",
                                                    "MI with Y (refit)",
                                                    "MI no Y (transported)",
                                                    "MI no Y (refit)",
                                                    "RI (refit)"),
        
        CPM == "PR_patternsubmodel" ~ ImputationMethod %in% c("All Data Required",
                                                              "CCA",
                                                              "Risk Absent Imputation",
                                                              "MI with Y (refit)",
                                                              "MI no Y (refit)",
                                                              "RI (refit)",
                                                              "Pattern Sub-Model"),
        .default = NA)) %>%
      dplyr::filter(combinations == 1) %>%
      dplyr::select(Simulation_Scenario,
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
                    gamma_x2,
                    CPM,
                    ImputationMethod,
                    CalInt_val_Mean,
                    CalSlope_val_Mean,
                    AUC_val_Mean,
                    Brier_val_Mean) %>%
      dplyr::rename(ValidationImputationMethod = ImputationMethod) %>%
      tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                          values_to = "Validation_Performance",
                          names_to = "Performance_Metric") %>%
      dplyr::mutate(Performance_Metric = stringr::str_remove(Performance_Metric,
                                                             "_val_Mean"),
                    Performance_Metric = forcats::fct_recode(Performance_Metric,
                                                             "Brier Score" = "Brier",
                                                             "Calibration Intercept" = "CalInt",
                                                             "Calibration Slope" = "CalSlope"))
    
    
    ModelBias <- Validation_Performance %>%
      dplyr::left_join(Deployment_Performance,
                       by = c("Simulation_Scenario",
                              "Y_prev",
                              "X_categorical",
                              "R_prev_dev",
                              "R_prev_deployment",
                              "beta_x1_dev",
                              "beta_x2_dev",
                              "beta_Y_dev",
                              "beta_x1_deployment",
                              "beta_x2_deployment",
                              "beta_Y_deployment",
                              "rho_X",
                              "gamma_x1",
                              "gamma_x2",
                              "CPM",
                              "Performance_Metric")) %>%
      dplyr::mutate("Eall_Model_Bias" = Validation_Performance - E_all,
                    "Emean_Model_Bias" = Validation_Performance - E_mean,
                    "ERI_Model_Bias" = Validation_Performance - E_RI,
                    "EMI_Model_Bias" = Validation_Performance - E_MI,
                    "EPSM_Model_Bias" = Validation_Performance - E_PSM) %>%
      dplyr::mutate(CPM = str_remove(CPM, "PR_"),
                    CPM = forcats::fct_recode(CPM,
                                              "CCA \n developed CPM" = "CCA",
                                              "RI \n developed CPM" = "RI",
                                              "MI no Y \n developed CPM" = "MInoY",
                                              "MI with Y \n developed CPM" = "MIwithY",
                                              "Fully Observed \n developed CPM" = "fullyobserved",
                                              "Mean Imputation \n developed CPM" = "zero",
                                              "PSM \n developed CPM" = "patternsubmodel"),
                    CPM = forcats::fct_relevel(CPM,
                                               "Fully Observed \n developed CPM",
                                               "CCA \n developed CPM",
                                               "Mean Imputation \n developed CPM",
                                               "RI \n developed CPM",
                                               "MI with Y \n developed CPM",
                                               "MI no Y \n developed CPM",
                                               "PSM \n developed CPM"),
                    
                    ValidationImputationMethod = forcats::fct_recode(ValidationImputationMethod,
                                                                     "Fully Observed Data" = "All Data Required",
                                                                     "Mean Imputation" = "Risk Absent Imputation"),
                    ValidationImputationMethod = forcats::fct_relevel(ValidationImputationMethod,
                                                                      "Fully Observed Data",
                                                                      "CCA",
                                                                      "Mean Imputation",
                                                                      "RI (refit)",
                                                                      "RI (transported)",
                                                                      "MI with Y (refit)",
                                                                      "MI with Y (transported)",
                                                                      "MI no Y (refit)",
                                                                      "MI no Y (transported)",
                                                                      "Pattern Sub-Model")) 
  }
  
  
  #Plot the results
  ModelBias %>%
    dplyr::select(CPM, 
                  ValidationImputationMethod, 
                  Performance_Metric, 
                  dplyr::ends_with("_Bias")) %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("_Bias"),
                        names_to = "Estimand",
                        values_to = "Bias") %>%
    dplyr::mutate(Estimand = stringr:::str_remove(Estimand,
                                                  "_Model_Bias"),
                  Estimand = forcats::fct_recode(Estimand,
                                                 "E-all" = "Eall",
                                                 "E-mean" = "Emean",
                                                 "E-RI" = "ERI",
                                                 "E-MI" = "EMI",
                                                 "E-PSM" = "EPSM"),
                  Estimand = forcats::fct_relevel(Estimand,
                                                  "E-all",
                                                  "E-mean" ,
                                                  "E-RI" ,
                                                  "E-MI" ,
                                                  "E-PSM" )) %>%
    dplyr::filter(!is.na(Bias)) %>%
    dplyr::mutate(CPM = str_remove(CPM, " \n developed CPM"),
                  CPM = forcats::fct_relevel(CPM,
                                             "Fully Observed",
                                             "CCA",
                                             "Mean Imputation",
                                             "RI",
                                             "MI with Y",
                                             "MI no Y",
                                             "PSM")) %>%
    ggplot(aes(
      y = ValidationImputationMethod,
      x = Bias,
      colour = CPM)) +
    geom_vline(data = data.frame("Metric" = c("AUC",
                                              "Brier Score",
                                              "Calibration Intercept",
                                              "Calibration Slope"),
                                 "Bias" = c(0,0,0,0)),
               aes(xintercept = Bias),
               linetype = "dashed") +
    geom_point(position = position_dodge(width = 0.5)) +
    ggh4x::facet_grid2(Performance_Metric ~ Estimand, scales = "free_x", independent = "x") +
    scale_colour_brewer(palette = "Dark2",
                        name = "Developed CPM") +
    ylab("Validation data imputation method") +
    xlab("Validation bias\n(validation performance - deployment performance)") +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "right",
      panel.background = element_rect(fill = "gray95"),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
    ) +
    ggh4x::facetted_pos_scales(
      x = list(
        Performance_Metric == "Calibration Intercept" ~
          scale_x_continuous(
            limits = c(-0.40, 0.40),
            breaks = seq(-0.4, 0.4, by = 0.1)),
        
        Performance_Metric == "Calibration Slope" ~
          scale_x_continuous(
            limits = c(-0.40, 0.40),
            breaks = seq(-0.4, 0.4, by = 0.1)),
        
        Performance_Metric == "AUC" ~ 
          scale_x_continuous(
            limits = c(-0.04, 0.04),
            breaks = seq(-0.04, 0.04, by = 0.01)),
        
        Performance_Metric == "Brier Score" ~
          scale_x_continuous(
            limits = c(-0.02, 0.02),
            breaks = seq(-0.02, 0.02, by = 0.01)))) 
}
