library(tidyverse)
library(ggh4x)

## Load the data 
simulation_results_all <- read_rds(here::here("Outputs", 
                                              "simulation_results_all.RDS"))


##### Add the missingness DAG type to each simulation scenario (can differ
##### across development and validation sets - see manuscript):
simulation_results_all <- simulation_results_all %>% 
  dplyr::mutate(DAG_dev = dplyr::case_when(
    beta_x1_dev == 0 & beta_x2_dev == 0 & beta_Y_dev == 0 ~ "DAG (b)",
    beta_x1_dev == 0 & beta_x2_dev != 0 & beta_Y_dev == 0 ~ "DAG (c)", 
    beta_x1_dev != 0 & beta_x2_dev != 0 & beta_Y_dev == 0 ~ "DAG (d)",
    beta_x1_dev == 0 & beta_x2_dev != 0 & beta_Y_dev != 0 ~ "DAG (e)",
    beta_x1_dev != 0 & beta_x2_dev != 0 & beta_Y_dev != 0 ~ "DAG (f)",
    TRUE ~ as.character("not_applicable")
  ),
  DAG_val = dplyr::case_when(
    beta_x1_val == 0 & beta_x2_val == 0 & beta_Y_val == 0 ~ "DAG (b)",
    beta_x1_val == 0 & beta_x2_val != 0 & beta_Y_val == 0 ~ "DAG (c)", 
    beta_x1_val != 0 & beta_x2_val != 0 & beta_Y_val == 0 ~ "DAG (d)",
    beta_x1_val == 0 & beta_x2_val != 0 & beta_Y_val != 0 ~ "DAG (e)",
    beta_x1_val != 0 & beta_x2_val != 0 & beta_Y_val != 0 ~ "DAG (f)",
    TRUE ~ as.character("not_applicable")
  ))


##### Summarise the results across all iterations for each simulation scenario
simulation_results_summarised <- simulation_results_all %>%
  dplyr::group_by(Simulation_Scenario, DAG_dev, DAG_val, 
                  Y_prev, X_categorical, R_prev,
                  beta_x1_dev, beta_x2_dev, beta_Y_dev, 
                  beta_x1_val, beta_x2_val, beta_Y_val,
                  rho_X,
                  gamma_x1, gamma_x2,
                  CPM,
                  Validation_Dataset) %>%
  dplyr::summarise(CalInt_Mean = mean(CalInt_est),
                   CalInt_withinSE = sqrt(mean(CalInt_var)),
                   CalInt_acrossSE = (sum(CalInt_est - CalInt_Mean))/(max(Iteration)-1),
                   CalInt_totalSE = CalInt_withinSE + CalInt_acrossSE + (CalInt_acrossSE/max(Iteration)),
                   CalInt_quantileLower = quantile(CalInt_est, 0.025),
                   CalInt_quantileUpper = quantile(CalInt_est, 0.975),
                   
                   CalSlope_Mean = mean(CalSlope_est),
                   CalSlope_withinSE = sqrt(mean(CalSlope_var)),
                   CalSlope_acrossSE = (sum(CalSlope_est - CalSlope_Mean))/(max(Iteration)-1),
                   CalSlope_totalSE = CalSlope_withinSE + CalSlope_acrossSE + (CalSlope_acrossSE/max(Iteration)),
                   CalSlope_quantileLower = quantile(CalSlope_est, 0.025),
                   CalSlope_quantileUpper = quantile(CalSlope_est, 0.975),
                   
                   AUC_Mean = mean(AUC_est),
                   AUC_withinSE = sqrt(mean(AUC_var)),
                   AUC_acrossSE = (sum(AUC_est - AUC_Mean))/(max(Iteration)-1),
                   AUC_totalSE = AUC_withinSE + AUC_acrossSE + (AUC_acrossSE/max(Iteration)),
                   AUC_quantileLower = quantile(AUC_est, 0.025),
                   AUC_quantileUpper = quantile(AUC_est, 0.975),
                   
                   Brier_Mean = mean(Brier_est),
                   Brier_withinSE = sqrt(mean(Brier_var)),
                   Brier_acrossSE = (sum(Brier_est - Brier_Mean))/(max(Iteration)-1),
                   Brier_totalSE = Brier_withinSE + Brier_acrossSE + (Brier_acrossSE/max(Iteration)),
                   Brier_quantileLower = quantile(Brier_est, 0.025),
                   Brier_quantileUpper = quantile(Brier_est, 0.975),
                   
                   .groups = "drop") %>%
  dplyr::select(-CalInt_withinSE,
                -CalInt_acrossSE,
                -CalSlope_withinSE,
                -CalSlope_acrossSE,
                -AUC_withinSE,
                -AUC_acrossSE,
                -Brier_withinSE,
                -Brier_acrossSE)

write_rds(simulation_results_summarised,
          file = here::here("Outputs", "simulation_results_summarised.RDS"))
# simulation_results_summarised <- read_rds(here::here("Outputs", 
#                                                      "simulation_results_summarised.RDS"))


simulation_results_summarised <- simulation_results_summarised %>%
  dplyr::mutate("Scenario" = paste(paste(DAG_dev, "Dev", sep=" "), 
                                   paste(DAG_val, "Val", sep=" "), 
                                   sep = " -\n "),
                
                Scenario = forcats::fct_relevel(Scenario,
                                                "DAG (b) Dev -\n DAG (b) Val",
                                                "DAG (c) Dev -\n DAG (c) Val",
                                                "DAG (d) Dev -\n DAG (d) Val",
                                                "DAG (e) Dev -\n DAG (e) Val",
                                                "DAG (f) Dev -\n DAG (f) Val"),
                
                Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                         "CCA" = "CCA_val",
                                                         "MI no Y (refit)" = "MI_val_noY_newimputation",
                                                         "MI no Y (transported)" = "MI_val_noY_transportedimputation",
                                                         "MI with Y (refit)" = "MI_val_withY_newimputation",
                                                         "MI with Y (transported)" = "MI_val_withY_transportedimputation",
                                                         "RI (refit)" = "RI_val_newimputation",
                                                         "RI (transported)" = "RI_val_transportedimputation",
                                                         "All Data Required" = "fullyobserved_val",
                                                         "Mean Imputation" = "mean_val",
                                                         "Risk Absent Imputation" ="zero_val",
                                                         "Pattern Sub-Model" = "observed_val"))

####----------------------------------------------------------------------------
## Define two functions for plotting model degradation and model bias for 
## given simulation scenarios
####----------------------------------------------------------------------------
model_degrdation_fnc <- function(df,
                                 DAG_Scenario,
                                 X_categorical_value, 
                                 R_prev_value, 
                                 rho_X_value,
                                 gamma_x1_value,
                                 target_estimands) {
  
  if(all((target_estimands %in% c("E-all",
                                  "E-mean",
                                  "E-RI",
                                  "E-MI",
                                  "E-PSM")))==FALSE) {
    stop("target_estimands vector needs to take correct estimand characters")
  }
  
  if(X_categorical_value == FALSE) {
    df <- df %>%
      dplyr::filter(Scenario == DAG_Scenario &
                      X_categorical == X_categorical_value & 
                      R_prev == R_prev_value & 
                      rho_X == rho_X_value &
                      gamma_x1 == gamma_x1_value,
                    
                    Validation_Dataset %in% c( "All Data Required",
                                               "Mean Imputation",
                                               "RI (refit)",
                                               "MI no Y (refit)",
                                               "Pattern Sub-Model")) %>%
      dplyr::mutate(Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                             "E_all" = "All Data Required",
                                                             "E_mean" = "Mean Imputation",
                                                             "E_RI" = "RI (refit)",
                                                             "E_MI" = "MI no Y (refit)",
                                                             "E_PSM" = "Pattern Sub-Model"))
  } else {
    df <- df %>%
      dplyr::filter(Scenario == DAG_Scenario &
                      X_categorical == X_categorical_value & 
                      R_prev == R_prev_value & 
                      rho_X == rho_X_value &
                      gamma_x1 == gamma_x1_value,
                    
                    Validation_Dataset %in% c( "All Data Required",
                                               "Risk Absent Imputation",
                                               "RI (refit)",
                                               "MI no Y (refit)",
                                               "Pattern Sub-Model")) %>%
      dplyr::mutate(Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                             "E_all" = "All Data Required",
                                                             "E_mean" = "Risk Absent Imputation",
                                                             "E_RI" = "RI (refit)",
                                                             "E_MI" = "MI no Y (refit)",
                                                             "E_PSM" = "Pattern Sub-Model"))
  }
  
  #Extract 'true' predictive performance as the performance of the DGM within
  #the validation set corresponding to the target estimand
  true_predictive_performance <- df %>%
    dplyr::filter(CPM == "PR_DGM") %>%
    dplyr::select(Simulation_Scenario,
                  Scenario,
                  Y_prev,
                  X_categorical,
                  R_prev,
                  rho_X,
                  gamma_x1,
                  gamma_x2,
                  Validation_Dataset,
                  dplyr::ends_with("_Mean")) %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                        values_to = "True_Performance",
                        names_to = "Performance_Metric")
  
  #Extract the predictive performance of each CPM within the 
  #validation set corresponding to the target estimand 
  CPM_predictive_performance <- df %>%
    dplyr::filter(CPM != "PR_DGM") %>%
    dplyr::select(Simulation_Scenario,
                  Scenario,
                  Y_prev,
                  X_categorical,
                  R_prev,
                  rho_X,
                  gamma_x1,
                  gamma_x2,
                  CPM,
                  Validation_Dataset,
                  dplyr::ends_with("_Mean")) %>%
    tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                        values_to = "Model_Performance",
                        names_to = "Performance_Metric")
  
  #Calculate model degradation:
  ModelDegradation <- CPM_predictive_performance %>%
    dplyr::left_join(true_predictive_performance,
                     by = c("Simulation_Scenario",
                            "Scenario",
                            "Y_prev",
                            "X_categorical",
                            "R_prev",
                            "rho_X",
                            "gamma_x1",
                            "gamma_x2",
                            "Validation_Dataset",
                            "Performance_Metric"))
  ModelDegradation$True_Performance[which(
    ModelDegradation$Validation_Dataset == "E_PSM"
    )] <- true_predictive_performance$True_Performance[which(
      true_predictive_performance$Validation_Dataset == "E_all"
    )]
  
  if(X_categorical_value == FALSE) {
    
    ModelDegradation <- ModelDegradation %>%
      dplyr::mutate("Model_Degradation" = Model_Performance - True_Performance) %>%
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
                    
                    Performance_Metric = stringr::str_remove(Performance_Metric,
                                                             "_Mean"),
                    Performance_Metric = forcats::fct_recode(Performance_Metric,
                                                             "Brier Score" = "Brier",
                                                             "Calibration Intercept" = "CalInt",
                                                             "Calibration Slope" = "CalSlope"),
                    
                    Validation_Dataset = forcats::fct_relevel(Validation_Dataset,
                                                              "E_all",
                                                              "E_mean",
                                                              "E_RI",
                                                              "E_MI",
                                                              "E_PSM"),
                    Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                             "E-all" = "E_all",
                                                             "E-mean" = "E_mean",
                                                             "E-RI" = "E_RI",
                                                             "E-MI" = "E_MI",
                                                             "E-PSM" = "E_PSM"))
  } else{
    
    ModelDegradation <- ModelDegradation %>%
      dplyr::mutate("Model_Degradation" = Model_Performance - True_Performance) %>%
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
                                               "PSM"),
                    
                    Performance_Metric = stringr::str_remove(Performance_Metric,
                                                             "_Mean"),
                    Performance_Metric = forcats::fct_recode(Performance_Metric,
                                                             "Brier Score" = "Brier",
                                                             "Calibration Intercept" = "CalInt",
                                                             "Calibration Slope" = "CalSlope"),
                    
                    Validation_Dataset = forcats::fct_relevel(Validation_Dataset,
                                                              "E_all",
                                                              "E_mean",
                                                              "E_RI",
                                                              "E_MI",
                                                              "E_PSM"),
                    Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                             "E-all" = "E_all",
                                                             "E-mean" = "E_mean",
                                                             "E-RI" = "E_RI",
                                                             "E-MI" = "E_MI",
                                                             "E-PSM" = "E_PSM"))
  }
  
  ModelDegradation %>%
    dplyr::filter(Validation_Dataset %in% target_estimands) %>%
    ggplot(aes(x = Model_Degradation, y = CPM)) +
    geom_point() +
    geom_vline(xintercept = 0, linetype = "dotted") +
    ggh4x::facet_grid2(Performance_Metric ~ Validation_Dataset, scales = "free_y") +
    xlab("Model Degradation \n (deployment performance - true model performance)") +
    ylab("Developed CPM") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "gray90"),
          panel.spacing.x = unit(0.5, "lines"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          plot.title = element_text(size = 14, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))
}


model_bias_fnc <- function(df,
                           DAG_Scenario,
                           X_categorical_value, 
                           R_prev_value, 
                           rho_X_value,
                           gamma_x1_value,
                           target_estimands) {
  
  if(all((target_estimands %in% c("E-all",
                                 "E-mean",
                                 "E-RI",
                                 "E-MI",
                                 "E-PSM")))==FALSE) {
    stop("target_estimands vector needs to take correct estimand characters")
  }
  
  #filter to the required simulation scenario:
  df <- df %>%
    dplyr::filter(Scenario == DAG_Scenario &
                    X_categorical == X_categorical_value & 
                    R_prev == R_prev_value & 
                    rho_X == rho_X_value &
                    gamma_x1 == gamma_x1_value)
  
  #remove the DGM model performance as dont need true performance for this part
  df <- df %>%
    dplyr::filter(CPM != "PR_DGM")
  
  #define deployment performance, for each CPM across all estimands
  if(X_categorical_value == FALSE) {
    Deployment_Performance <- df %>%
      dplyr::filter(Validation_Dataset %in% c( "All Data Required",
                                               "Mean Imputation",
                                               "RI (refit)",
                                               "MI no Y (refit)",
                                               "Pattern Sub-Model")) %>%
      dplyr::mutate(Target_Estimand = forcats::fct_recode(Validation_Dataset,
                                                          "E_all" = "All Data Required",
                                                          "E_mean" = "Mean Imputation",
                                                          "E_RI" = "RI (refit)",
                                                          "E_MI" = "MI no Y (refit)",
                                                          "E_PSM" = "Pattern Sub-Model")) %>%
      dplyr::select(Simulation_Scenario,
                    Scenario,
                    Y_prev,
                    X_categorical,
                    R_prev,
                    rho_X,
                    gamma_x1,
                    gamma_x2,
                    CPM,
                    Target_Estimand,
                    dplyr::ends_with("_Mean")) %>%
      tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                          values_to = "Deployment_Performance",
                          names_to = "Performance_Metric") %>%
      tidyr::pivot_wider(id_cols = c(Simulation_Scenario,
                                     Scenario,
                                     Y_prev,
                                     X_categorical,
                                     R_prev,
                                     rho_X,
                                     gamma_x1,
                                     gamma_x2,
                                     CPM,
                                     Performance_Metric),
                         names_from = "Target_Estimand",
                         values_from = "Deployment_Performance")
  } else {
    Deployment_Performance <- df %>%
      dplyr::filter(Validation_Dataset %in% c( "All Data Required",
                                               "Risk Absent Imputation",
                                               "RI (refit)",
                                               "MI no Y (refit)",
                                               "Pattern Sub-Model")) %>%
      dplyr::mutate(Target_Estimand = forcats::fct_recode(Validation_Dataset,
                                                          "E_all" = "All Data Required",
                                                          "E_mean" = "Risk Absent Imputation",
                                                          "E_RI" = "RI (refit)",
                                                          "E_MI" = "MI no Y (refit)",
                                                          "E_PSM" = "Pattern Sub-Model")) %>%
      dplyr::select(Simulation_Scenario,
                    Scenario,
                    Y_prev,
                    X_categorical,
                    R_prev,
                    rho_X,
                    gamma_x1,
                    gamma_x2,
                    CPM,
                    Target_Estimand,
                    dplyr::ends_with("_Mean")) %>%
      tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                          values_to = "Deployment_Performance",
                          names_to = "Performance_Metric") %>%
      tidyr::pivot_wider(id_cols = c(Simulation_Scenario,
                                     Scenario,
                                     Y_prev,
                                     X_categorical,
                                     R_prev,
                                     rho_X,
                                     gamma_x1,
                                     gamma_x2,
                                     CPM,
                                     Performance_Metric),
                         names_from = "Target_Estimand",
                         values_from = "Deployment_Performance")
  }
  
  if(X_categorical_value == FALSE) {
    
    CPM_performance <- df %>%
      #obtain valid development-validation pairs (table 1 of paper):
      dplyr::mutate("combinations" = case_when(
        CPM == "PR_fullyobserved" |
          CPM == "PR_CCA" |
          CPM == "PR_mean" ~ Validation_Dataset %in% c("All Data Required",
                                                       "CCA",
                                                       "Mean Imputation",
                                                       "MI with Y (refit)",
                                                       "MI no Y (refit)",
                                                       "RI (refit)"),
        CPM == "PR_RI" ~ Validation_Dataset %in% c("All Data Required",
                                                   "CCA",
                                                   "Mean Imputation",
                                                   "MI with Y (refit)",
                                                   "MI no Y (refit)",
                                                   "RI (transported)",
                                                   "RI (refit)"),
        CPM == "PR_MIwithY" ~ Validation_Dataset %in% c("All Data Required",
                                                        "CCA",
                                                        "Mean Imputation",
                                                        "MI with Y (transported)",
                                                        "MI with Y (refit)",
                                                        "MI no Y (refit)",
                                                        "RI (refit)"),
        CPM == "PR_MInoY" ~ Validation_Dataset %in% c("All Data Required",
                                                      "CCA",
                                                      "Mean Imputation",
                                                      "MI with Y (refit)",
                                                      "MI no Y (transported)",
                                                      "MI no Y (refit)",
                                                      "RI (refit)"),
        
        CPM == "PR_patternsubmodel" ~ Validation_Dataset %in% c("All Data Required",
                                                                "CCA",
                                                                "Mean Imputation",
                                                                "MI with Y (refit)",
                                                                "MI no Y (refit)",
                                                                "RI (refit)",
                                                                "Pattern Sub-Model"),
        .default = NA)) %>%
      dplyr::filter(combinations == 1) %>%
      dplyr::select(Simulation_Scenario,
                    Scenario,
                    Y_prev,
                    X_categorical,
                    R_prev,
                    rho_X,
                    gamma_x1,
                    gamma_x2,
                    CPM,
                    Validation_Dataset,
                    dplyr::ends_with("_Mean")) %>%
      tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                          values_to = "Model_Performance",
                          names_to = "Performance_Metric")
    
    ModelBias <- CPM_performance %>%
      dplyr::left_join(Deployment_Performance,
                       by = c("Simulation_Scenario",
                              "Scenario",
                              "Y_prev",
                              "X_categorical",
                              "R_prev",
                              "rho_X",
                              "gamma_x1",
                              "gamma_x2",
                              "CPM",
                              "Performance_Metric")) %>%
      dplyr::mutate("Eall_Model_Bias" = Model_Performance - E_all,
                    "Emean_Model_Bias" = Model_Performance - E_mean,
                    "ERI_Model_Bias" = Model_Performance - E_RI,
                    "EMI_Model_Bias" = Model_Performance - E_MI,
                    "EPSM_Model_Bias" = Model_Performance - E_PSM) %>%
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
                    
                    Performance_Metric = stringr::str_remove(Performance_Metric,
                                                             "_Mean"),
                    Performance_Metric = forcats::fct_recode(Performance_Metric,
                                                             "Brier Score" = "Brier",
                                                             "Calibration Intercept" = "CalInt",
                                                             "Calibration Slope" = "CalSlope"),
                    
                    Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                             "Fully Observed Data" = "All Data Required"),
                    Validation_Dataset = forcats::fct_relevel(Validation_Dataset,
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
    
    CPM_performance <- df %>%
      #obtain valid development-validation pairs (table 1 of paper):
      dplyr::mutate("combinations" = case_when(
        CPM == "PR_fullyobserved" |
          CPM == "PR_CCA" |
          CPM == "PR_zero" ~ Validation_Dataset %in% c("All Data Required",
                                                       "CCA",
                                                       "Risk Absent Imputation",
                                                       "MI with Y (refit)",
                                                       "MI no Y (refit)",
                                                       "RI (refit)"),
        CPM == "PR_RI" ~ Validation_Dataset %in% c("All Data Required",
                                                   "CCA",
                                                   "Risk Absent Imputation",
                                                   "MI with Y (refit)",
                                                   "MI no Y (refit)",
                                                   "RI (transported)",
                                                   "RI (refit)"),
        CPM == "PR_MIwithY" ~ Validation_Dataset %in% c("All Data Required",
                                                        "CCA",
                                                        "Risk Absent Imputation",
                                                        "MI with Y (transported)",
                                                        "MI with Y (refit)",
                                                        "MI no Y (refit)",
                                                        "RI (refit)"),
        CPM == "PR_MInoY" ~ Validation_Dataset %in% c("All Data Required",
                                                      "CCA",
                                                      "Risk Absent Imputation",
                                                      "MI with Y (refit)",
                                                      "MI no Y (transported)",
                                                      "MI no Y (refit)",
                                                      "RI (refit)"),
        
        CPM == "PR_patternsubmodel" ~ Validation_Dataset %in% c("All Data Required",
                                                                "CCA",
                                                                "Risk Absent Imputation",
                                                                "MI with Y (refit)",
                                                                "MI no Y (refit)",
                                                                "RI (refit)",
                                                                "Pattern Sub-Model"),
        .default = NA)) %>%
      dplyr::filter(combinations == 1) %>%
      dplyr::select(Simulation_Scenario,
                    Scenario,
                    Y_prev,
                    X_categorical,
                    R_prev,
                    rho_X,
                    gamma_x1,
                    gamma_x2,
                    CPM,
                    Validation_Dataset,
                    dplyr::ends_with("_Mean")) %>%
      tidyr::pivot_longer(cols = dplyr::ends_with("_Mean"),
                          values_to = "Model_Performance",
                          names_to = "Performance_Metric")
    
    ModelBias <- CPM_performance %>%
      dplyr::left_join(Deployment_Performance,
                       by = c("Simulation_Scenario",
                              "Scenario",
                              "Y_prev",
                              "X_categorical",
                              "R_prev",
                              "rho_X",
                              "gamma_x1",
                              "gamma_x2",
                              "CPM",
                              "Performance_Metric")) %>%
      dplyr::mutate("Eall_Model_Bias" = Model_Performance - E_all,
                    "Emean_Model_Bias" = Model_Performance - E_mean,
                    "ERI_Model_Bias" = Model_Performance - E_RI,
                    "EMI_Model_Bias" = Model_Performance - E_MI,
                    "EPSM_Model_Bias" = Model_Performance - E_PSM) %>%
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
                    
                    Performance_Metric = stringr::str_remove(Performance_Metric,
                                                             "_Mean"),
                    Performance_Metric = forcats::fct_recode(Performance_Metric,
                                                             "Brier Score" = "Brier",
                                                             "Calibration Intercept" = "CalInt",
                                                             "Calibration Slope" = "CalSlope"),
                    
                    Validation_Dataset = forcats::fct_recode(Validation_Dataset,
                                                             "Fully Observed Data" = "All Data Required"),
                    Validation_Dataset = forcats::fct_relevel(Validation_Dataset,
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
  
  ModelBias %>%
    dplyr::select(CPM, 
                  Validation_Dataset, 
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
    dplyr::filter(Estimand %in% target_estimands) %>%
    ggplot(aes(x = Bias,
               y = Validation_Dataset,
               color = Performance_Metric)) +
    geom_point(aes(shape = Performance_Metric)) +
    scale_color_brewer(palette = "Set1") +
    xlab("Validation Bias \n (validation performance - deployment performance)") +
    ylab("Validation Data Imputation Method") +
    geom_vline(data = data.frame("Metric" = c("AUC",
                                              "Brier Score",
                                              "Calibration Intercept",
                                              "Calibration Slope"),
                                 "Bias" = c(0,0,0,0)),
               aes(xintercept = Bias),
               linetype = "dashed") +
    ggh4x::facet_grid2(Estimand ~ CPM, scales = "fixed") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          panel.background = element_rect(fill = "gray90"),  
          panel.spacing.x = unit(0.5, "lines"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          plot.title = element_text(size = 14, hjust = 0.5),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)) 
}




####----------------------------------------------------------------------------
### The below plots look at scenarios where X_1 is continuous, contains 50%
### missing data, rho_X=0.75, and where gamma_1=gamma_2=0.5. Results were 
### quantitatively similar in other scenarios 
####----------------------------------------------------------------------------

### First look at cases with consistent missingness DAGs across development and validation
model_degrdation_fnc(df = simulation_results_summarised,
                     DAG_Scenario = "DAG (b) Dev -\n DAG (b) Val",
                     X_categorical_value = FALSE, 
                     R_prev_value = 0.5, 
                     rho_X_value = 0.75,
                     gamma_x1_value = 0.5,
                     target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))
model_bias_fnc(df = simulation_results_summarised,
               DAG_Scenario = "DAG (b) Dev -\n DAG (b) Val",
               X_categorical_value = FALSE, 
               R_prev_value = 0.5, 
               rho_X_value = 0.75,
               gamma_x1_value = 0.5,
               target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))


model_degrdation_fnc(df = simulation_results_summarised,
                     DAG_Scenario = "DAG (c) Dev -\n DAG (c) Val",
                     X_categorical_value = FALSE, 
                     R_prev_value = 0.5, 
                     rho_X_value = 0.75,
                     gamma_x1_value = 0.5,
                     target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))
model_bias_fnc(df = simulation_results_summarised,
               DAG_Scenario = "DAG (c) Dev -\n DAG (c) Val",
               X_categorical_value = FALSE, 
               R_prev_value = 0.5, 
               rho_X_value = 0.75,
               gamma_x1_value = 0.5,
               target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))


model_degrdation_fnc(df = simulation_results_summarised,
                     DAG_Scenario = "DAG (d) Dev -\n DAG (d) Val",
                     X_categorical_value = FALSE, 
                     R_prev_value = 0.5, 
                     rho_X_value = 0.75,
                     gamma_x1_value = 0.5,
                     target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))
model_bias_fnc(df = simulation_results_summarised,
               DAG_Scenario = "DAG (d) Dev -\n DAG (d) Val",
               X_categorical_value = FALSE, 
               R_prev_value = 0.5, 
               rho_X_value = 0.75,
               gamma_x1_value = 0.5,
               target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))


model_degrdation_fnc(df = simulation_results_summarised,
                     DAG_Scenario = "DAG (e) Dev -\n DAG (e) Val",
                     X_categorical_value = FALSE, 
                     R_prev_value = 0.5, 
                     rho_X_value = 0.75,
                     gamma_x1_value = 0.5,
                     target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))
model_bias_fnc(df = simulation_results_summarised,
               DAG_Scenario = "DAG (e) Dev -\n DAG (e) Val",
               X_categorical_value = FALSE, 
               R_prev_value = 0.5, 
               rho_X_value = 0.75,
               gamma_x1_value = 0.5,
               target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))


model_degrdation_fnc(df = simulation_results_summarised,
                     DAG_Scenario = "DAG (f) Dev -\n DAG (f) Val",
                     X_categorical_value = FALSE, 
                     R_prev_value = 0.5, 
                     rho_X_value = 0.75,
                     gamma_x1_value = 0.5,
                     target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))
model_bias_fnc(df = simulation_results_summarised,
               DAG_Scenario = "DAG (f) Dev -\n DAG (f) Val",
               X_categorical_value = FALSE, 
               R_prev_value = 0.5, 
               rho_X_value = 0.75,
               gamma_x1_value = 0.5,
               target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))


### Second look at cases with different missingness DAGs across development and validation

model_degrdation_fnc(df = simulation_results_summarised,
                     DAG_Scenario = "DAG (c) Dev -\n DAG (f) Val",
                     X_categorical_value = FALSE, 
                     R_prev_value = 0.5, 
                     rho_X_value = 0.75,
                     gamma_x1_value = 0.5,
                     target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))
model_bias_fnc(df = simulation_results_summarised,
               DAG_Scenario = "DAG (c) Dev -\n DAG (f) Val",
               X_categorical_value = FALSE, 
               R_prev_value = 0.5, 
               rho_X_value = 0.75,
               gamma_x1_value = 0.5,
               target_estimands = c("E-all", "E-mean", "E-RI", "E-MI", "E-PSM"))
