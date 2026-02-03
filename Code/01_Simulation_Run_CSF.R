# ##############################################################################

# Author of code: Antonia D. Tsvetanova & Glen P. Martin

# This is code for a simulation study presented in a manuscript entitled: 
# Compatibility of missing data handling methods across stages of producing CPMs

# ##############################################################################

####----------------------------------------------------------------------------
## This script runs the simulations across all scenarios: implemented using the 
## computational shared facility (CSF) at University of Manchester
####----------------------------------------------------------------------------

#Load the simulation functions
source("./00_Simulation_Functions.R") 

library(tidyverse)

# Define a dataset that includes all combinations of simulation parameters:
sims_parameters <- tidyr::crossing(
  Y_prev = c(0.2),
  X_categorical = c(TRUE, FALSE), 
  R_prev = c(0.9, 0.8, 0.5), #10%, 20% and 50% missingness, respectively
  beta_x1_dev = c(0, 0.5), 
  beta_x2_dev = c(0, 0.5), 
  beta_Y_dev = c(0, 0.5),  
  beta_x1_val = c(0, 0.5), 
  beta_x2_val = c(0, 0.5), 
  beta_Y_val = c(0, 0.5),  
  rho_X = c(0, 0.75),
  gamma_x1 = c(0, 0.5), 
  gamma_x2 = 0.5
) %>% 
  dplyr::mutate(beta_cases_dev = dplyr::case_when(
    beta_x1_dev == 0 & beta_x2_dev == 0 & beta_Y_dev == 0 ~ "DAG_b_dev",
    beta_x1_dev == 0 & beta_x2_dev != 0 & beta_Y_dev == 0 ~ "DAG_c_dev", 
    beta_x1_dev != 0 & beta_x2_dev != 0 & beta_Y_dev == 0 ~ "DAG_d_dev",
    beta_x1_dev == 0 & beta_x2_dev != 0 & beta_Y_dev != 0 ~ "DAG_e_dev",
    beta_x1_dev != 0 & beta_x2_dev != 0 & beta_Y_dev != 0 ~ "DAG_f_dev",
    TRUE ~ as.character("not_applicable")
  ),
  beta_cases_val = dplyr::case_when(
    beta_x1_val == 0 & beta_x2_val == 0 & beta_Y_val == 0 ~ "DAG_b_val",
    beta_x1_val == 0 & beta_x2_val != 0 & beta_Y_val == 0 ~ "DAG_c_val", 
    beta_x1_val != 0 & beta_x2_val != 0 & beta_Y_val == 0 ~ "DAG_d_val",
    beta_x1_val == 0 & beta_x2_val != 0 & beta_Y_val != 0 ~ "DAG_e_val",
    beta_x1_val != 0 & beta_x2_val != 0 & beta_Y_val != 0 ~ "DAG_f_val",
    TRUE ~ as.character("not_applicable")
  )) %>% 
  dplyr::filter(beta_cases_dev != "not_applicable" & 
                  beta_cases_val != "not_applicable") %>%
  dplyr::select(-beta_cases_dev,
                -beta_cases_val)

taskid <- commandArgs(trailingOnly = T) 
taskid <- as.numeric(taskid)

# number of repeats per scenario
n_rep <- 100
#Define the size of development, validation and implementation, respectively:
N_dev <- 100000
N_val <- 100000 

set.seed(465475 * taskid)

#run the simulation study:
simulation_results <- simulation_nrun_fnc(n_iter = n_rep, 
                                          N_dev = N_dev,
                                          N_val = N_val, 
                                          Y_prev = sims_parameters$Y_prev[taskid],
                                          X_categorical = sims_parameters$X_categorical[taskid], 
                                          R_prev = sims_parameters$R_prev[taskid],
                                          beta_x1_dev = sims_parameters$beta_x1_dev[taskid], 
                                          beta_x2_dev = sims_parameters$beta_x2_dev[taskid], 
                                          beta_Y_dev = sims_parameters$beta_Y_dev[taskid], 
                                          beta_x1_val = sims_parameters$beta_x1_val[taskid], 
                                          beta_x2_val = sims_parameters$beta_x2_val[taskid],  
                                          beta_Y_val = sims_parameters$beta_Y_val[taskid], 
                                          rho_X = sims_parameters$rho_X[taskid],
                                          gamma_x1 = sims_parameters$gamma_x1[taskid], 
                                          gamma_x2 = sims_parameters$gamma_x2[taskid])

#attach the simulation parameters for this scenario to the simulation results:
simulation_results <- simulation_results %>%
  dplyr::mutate("Simulation_Scenario" = taskid,
                "Y_prev" = sims_parameters$Y_prev[taskid],
                "X_categorical" = sims_parameters$X_categorical[taskid], 
                "R_prev" = sims_parameters$R_prev[taskid],
                "beta_x1_dev" = sims_parameters$beta_x1_dev[taskid], 
                "beta_x2_dev" = sims_parameters$beta_x2_dev[taskid], 
                "beta_Y_dev" = sims_parameters$beta_Y_dev[taskid], 
                "beta_x1_val" = sims_parameters$beta_x1_val[taskid], 
                "beta_x2_val" = sims_parameters$beta_x2_val[taskid],  
                "beta_Y_val" = sims_parameters$beta_Y_val[taskid], 
                "rho_X" = sims_parameters$rho_X[taskid],
                "gamma_x1" = sims_parameters$gamma_x1[taskid], 
                "gamma_x2" = sims_parameters$gamma_x2[taskid],
                .before = "Iteration")

readr::write_rds(simulation_results, 
                 file = paste("./simulation_results_", taskid, 
                              ".RDS", sep = ""))

warnings()