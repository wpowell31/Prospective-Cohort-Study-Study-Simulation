# ######################################################################### #
##                                                                         ##
##  Author: Will Powell                                                    ##
##  Program: 721simfunctions                                               ##
##  Purpose: Written functions used for simulating RR, OR, and RD for the  ##
##  BIOSTAT 721 Simulation Project                                         ##
##                                                                         ##                          
##                                                                         ##
##  Change Log:                                                            ##
##  12/05/2022 File started                                                ##
##                                                                         ##
# ######################################################################### #



simulationRR <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk, fexposure, fnonexposure, N){
  cohort_size <- num_exposure + num_nonexposure
  prob_exposure <- num_exposure/(num_exposure + num_nonexposure)
  rel_risk <- rep(0, N)
  
  for(j in 1:N){
    
    exposure_true <- rbinom(cohort_size, 1, prob_exposure)
    outcome_true <- rep(0,cohort_size)
    exposure_observed <- rep(0,cohort_size)
    indicator <- rep(1,cohort_size)
    sim_data <- data.frame(exposure_true, outcome_true, exposure_observed, indicator)
    
    for(i in 1:cohort_size){
      if(exposure_true[i] == 1){
        sim_data[i,2] <- rbinom(1, 1, exposure_risk)
        sim_data[i,3] <- rbinom(1, 1, 1-fnonexposure)
      }else{
        sim_data[i,2] <- rbinom(1, 1, nonexposure_risk)
        sim_data[i,3] <- rbinom(1, 1, fexposure)
      }
    }
    r_t <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 1),3])/sum(sim_data[,3])
    r_c <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 0),2])/sum(sim_data[which(sim_data[,3] == 0),4])
    rel_risk[j] <- r_t/r_c
    
  }
  
  return(rel_risk)
}


simulationRD <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk, fexposure, fnonexposure, N){
  cohort_size <- num_exposure + num_nonexposure
  prob_exposure <- num_exposure/(num_exposure + num_nonexposure)
  rel_risk <- rep(0, N)
  
  for(j in 1:N){
    
    exposure_true <- rbinom(cohort_size, 1, prob_exposure)
    outcome_true <- rep(0,cohort_size)
    exposure_observed <- rep(0,cohort_size)
    indicator <- rep(1,cohort_size)
    sim_data <- data.frame(exposure_true, outcome_true, exposure_observed, indicator)
    
    for(i in 1:cohort_size){
      if(exposure_true[i] == 1){
        sim_data[i,2] <- rbinom(1, 1, exposure_risk)
        sim_data[i,3] <- rbinom(1, 1, 1-fnonexposure)
      }else{
        sim_data[i,2] <- rbinom(1, 1, nonexposure_risk)
        sim_data[i,3] <- rbinom(1, 1, fexposure)
      }
    }
    r_t <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 1),3])/sum(sim_data[,3])
    r_c <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 0),2])/sum(sim_data[which(sim_data[,3] == 0),4])
    rel_risk[j] <- r_t - r_c
    
  }
  
  return(rel_risk)
}


simulationOR <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk, fexposure, fnonexposure, N){
  cohort_size <- num_exposure + num_nonexposure
  prob_exposure <- num_exposure/(num_exposure + num_nonexposure)
  rel_risk <- rep(0, N)
  
  for(j in 1:N){
    
    exposure_true <- rbinom(cohort_size, 1, prob_exposure)
    print(j)
    outcome_true <- rep(0,cohort_size)
    exposure_observed <- rep(0,cohort_size)
    indicator <- rep(1,cohort_size)
    sim_data <- data.frame(exposure_true, outcome_true, exposure_observed, indicator)
    
    for(i in 1:cohort_size){
      if(exposure_true[i] == 1){
        sim_data[i,2] <- rbinom(1, 1, exposure_risk)
        sim_data[i,3] <- rbinom(1, 1, 1-fnonexposure)
      }else{
        sim_data[i,2] <- rbinom(1, 1, nonexposure_risk)
        sim_data[i,3] <- rbinom(1, 1, fexposure)
      }
    }
    o_t <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 1),3])/sum(sim_data[which(sim_data[,2] == 0 & sim_data[,3] == 1),3])
    o_c <- sum(sim_data[which(sim_data[,2] == 1 & sim_data[,3] == 0),2])/sum(sim_data[which(sim_data[,2] == 0 & sim_data[,3] == 0),4])
    rel_risk[j] <- o_t/o_c
    
  }
  
  return(rel_risk)
}