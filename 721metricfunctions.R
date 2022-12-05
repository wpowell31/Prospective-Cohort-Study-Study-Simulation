# ######################################################################### #
##                                                                         ##
##  Author: Will Powell                                                    ##
##  Program: 721metricfunctions                                            ##
##  Purpose: Written functions used for calculating RR, OR, and RD for the ##
##  BIOSTAT 721 Simulation Project                                         ##
##                                                                         ##                          
##                                                                         ##
##  Change Log:                                                            ##
##  12/05/2022 File started                                                ##
##                                                                         ##
# ######################################################################### #


RR <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk){
  num_exposure_success <- as.integer(num_exposure * exposure_risk)
  num_nonexposure_success <- as.integer(num_nonexposure * nonexposure_risk)
  r_t <- num_exposure_success/num_exposure
  r_c <- num_nonexposure_success/num_nonexposure
  return(r_t/r_c)
}

RD <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk){
  num_exposure_success <- as.integer(num_exposure * exposure_risk)
  num_nonexposure_success <- as.integer(num_nonexposure * nonexposure_risk)
  r_t <- num_exposure_success/num_exposure
  r_c <- num_nonexposure_success/num_nonexposure
  return(r_t - r_c)
}

OR <- function(num_exposure, exposure_risk, num_nonexposure, nonexposure_risk){
  num_exposure_success <- as.integer(num_exposure * exposure_risk)
  num_nonexposure_success <- as.integer(num_nonexposure * nonexposure_risk)
  o_t <- num_exposure_success/(num_exposure - num_exposure_success) 
  o_c <- num_nonexposure_success/(num_nonexposure - num_nonexposure_success)
  return(o_t/o_c)
}