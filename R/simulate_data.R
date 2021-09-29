#' Simulate data for the fHMM package.
#' @description 
#' This function simulates financial data for the fHMM package.
#' @param controls 
#' An object of class \code{fHMM_controls}.
#' @param true_parameters
#' An object of class \code{fHMM_parameters}, used as simulation parameters.
#' @return 
#' A list containing the following elements:
#' \item{data}{A matrix of simulated data.}
#' \item{states0}{A matrix of simulated hidden states.}
#' \item{thetaUncon0}{True parameters in format \code{thetaUncon}.}
#' \item{thetaCon0}{True parameters in format \code{thetaCon}.}
#' \item{thetaList0}{True parameters in format \code{thetaList}.}
#' \item{T_star}{A vector of fine-scale chunk sizes.}

simulate_data = function(controls, true_parameter, seed = NULL){
  
  ### check inputs
  if(class(controls) != "fHMM_controls")
    stop("Not of class 'fHMM_controls'.")
  if(class(true_parameters) != "fHMM_parameters")
    stop("Not of class 'fHMM_parameters'.")
  
  ### simulate data
  if(controls[["model"]]=="hmm"){
    T = as.numeric(controls[["horizon"]][1])
    T_star = NA
    states = simulate_states(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    data = simulate_observations(states,thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],controls[["sdds"]][1])
  }
  if(controls[["model"]]=="hhmm"){ 
    T = as.numeric(controls[["horizon"]][1])
    T_star = compute_fs(fs_time_horizon = controls[["horizon"]][2], T = T)
    states = matrix(NA,T,max(T_star)+1) 
    data = matrix(NA,T,max(T_star)+1)
    states[,1] = simulate_states(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    data[,1] = simulate_observations(states[,1],thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],controls[["sdds"]][1])
    for(t in 1:T){
      S_t = states[t,1]
      states[t,-1] = c(simulate_states(Gamma2delta(thetaList[["Gammas_star"]][[S_t]]),thetaList[["Gammas_star"]][[S_t]],T_star[t]),rep(NA,max(T_star)-T_star[t]))
      data[t,-1] = c(simulate_observations(states[t,-1][!is.na(states[t,-1])],thetaList[["mus_star"]][[S_t]],thetaList[["sigmas_star"]][[S_t]],thetaList[["dfs_star"]][[S_t]],controls[["sdds"]][2]),rep(NA,max(T_star)-T_star[t]))
    }
  }
  
  ### return simulated data
  out = list(
    "data"        = data,
    "states0"     = states,
    "thetaUncon0" = thetaUncon,
    "thetaCon0"   = thetaCon,
    "thetaList0"  = thetaList,
    "T_star"      = T_star
  )
  return(out)
}