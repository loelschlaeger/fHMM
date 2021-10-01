#' Simulate data for the fHMM package.
#' @description 
#' This function simulates financial data for the fHMM package.
#' @inheritParams prepare_data
#' @param seed
#' Set a seed for the data simulation.
#' @return 
#' A list containing the following elements:

simulate_data = function(controls, true_parameters, seed = NULL){
  
  ### check inputs
  if(class(controls) != "fHMM_controls")
    stop("Not of class 'fHMM_controls'.")
  if(class(true_parameters) != "fHMM_parameters")
    stop("Not of class 'fHMM_parameters'.")
  
  ### simulate data
  if(!controls[["hierarchy"]]){
    markov_chain = simulate_markov_chain(Gamma = true_parameters$Gamma,
                                         T = controls[["horizon"]][1],
                                         seed = seed) 
    data = simulate_observations(markov_chain = markov_chain,
                                 sdd = controls[["sdds"]][[1]]$name,
                                 mus = true_parameters$mus,
                                 sigmas = true_parameters$sigmas,
                                 dfs = true_parameters$dfs)
  } else {
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