#' Simulate data from a (hierarchical) hidden Markov model
#' @param controls A list of controls
#' @param sim_par A list of model parameters for simulation in \code{thetaList} format, default \code{NULL}
#' @return A list containing the following elements:
#' \item{logReturns}{Simulated log-returns}
#' \item{states0}{Simulated hidden states}
#' \item{thetaUncon0}{True parameters in format \code{thetaUncon}}
#' \item{thetaCon0}{True parameters in format \code{thetaCon}}
#' \item{thetaList0}{True parameters in format \code{thetaList}}
#' \item{T_star}{Vector of fine-scale chunk sizes}
simulate_data = function(controls,sim_par=NULL){
  if(is.null(controls[["controls_checked"]])) stop(sprintf("%s (%s)",exception("F.6")[2],exception("F.6")[1]),call.=FALSE)
  if(!is.null(controls[["seed"]])) set.seed(controls[["seed"]])
  T = as.numeric(controls[["time_horizon"]][1])
  ### define simulation parameters
  if(!is.null(sim_par)){
    thetaUncon = thetaList2thetaUncon(sim_par,controls)
  } else {
    thetaUncon = init_est(controls)
  }
  thetaCon   = thetaUncon2thetaCon(thetaUncon,controls)
  thetaList  = thetaCon2thetaList(thetaCon,controls)
  ### simulate hidden states
  simulate_states = function(delta,Gamma,T){
    no_states = length(delta)
    states = numeric(T)
    states[1] = sample(1:no_states,1,prob=delta)
    for(t in 2:T){
      states[t] = sample(1:no_states,1,prob=Gamma[states[t-1],])
    }
    return(states)
  }
  ### simulate observations
  simulate_observations = function(states,mus,sigmas,dfs,sdd){
    T = length(states)
    logReturns = numeric(T)
    for(t in 1:T){
      if(sdd == "t"){
        logReturns[t] = rt(1,dfs[states[t]])*sigmas[states[t]]+mus[states[t]]
      }
      if(sdd == "gamma"){
        logReturns[t] = rgamma(1,shape=mus[states[t]]^2/sigmas[states[t]]^2,scale=sigmas[states[t]]^2/mus[states[t]])
      }
    }
    return(logReturns)
  }
  if(controls[["model"]]=="HMM"){
    T_star = NA
    states = simulate_states(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    logReturns = simulate_observations(states,thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],controls[["sdds"]][1])
  }
  if(controls[["model"]]=="HHMM"){ 
    T_star = compute_fs(fs_time_horizon = controls[["time_horizon"]][2], T = T)
    states = matrix(NA,T,max(T_star)+1) 
    logReturns = matrix(NA,T,max(T_star)+1)
    states[,1] = simulate_states(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    logReturns[,1] = simulate_observations(states[,1],thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],controls[["sdds"]][1])
    for(t in 1:T){
      S_t = states[t,1]
      states[t,-1] = c(simulate_states(Gamma2delta(thetaList[["Gammas_star"]][[S_t]]),thetaList[["Gammas_star"]][[S_t]],T_star[t]),rep(NA,max(T_star)-T_star[t]))
      logReturns[t,-1] = c(simulate_observations(states[t,-1][!is.na(states[t,-1])],thetaList[["mus_star"]][[S_t]],thetaList[["sigmas_star"]][[S_t]],thetaList[["dfs_star"]][[S_t]],controls[["sdds"]][2]),rep(NA,max(T_star)-T_star[t]))
    }
  }
  out = list(
    "logReturns"   = logReturns,
    "states0"      = states,
    "thetaUncon0"  = thetaUncon,
    "thetaCon0"    = thetaCon,
    "thetaList0"   = thetaList,
    "T_star"       = T_star
  )
  return(out)
}