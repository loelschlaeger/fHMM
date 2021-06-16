#' @title Negative log-likelihood of a HMM
#' @description Computes the negative log-likelihood of a hidden Markov model.
#' @param thetaUncon Vector of model parameters in format \code{thetaUncon}.
#' @param observations A vector of observations.
#' @param controls A list of controls.
#' @return Numeric, negative log-likelihood value.
#' @keywords internal

nLL_hmm = function(thetaUncon,observations,controls){
  T = length(observations)
  if(controls[["model"]]=="hmm"){
    nstates   = controls[["states"]][1]
    thetaList = thetaUncon2thetaList(thetaUncon,controls)
    sdd       = controls[["sdds"]][1]
  }
  if(controls[["model"]]=="hhmm"){
    nstates   = controls[["states"]][2]
    thetaList = thetaUnconSplit2thetaList(thetaUncon,controls)
    sdd       = controls[["sdds"]][2]
  }
  Gamma  = thetaList[["Gamma"]]
  delta  = Gamma2delta(Gamma)
  mus    = thetaList[["mus"]]
  sigmas = thetaList[["sigmas"]]
  dfs    = thetaList[["dfs"]]
  allprobs = matrix(NA,nstates,T)
  for (i in 1:nstates){
    if(sdd=="t")     allprobs[i,] = 1/sigmas[i]*dt(x = (observations-mus[i])/sigmas[i], df = dfs[i])
    if(sdd=="gamma") allprobs[i,] = dgamma(observations,shape=mus[i]^2/sigmas[i]^2,scale=sigmas[i]^2/mus[i])
  }
  nLL = -LL_HMM_Rcpp(allprobs,Gamma,delta,nstates,T) 
  return(nLL)
}

#' @title Negative log-likelihood of a HHMM
#' @description Computes the negative log-likelihood of a hierarchical hidden Markov model.
#' @param thetaUncon Vector of model parameters in format \code{thetaUncon}.
#' @param observations A matrix of observations, coarse-scale observations in the first column and fine-scale observations in the corresponding row.
#' @param controls A list of controls.
#' @return Numeric, negative log-likelihood value.
#' @keywords internal

nLL_hhmm = function(thetaUncon,observations,controls){
  M  = controls[["states"]][1] 
  N  = controls[["states"]][2]
  observations_cs = observations[,1]
  observations_fs = observations[,-1]
  T = length(observations_cs)
  thetaList = thetaUncon2thetaList(thetaUncon,controls)
  Gamma  = thetaList[["Gamma"]]
  delta  = Gamma2delta(Gamma)
  mus    = thetaList[["mus"]]
  sigmas = thetaList[["sigmas"]]
  dfs    = thetaList[["dfs"]]
  allprobs = matrix(0,M,T)
  log_likelihoods = matrix(0,M,T)
  thetaUnconSplit = thetaUncon2thetaUnconSplit(thetaUncon,controls)
  for (m in seq_len(M)){
    if(controls[["sdds"]][1]=="t"){
      allprobs[m,] = 1/sigmas[m]*dt((observations_cs-mus[m])/sigmas[m],dfs[m])
    }
    if(controls[["sdds"]][1]=="gamma"){
      allprobs[m,] = dgamma(observations_cs,shape=mus[m]^2/sigmas[m]^2,scale=sigmas[m]^2/mus[m])
    }
    for(t in seq_len(T)){
      log_likelihoods[m,t] = -nLL_hmm(thetaUnconSplit[[m]],observations_fs[t,][!is.na(observations_fs[t,])],controls)
    }
  }
  nLL = -LL_HHMM_Rcpp(log_likelihoods=log_likelihoods,allprobs=allprobs,Gamma=Gamma,delta=delta,M=M,T=T) 
  return(nLL)
}