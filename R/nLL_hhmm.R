#' Negative log-likelihood function of an HHMM
#' @description 
#' This function computes the negative log-likelihood of an HHMM.
#' @param parUncon 
#' An object of class \code{parUncon}.
#' @param observations
#' The matrix of the simulated or empirical data used for estimation.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @return 
#' The negative log-likelihood value.
#' @keywords 
#' internal

nLL_hhmm = function(parUncon, data){
  M  = controls[["states"]][1] 
  N  = controls[["states"]][2]
  observations_cs = observations[,1]
  observations_fs = observations[,-1]
  T = length(observations_cs)
  thetaList = thetaUncon2thetaList(parUncon,controls)
  Gamma  = thetaList[["Gamma"]]
  delta  = Gamma2delta(Gamma)
  mus    = thetaList[["mus"]]
  sigmas = thetaList[["sigmas"]]
  dfs    = thetaList[["dfs"]]
  allprobs = matrix(0,M,T)
  log_likelihoods = matrix(0,M,T)
  thetaUnconSplit = thetaUncon2thetaUnconSplit(parUncon,controls)
  for (m in seq_len(M)){
    if(controls[["sdds"]][1]=="t")
      allprobs[m,] = 1/sigmas[m]*dt((observations_cs-mus[m])/sigmas[m],dfs[m])
    if(controls[["sdds"]][1]=="gamma")
      allprobs[m,] = dgamma(observations_cs,shape=mus[m]^2/sigmas[m]^2,scale=sigmas[m]^2/mus[m])
    for(t in seq_len(T))
      log_likelihoods[m,t] = -nLL_hmm(thetaUnconSplit[[m]],observations_fs[t,][!is.na(observations_fs[t,])],controls)
  }
  nLL = -LL_HHMM_Rcpp(log_likelihoods=log_likelihoods,allprobs=allprobs,Gamma=Gamma,delta=delta,M=M,T=T) 
  return(nLL)
}