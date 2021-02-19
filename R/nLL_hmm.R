#' Compute the negative log-likelihood of a hidden Markov model
#' @param thetaUncon Model parameters in format \code{thetaUncon}
#' @param observations A vector of observations
#' @param controls A list of controls
#' @return Negative log-likelihood
nLL_hmm = function(thetaUncon,observations,controls){
  T = length(observations)
  if(controls[["model"]]=="HMM"){
    nstates   = controls[["states"]][1]
    thetaList = thetaUncon2thetaList(thetaUncon,controls)
    sdd       = controls[["sdds"]][1]
  }
  if(controls[["model"]]=="HHMM"){
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