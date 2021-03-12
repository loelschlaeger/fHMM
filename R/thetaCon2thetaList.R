#' Brings contrained model parameters from vector form \code{thetaCon} to list form \code{ThetaList}.
#' @param thetaCon Constrained model parameters in vector form.
#' @param controls A list of controls.
#' @return Constrained model parameters in list form.

thetaCon2thetaList = function(thetaCon,controls){
  M = controls[["states"]][1] 
  N = controls[["states"]][2]
  
  gammasCon = thetaCon[1:((M-1)*M)]; thetaCon = thetaCon[-(1:((M-1)*M))]
  Gamma = gammasCon2Gamma(gammasCon,M)
  if(controls[["model"]]=="HHMM"){
    Gammas_star = list()
    for(m in seq_len(M)){
      gammasCon_star   = thetaCon[1:((N-1)*N)]; thetaCon = thetaCon[-(1:((N-1)*N))]
      Gammas_star[[m]] = gammasCon2Gamma(gammasCon_star,N)
    }
  }
  
  musCon = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
  if(controls[["model"]]=="HHMM"){ 
    musCon_star = list()
    for(m in seq_len(M)){
      musCon_star[[m]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
    }
  }
  
  sigmasCon = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
  if(controls[["model"]]=="HHMM"){
    sigmasCon_star = list()
    for(m in seq_len(M)){
      sigmasCon_star[[m]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
    }
  }
  
  if(controls[["sdds"]][1] == "t"){
    if(is.na(controls[["fixed_dfs"]][1])){
      dfsCon = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
    } else {
      dfsCon = rep(controls[["fixed_dfs"]][1],M)
    }
  }
  if(controls[["sdds"]][1] == "gamma"){
    dfsCon = NULL
  }
  
  if(controls[["model"]]=="HHMM"){ 
    if(controls[["sdds"]][2] == "t"){
      dfsCon_star = list()
      if(is.na(controls[["fixed_dfs"]][2])){
        for(m in seq_len(M)){
          dfsCon_star[[m]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
        }
      } else {
        for(m in seq_len(M)){
          dfsCon_star[[m]] = rep(controls[["fixed_dfs"]][2],N)
        }
      }
    }
    if(controls[["sdds"]][2] == "gamma"){
      dfsCon_star = NULL
    }
  }
  
  if(controls[["model"]]=="HMM"){
    thetaList = list(
      "Gamma"       = Gamma,
      "mus"         = musCon,
      "sigmas"      = sigmasCon,
      "dfs"         = dfsCon
    ) 
  }
  if(controls[["model"]]=="HHMM"){
    thetaList = list(
      "Gamma"       = Gamma,
      "mus"         = musCon,
      "sigmas"      = sigmasCon,
      "dfs"         = dfsCon,
      "Gammas_star" = Gammas_star,
      "mus_star"    = musCon_star,
      "sigmas_star" = sigmasCon_star,
      "dfs_star"    = dfsCon_star
    ) 
  }
  return(thetaList)
}