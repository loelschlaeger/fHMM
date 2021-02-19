#' Constrain model parameters
#' @param thetaUncon Unconstrained model parameters in vector form
#' @param controls A list of controls
#' @return Constrained model parameters in vector form
thetaUncon2thetaCon = function(thetaUncon,controls){
  M  = controls[["states"]][1] 
  N  = controls[["states"]][2]
  if(controls[["model"]]=="HMM"){
    gammasUncon = thetaUncon[1:((M-1)*M)]; thetaUncon = thetaUncon[-(1:((M-1)*M))]
    gammasCon   = gammasUncon2gammasCon(gammasUncon,M)
    musUncon    = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
    musCon      = muUncon2muCon(musUncon,link=(controls[["sdds"]][1] == "gamma"))
    sigmasUncon = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
    sigmasCon   = sigmaUncon2sigmaCon(sigmasUncon)
    if(controls[["sdds"]][1] == "t"){
      if(is.na(controls[["fixed_dfs"]][1])){
        dfsUncon = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
        dfsCon   = dfUncon2dfCon(dfsUncon)
      } else {
        dfsCon = integer(0)
      }
      thetaCon = c(gammasCon,musCon,sigmasCon,dfsCon)
    }
    if(controls[["sdds"]][1] == "gamma"){
      thetaCon = c(gammasCon,musCon,sigmasCon)
    }
  }
  if(controls[["model"]]=="HHMM"){
    gammasUncon = thetaUncon[1:((M-1)*M)]; thetaUncon = thetaUncon[-(1:((M-1)*M))]
    gammasCon   = gammasUncon2gammasCon(gammasUncon,M)
    for(m in seq_len(M)){
      gammasUncon_star = thetaUncon[1:((N-1)*N)]; thetaUncon = thetaUncon[-(1:((N-1)*N))]
      gammasCon_star   = gammasUncon2gammasCon(gammasUncon_star,N)
      gammasCon        = c(gammasCon,gammasCon_star)
    }
    musUncon = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
    musCon   = muUncon2muCon(musUncon,link=(controls[["sdds"]][1] == "gamma"))
    for(m in seq_len(M)){
      musUncon_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
      musCon_star   = muUncon2muCon(musUncon_star,link=(controls[["sdds"]][2] == "gamma"))
      musCon        = c(musCon,musCon_star)
    }
    sigmasUncon = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
    sigmasCon   = sigmaUncon2sigmaCon(sigmasUncon)
    for(m in seq_len(M)){
      sigmasUncon_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
      sigmasCon_star   = sigmaUncon2sigmaCon(sigmasUncon_star)
      sigmasCon        = c(sigmasCon,sigmasCon_star)
    }
    if(controls[["sdds"]][1] == "t"){
      if(is.na(controls[["fixed_dfs"]][1])){
        dfsUncon = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
        dfsCon   = dfUncon2dfCon(dfsUncon)
      } else {
        dfsCon = integer(0)
      }
      if(controls[["sdds"]][2] == "t"){
        if(is.na(controls[["fixed_dfs"]][2])){
          for(m in seq_len(M)){
            dfsUncon_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
            dfsCon_star   = dfUncon2dfCon(dfsUncon_star)
            dfsCon        = c(dfsCon,dfsCon_star)
          }
        }
        thetaCon = c(gammasCon,musCon,sigmasCon,dfsCon)
      }
      if(controls[["sdds"]][2] == "gamma"){
        thetaCon = c(gammasCon,musCon,sigmasCon,dfsCon) 
      }
    }
    if(controls[["sdds"]][1] == "gamma"){
      dfsCon = integer(0)
      if(controls[["sdds"]][2] == "t"){
        if(is.na(controls[["fixed_dfs"]][2])){
          for(m in seq_len(M)){
            dfsUncon_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
            dfsCon_star   = dfUncon2dfCon(dfsUncon_star)
            dfsCon        = c(dfsCon,dfsCon_star)
          }
        }
        thetaCon = c(gammasCon,musCon,sigmasCon,dfsCon)
      }
      if(controls[["sdds"]][2] == "gamma"){
        thetaCon = c(gammasCon,musCon,sigmasCon) 
      }
    }
  }
  return(thetaCon)
}