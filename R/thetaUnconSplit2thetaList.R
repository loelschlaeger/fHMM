#' Bring uncontrained fine-scale model parameters in constrained list form
#'
#' @param thetaUncon Unconstrained fine-scale model parameters in vector form
#' @param controls A list of controls
#' 
#' @return Constrained fine-scale model parameters in list form

thetaUnconSplit2thetaList = function(thetaUncon,controls){
  N = controls[["states"]][2]
  gammasUncon = thetaUncon[1:((N-1)*N)]; thetaUncon = thetaUncon[-(1:((N-1)*N))]
  Gamma       = gammasUncon2Gamma(gammasUncon,N)
  musCon      = muUncon2muCon(thetaUncon[1:N],link=(controls[["sdds"]][2]=="gamma")); thetaUncon = thetaUncon[-(1:N)]
  sigmasCon   = sigmaUncon2sigmaCon(thetaUncon[1:N]); thetaUncon = thetaUncon[-(1:N)]
  if(controls[["sdds"]][2]=="t"){
    if(is.na(controls[["fixed_dfs"]][2])){
      dfs = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
    } else {
      dfs = rep(controls[["fixed_dfs"]][2],N)
    }
  }
  if(controls[["sdds"]][2]=="gamma"){
    dfs = NULL
  }
  thetaList = list(
    "Gamma"  = Gamma,
    "mus"    = musCon,
    "sigmas" = sigmasCon,
    "dfs"    = dfs
  ) 
  return(thetaList)
}