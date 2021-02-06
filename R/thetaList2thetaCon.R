#' Bring contrained model parameters in vector form
#'
#' @param thetaList Constrained model parameters in list form
#' @param controls A list of controls
#' 
#' @return Constrained model parameters in vector form

thetaList2thetaCon = function(thetaList,controls){
  
  if(controls[["model"]]=="HMM"){
    gammasCon = Gamma2gammasCon(thetaList[["Gamma"]])
    musCon    = thetaList[["mus"]]
    sigmasCon = thetaList[["sigmas"]]
    if(controls[["sdds"]][1]=="t"){
      if(is.na(controls[["fixed_dfs"]][1])){
        dfsCon = thetaList[["dfs"]]
        thetaCon = c(gammasCon,musCon,sigmasCon,dfsCon)
      } else {
        thetaCon = c(gammasCon,musCon,sigmasCon)
      }
    }
  }
  if(controls[["model"]]=="HHMM"){
    gammasCon      = Gamma2gammasCon(thetaList[["Gamma"]])
    gammasCon_star = vector()
    for(s in seq_len(controls[["states"]][1] )){
      gammasCon_star = c(gammasCon_star,Gamma2gammasCon(thetaList[["Gammas_star"]][[s]]))
    }
    musCon      = thetaList[["mus"]]
    musCon_star = vector()
    for(s in seq_len(controls[["states"]][1] )){
      musCon_star = c(musCon_star,thetaList[["mus_star"]][[s]])
    }
    sigmasCon      = thetaList[["sigmas"]]
    sigmasCon_star = vector()
    for(s in seq_len(controls[["states"]][1] )){
      sigmasCon_star = c(sigmasCon_star,thetaList[["sigmas_star"]][[s]])
    }
    if(controls[["sdds"]][1]=="t" & is.na(controls[["fixed_dfs"]][1])){
      dfsCon = thetaList[["dfs"]]
      if(controls[["sdds"]][2]=="t" & is.na(controls[["fixed_dfs"]][2])){
        dfsCon_star = vector()
        for(s in seq_len(controls[["states"]][1] )){
          dfsCon_star = c(dfsCon_star,thetaList[["dfs_star"]][[s]])
        }
        thetaCon = c(gammasCon,gammasCon_star,musCon,musCon_star,sigmasCon,sigmasCon_star,dfsCon,dfsCon_star)
      } else {
        thetaCon = c(gammasCon,gammasCon_star,musCon,musCon_star,sigmasCon,sigmasCon_star,dfsCon)
      }
    } else {
      if(controls[["sdds"]][2]=="t" & is.na(controls[["fixed_dfs"]][2])){
        dfsCon_star = vector()
        for(s in seq_len(controls[["states"]][1] )){
          dfsCon_star = c(dfsCon_star,thetaList[["dfs_star"]][[s]])
        }
        thetaCon = c(gammasCon,gammasCon_star,musCon,musCon_star,sigmasCon,sigmasCon_star,dfsCon_star)
      } else {
        thetaCon = c(gammasCon,gammasCon_star,musCon,musCon_star,sigmasCon,sigmasCon_star)
      }
    }
        
  }
  
  return(thetaCon)
}
