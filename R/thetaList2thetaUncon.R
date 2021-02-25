#' Bring constrained model parameters in list form to unconstrained parameters in vector form
#' @param thetaList Constrained model parameters in list form
#' @param controls A list of controls
#' @return Unconstrained model parameters in vector form
thetaList2thetaUncon = function(thetaList,controls){
  if(controls[["model"]]=="HMM"){
    gammasUncon = Gamma2gammasUncon(thetaList[["Gamma"]])
    musUncon    = muCon2muUncon(thetaList[["mus"]],link=(controls[["sdds"]][1] == "gamma"))
    sigmasUncon = sigmaCon2sigmaUncon(thetaList[["sigmas"]])
    if(controls[["sdds"]][1]=="t" && is.na(controls[["fixed_dfs"]][1])){
      dfsUncon = dfCon2dfUncon(thetaList[["dfs"]])
      thetaUncon = c(gammasUncon,musUncon,sigmasUncon,dfsUncon)
    } else {
      thetaUncon = c(gammasUncon,musUncon,sigmasUncon)
    }
  }
  if(controls[["model"]]=="HHMM"){
    gammasUncon      = Gamma2gammasUncon(thetaList[["Gamma"]])
    gammasUncon_star = vector()
    for(s in seq_len(controls[["states"]][1])){
      gammasUncon_star = c(gammasUncon_star,Gamma2gammasUncon(thetaList[["Gammas_star"]][[s]]))
    }
    musUncon      = muCon2muUncon(thetaList[["mus"]],link=(controls[["sdds"]][1] == "gamma"))
    musUncon_star = vector()
    for(s in seq_len(controls[["states"]][1] )){
      musUncon_star = c(musUncon_star,muCon2muUncon(thetaList[["mus_star"]][[s]],link=(controls[["sdds"]][2] == "gamma")))
    }
    sigmasUncon      = sigmaCon2sigmaUncon(thetaList[["sigmas"]])
    sigmasUncon_star = vector()
    for(s in seq_len(controls[["states"]][1] )){
      sigmasUncon_star = c(sigmasUncon_star,sigmaCon2sigmaUncon(thetaList[["sigmas_star"]][[s]]))
    }
    if(controls[["sdds"]][1]=="t" & is.na(controls[["fixed_dfs"]][1])){
      dfsUncon = dfCon2dfUncon(thetaList[["dfs"]])
      if(controls[["sdds"]][2]=="t" & is.na(controls[["fixed_dfs"]][2])){
        dfsUncon_star = vector()
        for(s in seq_len(controls[["states"]][1] )){
          dfsUncon_star = c(dfsUncon_star,dfCon2dfUncon(thetaList[["dfs_star"]][[s]]))
        }
        thetaUncon = c(gammasUncon,gammasUncon_star,musUncon,musUncon_star,sigmasUncon,sigmasUncon_star,dfsUncon,dfsUncon_star)
      } else {
        thetaUncon = c(gammasUncon,gammasUncon_star,musUncon,musUncon_star,sigmasUncon,sigmasUncon_star,dfsUncon)
      }
    } else {
      if(controls[["sdds"]][2]=="t" & is.na(controls[["fixed_dfs"]][2])){
        dfsUncon_star = vector()
        for(s in seq_len(controls[["states"]][1] )){
          dfsUncon_star = c(dfsUncon_star,dfCon2dfUncon(thetaList[["dfs_star"]][[s]]))
        }
        thetaUncon = c(gammasUncon,gammasUncon_star,musUncon,musUncon_star,sigmasUncon,sigmasUncon_star,dfsUncon_star)
      } else {
        thetaUncon = c(gammasUncon,gammasUncon_star,musUncon,musUncon_star,sigmasUncon,sigmasUncon_star)
      }
    }
  }
  return(thetaUncon)
}
