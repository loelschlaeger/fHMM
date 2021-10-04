#' @title Estimation check
#' @description Summarizes and saves estimates.
#' @param mods A list of fitted models in the different estimation runs.
#' @param lls A vector of log-likelihood values of accepted \code{mods}.
#' @param data A list of processed data information.
#' @param hessian Hessian matrix of the estimated model.
#' @param controls A list of controls.
#' @return A list of fitted model information.

fHMM_model = function(data, mods, lls, hessian){
  
  ### select run with highest log-likelihood
  mod        = mods[[which.max(lls)]]
  ll         = -mod[["minimum"]]
  thetaUncon = mod[["estimate"]]
  thetaCon   = thetaUncon2thetaCon(thetaUncon,controls)
  thetaList  = thetaCon2thetaList(thetaCon,controls)
  
  ### sort model parameters
  thetaListOrdered = thetaList2thetaListOrdered(thetaList,controls)
  thetaConOrdered = thetaList2thetaCon(thetaListOrdered,controls)
  thetaUnconOrdered = thetaCon2thetaUncon(thetaConOrdered,controls)
  
  ### check Hessian
  permut = diag(length(thetaUncon))[match(thetaCon,thetaConOrdered),]
  hessianOrdered = permut %*% hessian %*% t(permut)
  hessianOrdered[is.na(hessianOrdered)] = 0
  
  ### check if iteration limit was reached
  if(mod[["iterations"]] >= controls[["fit"]][["iterlim"]])
    warning("C.5",call.=FALSE)
  
  ### detect unidentified states
  check_unid_states = function(matrix_list){
    flag = FALSE
    for(matrix in matrix_list) 
      for(x in c(0,1)) 
        if(any(abs(suppressWarnings(Gamma2delta(matrix))-x)<1e-04)==TRUE) 
          flag = TRUE
    if(flag) 
      warning("C.6", call.=FALSE)
  }
  if(controls[["model"]]=="HMM"){
    check_unid_states(list(thetaList[["Gamma"]]))
  }
  if(controls[["model"]]=="HHMM"){
    check_unid_states(list(thetaList[["Gamma"]],thetaList[["Gammas_star"]][seq_len(controls[["states"]][1])]))
  }
  
  
  
  ### create object 'fit'
  fit = list("ll"         = ll,
             "mod"        = mod,
             "thetaUncon" = thetaUnconOrdered,
             "thetaCon"   = thetaConOrdered,
             "thetaList"  = thetaListOrdered,
             "hessian"    = hessianOrdered,
             "mods"       = mods,
             "lls"        = lls
             )

  ### return fit object
  return(fit)
}