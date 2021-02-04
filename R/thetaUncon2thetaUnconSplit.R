#' Bring uncontrained model parameters in list form for each fine-scale model
#'
#' @param thetaUncon Unconstrained model parameters in vector form
#' @param controls A list of controls
#' 
#' @return Unconstrained fine-scale model parameters in list form

thetaUncon2thetaUnconSplit = function(thetaUncon,controls){
  M = controls[["states"]][1] 
  N = controls[["states"]][2] 
  
  thetaUnconSplit = list()
  thetaUncon = thetaUncon[-(1:((M-1)*M))] 
  for(m in seq_len(M)){
    thetaUnconSplit[[m]] = thetaUncon[1:((N-1)*N)]
    thetaUncon = thetaUncon[-(1:((N-1)*N))] 
  }
  thetaUncon = thetaUncon[-(1:M)]
  for(m in seq_len(M)){
    thetaUnconSplit[[m]] = c(thetaUnconSplit[[m]],thetaUncon[1:N])
    thetaUncon = thetaUncon[-(1:N)] 
  }
  thetaUncon = thetaUncon[-(1:M)] 
  for(m in seq_len(M)){
    thetaUnconSplit[[m]] = c(thetaUnconSplit[[m]],thetaUncon[1:N])
    thetaUncon = thetaUncon[-(1:N)] 
  }
  if(controls[["sdds"]][1]=="t"){
    if(is.na(controls[["fixed_dfs"]][1])){
      thetaUncon = thetaUncon[-(1:M)]
    }
  }
  if(controls[["sdds"]][2]=="t"){
    if(is.na(controls[["fixed_dfs"]][2])){
      for(m in seq_len(M)){
        thetaUnconSplit[[m]] = c(thetaUnconSplit[[m]],thetaUncon[1:N])
        thetaUncon = thetaUncon[-(1:N)] 
      }
    }
  }
  
  return(thetaUnconSplit)
}