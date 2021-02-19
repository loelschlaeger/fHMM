#' Order states in thetaList based on expected values
#' @param thetaList Constrained model parameters in list form
#' @param controls A list of controls
#' @return Constrained and ordered model parameters in list form
#' @details 
#' If t SDD, order states decreasing with respect to expected values. If Gamma SDD, vice versa.
thetaList2thetaListOrdered = function(thetaList,controls){
  M = controls[["states"]][1] 
  N = controls[["states"]][2] 
  mu_order = order(thetaList[["mus"]],decreasing=(controls[["sdds"]][1]=="t"))
  permut = diag(M)[mu_order,]
  thetaList[["Gamma"]] = permut %*% thetaList[["Gamma"]] %*% t(permut)
  thetaList[["mus"]] = as.vector(permut %*% thetaList[["mus"]])
  thetaList[["sigmas"]] = as.vector(permut %*% thetaList[["sigmas"]])
  if(controls[["sdds"]][1]=="t"){
    thetaList[["dfs"]] = as.vector(permut %*% thetaList[["dfs"]]); thetaList[["dfs"]][which(is.nan(thetaList[["dfs"]]))] = Inf
  }
  if(controls$model=="HHMM"){
    thetaList[["Gammas_star"]] = thetaList[["Gammas_star"]][mu_order]
    thetaList[["mus_star"]] = thetaList[["mus_star"]][mu_order]
    thetaList[["sigmas_star"]] = thetaList[["sigmas_star"]][mu_order]
    if(controls[["sdds"]][2]=="t"){
      thetaList[["dfs_star"]] = thetaList[["dfs_star"]][mu_order]
    }
    for(m in seq_len(M)){
      permut = diag(N)[order(thetaList[["mus_star"]][[m]],decreasing=TRUE),]
      thetaList[["Gammas_star"]][[m]] = permut %*% thetaList[["Gammas_star"]][[m]] %*% t(permut)
      thetaList[["mus_star"]][[m]] = as.vector(permut %*% thetaList[["mus_star"]][[m]])
      thetaList[["sigmas_star"]][[m]] = as.vector(permut %*% thetaList[["sigmas_star"]][[m]])
      if(controls[["sdds"]][2]=="t"){
        thetaList[["dfs_star"]][[m]] = as.vector(permut %*% thetaList[["dfs_star"]][[m]]); thetaList[["dfs_star"]][[m]][which(is.nan(thetaList[["dfs_star"]][[m]]))] = Inf
      }
    }
  }
  return(thetaList)
}