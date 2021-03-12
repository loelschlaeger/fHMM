#' @title Parameter names
#' @description Creates model parameter names.
#' @param controls A list of controls.
#' @param all A boolean, determining whether all (\code{all=TRUE}) or only estimated (\code{all=FALSe}) names should be produced.
#' @return Vector of model parameter names.

parameter_names = function(controls,all){
  matrix_indices = function(dim,all){
    indices = vector()
    for(i in seq_len(dim)) for(j in seq_len(dim)){
      if(i!=j) indices = c(indices,paste0(j,i))
      if(i==j & all) indices = c(indices,paste0(j,i))
    }
    return(indices)
  }
  if(controls[["model"]]=="HMM"){
    Gamma_names = paste0("Gamma_",matrix_indices(controls[["states"]][1],all))
    mu_names = paste0("mu_",seq_len(controls[["states"]][1]))
    sigma_names = paste0("sigma_",seq_len(controls[["states"]][1]))
    if(controls[["sdds"]][1]=="t" & (all || is.na(controls[["fixed_dfs"]][1]))){
      df_names = paste0("df_",seq_len(controls[["states"]][1]))
      names = c(Gamma_names,mu_names,sigma_names,df_names)
    } else {
      names = c(Gamma_names,mu_names,sigma_names)
    }
  }
  if(controls[["model"]]=="HHMM"){
    Gamma_names = paste0("Gamma_",matrix_indices(controls[["states"]][1],all))
    Gamma_star_names = vector()
    for(s in seq_len(controls[["states"]][1])){
      Gamma_star_names = c(Gamma_star_names,paste0("Gamma_star",s,"_",matrix_indices(controls[["states"]][2],all)))  
    }
    mu_names = paste0("mu_",seq_len(controls[["states"]][1]))
    mu_star_names = vector()
    for(s in seq_len(controls[["states"]][1])){
      mu_star_names = c(mu_star_names,paste0("mu_star",s,"_",seq_len(controls[["states"]][2])))  
    }
    sigma_names = paste0("sigma_",seq_len(controls[["states"]][1]))
    sigma_star_names = vector()
    for(s in seq_len(controls[["states"]][1])){
      sigma_star_names = c(sigma_star_names,paste0("sigma_star",s,"_",seq_len(controls[["states"]][2])))    
    }
    if(controls[["sdds"]][1]=="t" & (all || is.na(controls[["fixed_dfs"]][1]))){
      df_names = paste0("df_",seq_len(controls[["states"]][1]))
      if(controls[["sdds"]][2]=="t" & (all || is.na(controls[["fixed_dfs"]][2]))){
        df_star_names = vector()
        for(s in seq_len(controls[["states"]][1])){
          df_star_names = c(df_star_names,paste0("df_star",s,"_",seq_len(controls[["states"]][2])))
        }
        names = c(Gamma_names,Gamma_star_names,mu_names,mu_star_names,sigma_names,sigma_star_names,df_names,df_star_names)
      } else {
        names = c(Gamma_names,Gamma_star_names,mu_names,mu_star_names,sigma_names,sigma_star_names,df_names)
      }
    } else {
      if(controls[["sdds"]][2]=="t" & (all || is.na(controls[["fixed_dfs"]][2]))){
        df_star_names = vector()
        for(s in seq_len(controls[["states"]][1])){
          df_star_names = c(df_star_names,paste0("df_star",s,"_",seq_len(controls[["states"]][2])))
        }
        names = c(Gamma_names,Gamma_star_names,mu_names,mu_star_names,sigma_names,sigma_star_names,df_star_names)
      } else {
        names = c(Gamma_names,Gamma_star_names,mu_names,mu_star_names,sigma_names,sigma_star_names)
      }
    }
  }
  return(names)
}