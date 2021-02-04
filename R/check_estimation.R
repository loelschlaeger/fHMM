#' Check and save estimates
#'
#' @param mods A list of fitted models in the different estimation runs
#' @param llks A vector of log-likelihood values of accepted \code{mods}
#' @param data A list of processed data information
#' @param hessian The Hessian matrix
#' @param controls A list of controls
#' 
#' @return A fitted model

check_estimation = function(mods,llks,data,hessian,controls){
  
  ### select run with highest log-likelihood
  mod       = mods[[which.max(llks)]]
  mod_LL    = -mod[["minimum"]]
  thetaCon  = thetaUncon2thetaCon(mod[["estimate"]],controls)
  thetaList = states_decreasing(thetaCon2thetaList(thetaCon,controls),controls)
  
  ### check if iteration limit was reached
  if(mod[["iterations"]] >= controls[["iterlim"]]) warning(sprintf("%s (%s)",exception("C.6")[2],exception("C.6")[1]),call.=FALSE)
  
  ### detect unidentified states
  check_unid_states = function(matrix_list){
    flag = FALSE
    for(matrix in matrix_list) for(x in c(0,1)) if(any(abs(suppressWarnings(Gamma2delta(matrix))-x)<1e-04)==TRUE) flag = TRUE
    if(flag) warning(sprintf("%s (%s)",exception("C.7")[2],exception("C.7")[1]),call.=FALSE)
  }
  if(controls[["model"]]=="HMM") check_unid_states(list(thetaList[["Gamma"]]))
  if(controls[["model"]]=="HHMM") check_unid_states(list(thetaList[["Gamma"]],thetaList[["Gammas_star"]][seq_len(controls[["states"]][1])]))
  
  ### create visualization of log-likelihoods
  plot_ll(llks,controls)
  
  ### compute model selection criteria
  no_par   = length(mod[["estimate"]])
  comp_AIC = function(LL) return(2*no_par-2*LL)
  comp_BIC = function(T,LL) return(log(T)*no_par-2*LL)
  
  ### create object 'fit'
  fit = list("LL"        = mod_LL,
             "mod"       = mod,
             "thetaList" = thetaList,
             "AIC"       = comp_AIC(-mod[["minimum"]]),
             "BIC"       = comp_BIC(prod(dim(t(data[["logReturns"]]))),-mod[["minimum"]]),
             "LLs"       = llks,
             "mods"      = mods,
             "hessian"   = hessian
             )
  
  ### create estimation information file
  if(check_saving(name = "estimates", filetype = "txt", controls = controls)){  
    options(max.print=1000000)
      sink(file = paste0("models/",controls[["id"]],"/estimates.txt"))
        first_col = c("log-likelihood","AIC","BIC","exit code","iterations")
        second_col = c(fit[["LL"]],fit[["AIC"]],fit[["BIC"]],mod[["code"]],mod[["iterations"]])
        df = data.frame(first_col,second_col); names(df) = NULL
        writeLines(paste0("Estimation results of model '",controls[["id"]],"':")); print(df,row.names=FALSE,right=FALSE); writeLines("")
        est = unlist(fit$thetaList,use.names=FALSE)
        if(controls[["sim"]]){
          true = unlist(data$thetaList0,use.names=FALSE)
          rbias = (est-true)/true 
          table = cbind(as.numeric(sprintf("%.4f",true)),
                        as.numeric(sprintf("%.4f",est)),
                        as.numeric(sprintf("%.2f",rbias)),
                        NA,
                        NA)
          colnames(table) = c("true","est","rel. bias","lb 95% CI","ub 95% CI")
        } else {
          table = cbind(as.numeric(sprintf("%.4f",est)),
                        NA,
                        NA)
          colnames(table) = c("est","lb 95% CI","ub 95% CI")
        }
        rownames(table) = names(unlist(data$thetaList))
        print(table)
      sink()
    options(max.print=1000)
  }
  
  ### save results
  check_saving(object = fit, filetype = "rds", controls = controls)
  
  return(fit)
}