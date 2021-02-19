#' Check and save estimates
#' @param mods A list of fitted models in the different estimation runs
#' @param llks A vector of log-likelihood values of accepted \code{mods}
#' @param data A list of processed data information
#' @param hessian The Hessian matrix
#' @param controls A list of controls
#' @return A fitted model
check_estimation = function(mods,llks,data,hessian,controls){
  ### select run with highest log-likelihood
  mod        = mods[[which.max(llks)]]
  mod_LL     = -mod[["minimum"]]
  thetaUncon = mod[["estimate"]]
  thetaCon   = thetaUncon2thetaCon(thetaUncon,controls)
  thetaList  = thetaCon2thetaList(thetaCon,controls)
  ### check if iteration limit was reached
  if(mod[["iterations"]] >= controls[["iterlim"]]){
    warning(sprintf("%s (%s)",exception("C.5")[2],exception("C.5")[1]),call.=FALSE)
  }
  ### detect unidentified states
  check_unid_states = function(matrix_list){
    flag = FALSE
    for(matrix in matrix_list) for(x in c(0,1)) if(any(abs(suppressWarnings(Gamma2delta(matrix))-x)<1e-04)==TRUE) flag = TRUE
    if(flag) warning(sprintf("%s (%s)",exception("C.6")[2],exception("C.6")[1]),call.=FALSE)
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
  fit = list("logLikelihood"      = mod_LL,
             "model"              = mod,
             "thetaUncon"         = thetaUncon,
             "thetaCon"           = thetaCon,
             "thetaList"          = thetaList,
             "AIC"                = comp_AIC(-mod[["minimum"]]),
             "BIC"                = comp_BIC(prod(dim(t(data[["logReturns"]]))),-mod[["minimum"]]),
             "hessian"            = hessian,
             "all_models"          = mods,
             "all_logLikelihoods" = llks
             )
  ### order estimates
  thetaListOrdered = thetaList2thetaListOrdered(thetaList,controls)
  thetaConOrdered = thetaList2thetaCon(thetaListOrdered,controls)
  shift = match(thetaConOrdered,thetaCon)
  ### compute confidence intervals
  ci = compute_ci(fit,controls)
  lb = ci[[1]][shift]
  est = ci[["estimate"]][shift]
  ub = ci[[3]][shift]
  ### true estimates and relative bias
  if(controls[["sim"]]){
    true = data[["thetaCon0"]]
    rbias = (est-true)/true
  }
  ### create estimation information file
  if(check_saving(name = "estimates", filetype = "txt", controls = controls)){  
      sink(file = paste0(controls[["path"]],"/models/",controls[["id"]],"/estimates.txt"))
        writeLines(paste0("Estimation results of model '",controls[["id"]],"':\n"))
        writeLines(sprintf("%-15s %.2f","log-likelihood:",fit[["logLikelihood"]]))
        writeLines(sprintf("%-15s %.2f","AIC:",fit[["AIC"]]))
        writeLines(sprintf("%-15s %.2f","BIC:",fit[["BIC"]]))
        writeLines(sprintf("%-15s %.0f","exit code:",mod[["code"]]))
        writeLines(sprintf("%-15s %.0f","iterations:",mod[["iterations"]])); cat("\n")
        if(controls[["sim"]]){
          table = cbind(sprintf("%.2f",true),
                        sprintf("%.2f",est),
                        sprintf("%.2f",rbias),
                        suppressWarnings(sprintf("%.2f",lb)),
                        suppressWarnings(sprintf("%.2f",ub)))
          colnames(table) = c("true","est","rel. bias",names(ci)[1],names(ci)[3])
        } else {
          table = cbind(sprintf("%.2f",est),
                        suppressWarnings(sprintf("%.2f",lb)),
                        suppressWarnings(sprintf("%.2f",ub)))
          colnames(table) = c("est",names(ci)[1],names(ci)[3])
        }
        rownames(table) = parameter_names(controls,all=FALSE)
        print(table,quote=FALSE)
      sink()
  }
  ### save results
  check_saving(object = fit, filetype = "rds", controls = controls)
  return(fit)
}
