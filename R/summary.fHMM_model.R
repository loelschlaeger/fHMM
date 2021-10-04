summary.fHMM_model = function() {
  ### compute model selection criteria
  no_par   = length(mod[["estimate"]])
  comp_AIC = function(LL) return(2*no_par-2*LL)
  comp_BIC = function(T,LL) return(log(T)*no_par-2*LL)
  
  ### compute confidence intervals
  ci = compute_ci(fit,controls)
  lb = ci[[1]]
  est = ci[["estimate"]]
  ub = ci[[3]]
  
  ### true estimates and relative bias
  if(controls[["sim"]]){
    true = data[["thetaCon0"]]
    rbias = (est-true)/true
  }
  
  if(controls[["sim"]]){
    table = cbind(sprintf("%.4g",true),
                  sprintf("%.4g",est),
                  sprintf("%.4g",rbias),
                  suppressWarnings(sprintf("%.4g",lb)),
                  suppressWarnings(sprintf("%.4g",ub)))
    colnames(table) = c("true","est","rel. bias",names(ci)[1],names(ci)[3])
    
    ### remove
    estimates_table = cbind(true,est,rbias,lb,ub)
    rownames(estimates_table) = parameter_names(controls,all=FALSE)
    check_saving(object = estimates_table, filetype = "rds", controls = controls)
    
  } else {
    table = cbind(sprintf("%.4g",est),
                  suppressWarnings(sprintf("%.4g",lb)),
                  suppressWarnings(sprintf("%.4g",ub)))
    colnames(table) = c("est",names(ci)[1],names(ci)[3])
  }
  rownames(table) = parameter_names(controls,all=FALSE)
  print(table,quote=FALSE)
}