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
  
  
  
  ### frequency of decoded states
  writeLines("Frequency of decoded states\n")
  if(controls[["model"]]=="hmm"){ 
    out = table((decoding))
    names(out) = paste("state",names(out))
    print(out)
    cat("\n")
  }
  if(controls[["model"]]=="hhmm"){
    out_cs = table(factor(decoding[,1],levels = seq_len(controls[["states"]][1])))
    names(out_cs) = paste("CS state",names(out_cs))
    print(out_cs)
    cat("\n")
    for(state in seq_len(controls[["states"]][1])){
      writeLines(paste0("Conditional on CS state ",state,":"))
      out_fs = table(factor(decoding[decoding[,1]==state,-1],levels = seq_len(controls[["states"]][2])))
      names(out_fs) = paste("FS state",names(out_fs))
      print(out_fs)
      cat("\n")
    }
  }
  
  ### comparison between true states and predicted states
  if(controls[["sim"]]){
    compare_true_predicted_states = function(no_states,decoded_states,true_states,label=NULL){
      c_table = matrix(0,no_states,no_states)
      rownames(c_table) = paste0("true ",label,"state ",seq_len(no_states))
      colnames(c_table) = paste0("decoded ",label,"state ",seq_len(no_states))
      for(i in seq_len(no_states)) for(j in seq_len(no_states)){
        value = sum(decoded_states==i & true_states==j, na.rm = TRUE) / sum(decoded_states==i, na.rm = TRUE) * 100
        c_table[i,j] = if(is.nan(value)) "NA" else sprintf("%.0f%%",value)
      }
      print(c_table,quote=FALSE,right=TRUE)
    }
    writeLines("Comparison between true and decoded states\n")
    if(controls[["model"]]=="hmm"){
      compare_true_predicted_states(controls[["states"]][1],decoding,data[["states0"]])
    }
    if(controls[["model"]]=="hhmm"){
      compare_true_predicted_states(controls[["states"]][1],decoding[,1],data[["states0"]][,1],label="CS ")
      writeLines("")
      for(cs_state in seq_len(controls[["states"]][1])){
        writeLines(paste0("Conditional on true CS state ",cs_state,":"))
        compare_true_predicted_states(controls[["states"]][2],decoding[data[["states0"]][,1]==cs_state,-1],data[["states0"]][data[["states0"]][,1]==cs_state,-1],label="FS ")
        writeLines("")
      }
    }
  }
}