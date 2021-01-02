check_controls = function(controls){
  
  ### check supplied control values
  all_controls = c("model_name","data_source","truncate_data","states","time_horizon","fix_dfs","runs","iterlim","hessian","seed","print.level","steptol","accept_codes","overwrite")
  required_controls = c("model_name","states")
  artificial_controls = c("sim","model","est_dfs","HHMM_av","controls_checked")
  missing_controls = setdiff(all_controls,names(controls))
  redundant_controls = setdiff(names(controls),c(all_controls,artificial_controls))
  controls_with_length_2 = c("data_source","truncate_data","states","time_horizon","fix_dfs")
  numeric_controls = c("states","runs","iterlim","print.level","steptol","accept_codes","seed")
  boolean_controls = c("hessian","overwrite")
  for(required_control in required_controls){
    if(!(required_control %in% names(controls))) stop(paste0("Please specify '", required_control, "' in 'controls'."),call.=FALSE)
  }
  if(length(missing_controls)>=1) warning(paste("Some controls are not specified and set to default."),call.=FALSE)
  if(length(redundant_controls)==1) warning(paste("The following element in 'controls' is not supported and will be ignored:", paste(redundant_controls,collapse=", ")),call.=FALSE)
  if(length(redundant_controls)>1) warning(paste("The following elements in 'controls' are not supported and will be ignored:", paste(redundant_controls,collapse=", ")),call.=FALSE)
  for(control_with_length_2 in intersect(controls_with_length_2,names(controls))){
    if(length(controls[[control_with_length_2]])!=2) stop(paste0("'", control_with_length_2, "' in 'controls' must be a vector of length 2."),call.=FALSE)
  }
  for(numeric_control in intersect(numeric_controls,names(controls))){
    if(length(controls[[numeric_control]])==1) if(!is.numeric(controls[[numeric_control]])) stop(paste0("The element '", numeric_control, "' in 'controls' must be numeric."),call.=FALSE)
    if(length(controls[[numeric_control]])>1) if(!isTRUE(all(controls[[numeric_control]] == floor(controls[[numeric_control]])))) stop(paste0("The element '", numeric_control, "' in 'controls' must be a numeric vector."),call.=FALSE)
  }
  for(boolean_control in intersect(boolean_controls,names(controls))){
    if(!is.logical(controls[[boolean_control]])) stop(paste0("'", boolean_control, "' in 'controls' must be a boolean."),call.=FALSE)
  }
  
  ### set default values
  if("data_source" %in% missing_controls) controls[["data_source"]] = c(NA,NA)
  if("truncate_data" %in% missing_controls) controls[["truncate_data"]] = c(NA,NA)
  if("time_horizon" %in% missing_controls) controls[["time_horizon"]] = c(NA,NA)
  if("fix_dfs" %in% missing_controls) controls[["fix_dfs"]] = c(NA,NA)
  if("runs" %in% missing_controls) controls[["runs"]] = 200
  if("iterlim" %in% missing_controls) controls[["iterlim"]] = 500
  if("hessian" %in% missing_controls) controls[["hessian"]] = TRUE
  if("print.level" %in% missing_controls) controls[["print.level"]] = 0
  if("steptol" %in% missing_controls) controls[["steptol"]] = 1e-8
  if("accept_codes" %in% missing_controls) controls[["accept_codes"]] = c(1)
  if("overwrite" %in% missing_controls) controls[["overwrite"]] = FALSE; if(controls[["overwrite"]]) warning("Saved model results may be overwritten.",call.=FALSE)
  
  ### create artificial controls
  controls[["model"]] = if(controls[["states"]][2]==0) "HMM" else "HHMM"
  if(controls[["model"]]=="HMM"){
    controls[["sim"]] = if(is.na(controls[["data_source"]][1])) TRUE else FALSE;
    controls[["est_dfs"]] = if(is.na(controls[["fix_dfs"]][1])) TRUE else FALSE;
  }
  if(controls[["model"]]=="HHMM"){
    controls[["sim"]] = if(is.na(controls[["data_source"]][2])) TRUE else FALSE;
    if(!controls[["sim"]] & is.na(controls[["data_source"]][1]) & !is.na(controls[["data_source"]][2])) controls[["HHMM_av"]] = TRUE
    if(!controls[["sim"]] & all(!is.na(controls[["data_source"]]))) controls[["HHMM_av"]] = FALSE
    controls[["est_dfs"]] = if(any(is.na(controls[["fix_dfs"]]))) TRUE else FALSE;
  }
  
  ### check correct parameter format for HMM and HHMM resp.
  if(controls[["sim"]] & any(!is.na(controls[["truncate_data"]]))){
    warning("Entries of 'truncate_data' will be ignored.",call.=FALSE)
    controls[["truncate_data"]] = c(NA,NA)
  }
  if(controls[["model"]]=="HMM") {
    if(controls[["states"]][1]%%1!=0 || controls[["states"]][1]<2){
      stop("First entry of 'states' in 'controls' must be an integer greater or equal 2.",call.=FALSE)
    }
    if(!is.na(controls[["data_source"]][2])){
      warning("Second entry of 'data_source' will be ignored.",call.=FALSE)
      controls[["data_source"]][2] = NA
    }
    if(controls[["sim"]] & is.na(controls[["time_horizon"]][1])){
      stop("Either first entry of 'data_source' or 'time_horizon' has to be specified.",call.=FALSE)
    }
    if(!controls[["sim"]] & !is.na(controls[["time_horizon"]][1])){
      warning("First entry of 'time_horizon' will be ignored.",call.=FALSE)
      controls[["time_horizon"]][1] = NA
    }
    if(!is.na(controls[["time_horizon"]][2])){
      warning("Second entry of 'time_horizon' will be ignored.",call.=FALSE)
      controls[["time_horizon"]][2] = NA
    }
    if(!is.na(controls[["fix_dfs"]][2])){
      warning("Second entry of 'fix_dfs' will be ignored.",call.=FALSE)
      controls[["fix_dfs"]][2] = NA
      }
  }
  if(controls[["model"]]=="HHMM") {
    if(controls[["states"]][1]%%1!=0 || controls[["states"]][1]<2){
      stop("First entry of 'states' in 'controls' must be an integer greater or equal 2.",call.=FALSE)
    }
    if(controls[["states"]][2]%%1!=0 || controls[["states"]][2]<2){
      stop("Second entry of 'states' in 'controls' must be an integer greater or equal 2.",call.=FALSE)
    }
    if(controls[["sim"]] & any(!is.na(controls[["data_source"]]))){
      warning("Entries of 'data_source' will be ignored.",call.=FALSE)
      controls[["data_source"]] = c(NA,NA)
    }
    if(controls[["sim"]] & any(is.na(controls[["time_horizon"]]))){
      stop("Either 'data_source' or 'time_horizon' has to be specified.",call.=FALSE)
    }
    if(!controls[["sim"]] & is.na(controls[["time_horizon"]][2])){
      stop("Second entry of 'time_horizon' has to be specified.",call.=FALSE)
    }
    if(!controls[["sim"]] & !is.na(controls[["time_horizon"]][1])){
      warning("First entry of 'time_horizon' will be ignored.",call.=FALSE)
      controls[["time_horizon"]][1] = NA
    }
  }
  
  ### check if data paths are correct
  if(!is.na(controls[["data_source"]][1]) & !file.exists(paste0("data/",controls[["data_source"]][1]))) stop(paste0("File 'data/",controls[["data_source"]][1],"' does not exist."),call.=FALSE)
  if(!is.na(controls[["data_source"]][2]) & !file.exists(paste0("data/",controls[["data_source"]][2]))) stop(paste0("File 'data/",controls[["data_source"]][2],"' does not exist."),call.=FALSE)
  
  ### note that 'controls' is checked
  writeLines("Checks successful.")
  controls[["controls_checked"]] = TRUE
  
  ### print model specification
  writeLines(paste0("Model name: ",controls[["model_name"]]))
  writeLines(paste0("Model:      ",controls[["model"]]))
  if(controls[["sim"]]) writeLines("Data:       simulated")
  if(!controls[["sim"]]) writeLines("Data:       empirical")
  if(controls[["model"]]=="HMM") {
    writeLines(paste0("States:     ",controls[["states"]][1]))
    writeLines("SDDs:       t-distribution")
  } 
  if(controls[["model"]]=="HHMM") {
    writeLines(paste0("States:     ",controls[["states"]][1],"/",controls[["states"]][2]))
    writeLines("SDDs:       t-distribution")
  }
  if(!is.null(controls[["seed"]])) writeLines(paste0("Seed:       ",controls[["seed"]]))
  
  ### save controls
  check_saving(controls,controls)
  
  return(controls)
}

check_data = function(controls,data){
  if(controls[["sim"]]){
    if(controls[["model"]]=="HMM")  writeLines(paste0("Data points: ",controls[["time_horizon"]][1]))
    if(controls[["model"]]=="HHMM") writeLines(paste0("Data points: ",controls[["time_horizon"]][1],"/",controls[["time_horizon"]][2]))
  }
  if(!controls[["sim"]]){
    writeLines("Source:     ",sep=" ")
    if(controls[["model"]]=="HMM"){
      writeLines(paste0("closing prices of '",controls[["data_source"]][1],"'"))
      writeLines(paste0("Horizon:     ", data[["dates"]][1], " to ", rev(data[["dates"]])[1]))
      writeLines(paste0("Data points: ",length(data[["observations"]])))
    }
    if(controls[["model"]]=="HHMM"){
      if(controls[["HHMM_av"]]) writeLines(paste0("average-/closing prices of '",controls[["data_source"]][2],"'"))
      if(!controls[["HHMM_av"]]) writeLines(paste0("average-/closing prices of '",controls[["data_source"]][1],"'/'",controls[["data_source"]][2],"'"))
      writeLines(paste0("Horizon:     ", data[["dates"]][1], " to ", rev(data[["dates"]])[1]))
      writeLines(paste0("FS dim:      ",controls[["time_horizon"]][2]))
      writeLines(paste0("Data points: ",dim(data[["observations"]])[1],"/",dim(data[["observations"]])[2]-1))
    }
  }
  
  ### save data
  check_saving(data,controls)
}

check_estimation = function(time,mods,llks,data,controls){
  if(all(is.na(llks))) stop("None of the estimation runs ended successfully. Consider increasing 'runs' in 'controls'.",call.=FALSE)
  writeLines(paste0("Estimation finished, it took ",time," minute(s). Successful runs: ",sum(!is.na(llks))," out of ",length(llks),"."))

  ### select model with highest LL
  mod       = mods[[which.max(llks)]]
  mod_LL    = -mod$minimum
  thetaCon  = thetaUncon2thetaCon(mod$estimate,controls)
  thetaList = statesDecreasing(thetaCon2thetaList(thetaCon,controls),controls)
  
  ### check for unidentified states
  warning_unidentified_states = FALSE
  if(any(abs(Gamma2delta(thetaList$Gamma)-0)<1e-04)) warning_unidentified_states = TRUE
  if(controls[["model"]]=="HHMM"){
    for(s in seq_len(controls[["states"]][2])){
      if(any(abs(Gamma2delta(thetaList$Gammas_star[[s]])-0)<1e-04)) warning_unidentified_states = TRUE
    }
  }
  if(warning_unidentified_states) warning("Some states seem to be unidentified.",call.=FALSE)
  
  ### create visualization of LLs
  filename = paste0("models/",controls[["model_name"]],"/lls.pdf")
  if(controls[["overwrite"]]==FALSE & file.exists(filename)){
    warning(paste0("Cannot create a plot of the log-likelihoods because the path '",filename,"' already exists and you chose not to overwrite."),call.=FALSE)
  } else {
    pdf(filename, width=9, height=7)
      plot(llks,yaxt="n",xlab="Estimation run",ylab="",main="Log-likelihoods",pch=16,ylim=c(floor(min(llks,na.rm=TRUE)),ceiling(max(llks,na.rm=TRUE))))
      points(x=which.max(llks),y=mod_LL,pch=16,cex=1.25,col="red")
      axis(2,las=1,at=unique(round(llks[!is.na(llks)])),labels=unique(round(llks[!is.na(llks)])))
    invisible(dev.off())
  }
  
  ### check if iteration limit was reached
  if(mod[["iterations"]] >= controls[["iterlim"]]) warning("Selected estimation run reached the iteration limit. Consider increasing 'iterlim'.",call.=FALSE)
  exceeded_runs = unlist(lapply(mods,function (x) x[["iterations"]])) >= controls[["iterlim"]] 
  if(any(exceeded_runs)) warning(paste0(sum(exceeded_runs)," of ",length(llks)," runs reached the iteration limit. Consider increasing 'iterlim'."),call.=FALSE)
  
  ### compute model selection criteria
  noPar = function(M,N,est_dfs) return(M*(M-1) + M*N*(N-1) + M + M*N + M + M*N + est_dfs*(M + M*N))
  compAIC = function(M,N,LL,est_dfs) return(2*noPar(M,N,est_dfs) - 2*LL)
  compBIC = function(T,M,N,LL,est_dfs) return(log(T)*noPar(M,N,est_dfs) - 2*LL)
  
  ### create estimation output
  fit = list("LL"        = mod_LL,
             "mod"       = mod,
             "thetaList" = thetaList,
             "AIC"       = compAIC(controls$states[1],controls$states[2],-mod$minimum,controls[["est_dfs"]]),
             "BIC"       = compBIC(prod(dim(t(data$observations))),controls$states[1],controls$states[2],-mod$minimum,controls[["est_dfs"]]),
             "all_LL"    = llks,
             "all_mods"  = mods
  )
  file = paste0("models/",controls[["model_name"]],"/estimates.txt")
  if(file.exists(file) & !controls[["overwrite"]]){ 
    warning(paste0("Cannot save 'estimates.txt' because '",filename,"' already exists and you chose not to overwrite."),call.=FALSE) 
  } else {
    options(max.print=1000000)
    sink(file=file)
    first_col = c("LL","AIC","BIC","exit code","iterations", "run time (min)")
    second_col = c(fit[["LL"]],fit[["AIC"]],fit[["BIC"]],mod[["code"]],mod[["iterations"]],time)
    df = data.frame(first_col,second_col); names(df) = NULL
    writeLines(paste0("Results of model '",controls[["model_name"]],"':")); print(df,row.names=FALSE,right=FALSE); writeLines("")
    if(controls[["sim"]]){
      writeLines("True parameter values:\n"); print(data[["thetaList0"]])
    }
    writeLines("Estimates:\n"); print(fit[["thetaList"]])
    writeLines("Gradient:\n"); print(mod[["gradient"]]); writeLines("")
    if(controls[["hessian"]]){
      writeLines("Hessian:\n"); print(mod[["hessian"]])
    }
    sink()
    options(max.print=1000)
  }
  
  ### save results
  check_saving(fit,controls)
  
  return(fit)
}

check_decoding = function(decoding,controls){
  
  ### create states summary output
  file = paste0("models/",controls[["model_name"]],"/states.txt")
  if(file.exists(file) & !controls[["overwrite"]]){ 
    warning(paste0("Cannot save 'states.txt' because '",filename,"' already exists and you chose not to overwrite."),call.=FALSE) 
  } else {
    sink(file=file)
    writeLines("Frequency of decoded states:\n")
    if(controls[["model"]]=="HMM"){
      out = table((decoding)); names(out) = paste("state",names(out))
      print(out)
    }
    if(controls[["model"]]=="HHMM"){
      out_cs = table((decoding[,1])); names(out_cs) = paste("CS state",names(out_cs))
      print(out_cs); writeLines("")
      for(state in seq_len(controls[["states"]][2])){
        writeLines(paste0("Conditional on CS state ",state,":"))
        out_fs = table((decoding[decoding[,1]==state,-1])); names(out_fs) = paste("FS state",names(out_fs))
        print(out_fs); writeLines("")
      }
    }
    sink()
  }
  
  ### save decoding
  check_saving(decoding,controls)
  
  writeLines("State decoding successful.")
}

check_saving = function(object,controls){
  overwrite = controls[["overwrite"]]
  path = paste0("models/",controls[["model_name"]])
  name = deparse(substitute(object))
  filename = paste0(path,"/",name)
  if(!dir.exists(path)){
    dir.create(path)
  }
  if(file.exists(filename)){
    if(overwrite){
      saveRDS(object,file=filename)
    } else { 
      warning(paste0("Cannot save '",name,"' because '",filename,"' already exists and you chose not to overwrite."),call.=FALSE) 
      }
  } else {
    saveRDS(object,file=filename)
    }
}