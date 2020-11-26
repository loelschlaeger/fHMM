check_controls = function(controls){
  
  all_controls = c("modelName","dataSource","trunc_data","states","timeHorizon","fix_df","runs","iterlim","hessian","seed","print.level","steptol","accept_codes","overwrite")
  required_controls = c("modelName","states","timeHorizon")
  artificial_controls = c("sim","model","est_df")
  missing_controls = setdiff(all_controls,names(controls))
  redundant_controls = setdiff(names(controls),c(all_controls,artificial_controls))
  controls_with_length_2 = c("dataSource","trunc_data","states","timeHorizon")
  numeric_controls = c("states","runs","iterlim","print.level","steptol","accept_codes","seed")
  boolean_controls = c("hessian","overwrite")
  
  for(required_control in required_controls){
    if(!(required_control %in% names(controls))) stop(paste0("Please specify '", required_control, "' in 'controls'."),call.=FALSE)
  }
  
  if(length(missing_controls)>=1) warning(paste("Some elements in 'controls' are not specified and set to default."),call.=FALSE)

  if(length(redundant_controls)==1) warning(paste("The following element in 'controls' is not supported and will be ignored:", paste(redundant_controls,collapse=", ")),call.=FALSE)
  if(length(redundant_controls)>1) warning(paste("The following elements in 'controls' are not supported and will be ignored:", paste(redundant_controls,collapse=", ")),call.=FALSE)
  
  if("dataSource" %in% missing_controls) controls[["dataSource"]] = c(NA,NA)
  if("trunc_data" %in% missing_controls) controls[["trunc_data"]] = c(NA,NA)
  if("fix_df" %in% missing_controls) controls[["fix_df"]] = c(NA,NA)
  if("runs" %in% missing_controls) controls[["runs"]] = 100
  if("iterlim" %in% missing_controls) controls[["iterlim"]] = 500
  if("hessian" %in% missing_controls) controls[["hessian"]] = TRUE
  if("print.level" %in% missing_controls) controls[["print.level"]] = 0
  if("steptol" %in% missing_controls) controls[["steptol"]] = 1e-8
  if("accept_codes" %in% missing_controls) controls[["accept_codes"]] = c(1,2)
  if("overwrite" %in% missing_controls) controls[["overwrite"]] = FALSE; if(controls[["overwrite"]]) warning("Saved model results may be overwritten.",call.=FALSE)

  for(control_with_length_2 in controls_with_length_2){
    if(length(controls[[control_with_length_2]])!=2) stop(paste0("'", control_with_length_2, "' in 'controls' must be a vector of length 2."),call.=FALSE)
  }
  
  for(numeric_control in numeric_controls){
    if(length(controls[[numeric_control]])==1) if(!is.numeric(controls[[numeric_control]])) stop(paste0("The element '", numeric_control, "' in 'controls' must be numeric."),call.=FALSE)
    if(length(controls[[numeric_control]])>1) if(!isTRUE(all(controls[[numeric_control]] == floor(controls[[numeric_control]])))) stop(paste0("The element '", numeric_control, "' in 'controls' must be a numeric vector."),call.=FALSE)
  }
  
  for(boolean_control in boolean_controls){
    if(!is.logical(controls[[boolean_control]])) stop(paste0("'", boolean_control, "' in 'controls' must be a boolean."),call.=FALSE)
  }
  
  controls[["sim"]] = if(is.na(controls[["dataSource"]][1])) TRUE else FALSE;
  controls[["model"]] = if(controls[["states"]][2]==0) "HMM" else "HHMM"
  controls[["est_df"]] = if(any(is.na(controls[["fix_df"]]))) TRUE else FALSE;
  
  if(controls[["model"]]=="HMM") {
    writeLines(paste0("Model: ",controls[["states"]][1],"-state HMM with state-dependent t-distributions"),sep=" ")
    if(!controls[["est_df"]]) writeLines("") else writeLines(paste0("(degrees of freedom fixed to ",controls[["fix_df"]][1],")"))
  } else {
    writeLines(paste0("Model: ",controls[["states"]][1],"/",controls[["states"]][2],"-state HHMM with state-dependent t-distributions"),sep=" ")
    if(!controls[["est_df"]]) writeLines("") else writeLines(paste0("(degrees of freedom fixed to ",controls[["fix_df"]][1],"/",controls[["fix_df"]][2],")"))
  }
  
  check_saving(controls,controls)
  
  return(controls)
}

check_data = function(controls,data){
  if(controls[["sim"]]){
    writeLines(paste0("Data: simulated, T=",controls[["timeHorizon"]][1]),sep="")
    if(controls[["model"]]=="HMM") writeLines("") else writeLines(paste0(", T*=",controls[["timeHorizon"]][2]),sep="")
    if(is.null(controls[["seed"]])) writeLines("") else writeLines(paste0(", seed=",controls[["seed"]]))
  }
  if(!controls[["sim"]]){
    writeLines("Data: empirical")
    #TODO: time horizon, dates, data names
  }
  check_saving(data,controls)
}

check_estimation = function(time,mods,llks,data,controls){
  if(all(is.na(llks))) stop("None of the estimation runs ended successfully. Consider increasing 'runs' in 'controls'.",call.=FALSE)
  writeLines(paste0("Done with estimation, it took ",time," minute(s). Successful runs: ",sum(!is.na(llks))," out of ",length(llks),"."))

  mod       = mods[[which.min(llks)]]
  thetaCon  = thetaUncon2thetaCon(mod$estimate,controls)
  thetaList = statesDecreasing(thetaCon2thetaList(thetaCon,controls),controls)
  
  #TODO: check if iterlim was exceeded -> increase!
  #TODO: sink results to txt file
  
  noPar = function(M,N,est_df) return(M*(M-1) + M*N*(N-1) + M + M*N + M + M*N + est_df*(M + M*N))
  compAIC = function(M,N,LL,est_df) return(2*noPar(M,N,est_df) - 2*LL)
  compBIC = function(T,M,N,LL,est_df) return(log(T)*noPar(M,N,est_df) - 2*LL)
  
  est = list("LL"        = -mod$minimum,
             "all_LL"    = -llks,
             "thetaList" = thetaList,
             "mod"       = mod,
             "AIC"       = compAIC(controls$states[1],controls$states[2],-mod$minimum,controls[["est_df"]]),
             "BIC"       = compBIC(prod(dim(t(data$observations))),controls$states[1],controls$states[2],-mod$minimum,controls[["est_df"]])
            )
  
  check_saving(est,controls)
  
  return(est)
}

check_saving = function(object,controls){
  overwrite = controls[["overwrite"]]
  path = paste0("models/",controls[["modelName"]])
  name = deparse(substitute(object))
  filename = paste0(path,"/",name)
  if(!dir.exists(path)){
    dir.create(path)
  }
  if(file.exists(filename)){
    if(overwrite){
      saveRDS(object,file=filename)
    } else { 
      warning(paste0("Cannot save '",name,"' because the path '",filename,"' already exists and you chose not to overwrite."),call.=FALSE) 
      }
  } else {
    saveRDS(object,file=filename)
    }
}