check_controls = function(controls){
  attach(controls)
  on.exit(detach(controls))
  required_controls = c("modelName","states","timeHorizon")
  for(required_control in required_controls){
    if(!(required_control %in% names(controls))) stop(paste0("Please specify '", required_control, "' in 'controls'."),call.=FALSE)
  }
  all_controls = c("modelName","dataSource","trunc_data","states","timeHorizon","fix_df","runs","iterlim","hessian","seed")
  missing_controls = setdiff(all_controls,names(controls))
  for(missing_control in missing_controls){
    warning(paste("The following element in 'control' is not specified and set to default:", missing_control),call.=FALSE)
  }
  if("dataSource" %in% missing_controls) controls[["dataSource"]] = c(NA,NA)
  if("trunc_data" %in% missing_controls) controls[["trunc_data"]] = c(NA,NA)
  if("fix_df" %in% missing_controls) controls[["fix_df"]] = c(NA,NA)
  if("runs" %in% missing_controls) controls[["runs"]] = 200
  if("iterlim" %in% missing_controls) controls[["iterlim"]] = 500
  if("hessian" %in% missing_controls) controls[["hessian"]] = TRUE
  if("seed" %in% missing_controls) controls[["seed"]] = NULL
  for(redundant_control in setdiff(names(controls),all_controls)){
    warning(paste("The following element in 'control' is not supported and will be ignored:", redundant_control),call.=FALSE)
  }
  controls_with_length_2 = c("dataSource","trunc_data","states","timeHorizon")
  for(control_with_length_2 in controls_with_length_2){
    if(length(controls[[control_with_length_2]])!=2) stop(paste0("'", control_with_length_2, "' in 'controls' must be a vector of length 2."),call.=FALSE)
  }
  numeric_controls = c("states","timeHorizon","runs","iterlim","seed")
  for(numeric_control in numeric_controls){
    if(!is.numeric(controls[[numeric_control]])) stop(paste0("'", numeric_control, "' in 'controls' must be numeric."),call.=FALSE)
  }
  if(is.na(controls[["dataSource"]][1])) controls[["sim"]] = TRUE
  if(!is.na(controls[["dataSource"]][1])) controls[["sim"]] = FALSE
  if(controls[["states"]][2]==0) {
    controls[["model"]] = "HMM"
    writeLines("Model: HMM")
    writeLines(paste("States:",controls[["states"]][1]))
    writeLines("State-dependent distributions: t-distributions",sep=" ")
    if(is.na(controls[["fix_df"]][1])) writeLines("")
    if(!is.na(controls[["fix_df"]][1])) writeLines(paste0("(degrees of freedom fixed to ",controls[["fix_df"]][1],")"))
  } else {
    controls[["model"]] = "HHMM"
    writeLines("Model: HHMM")
    writeLines(paste("States:",states[1],"/",states[2]))
    writeLines("State-dependent distributions: t-distributions",sep=" ")
    if(is.na(controls[["fix_df"]][1])) writeLines("")
    if(!is.na(controls[["fix_df"]][1])) writeLines(paste0("(degrees of freedom fixed to ",controls[["fix_df"]][1]," / ",controls[["fix_df"]][2],")"))
  }
  return(controls)
}

# TODO: truncation?, timeHorizon still valid?
check_data = function(controls,data){
  if(controls[["sim"]]){
    writeLines("Data: simulated")
  }
  if(!controls[["sim"]]){
    writeLines("Data: empirical")
  }
}

# TODO: runs?, iterlim to close at true iterations?, hessian computed?, seed
check_estimation = function(controls){

}

# TODO: does path already exists? overwriting?
check_save_path = function(controls){
  
}
