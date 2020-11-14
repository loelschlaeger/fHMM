### checks control parameters

check_controls = function(controls){
  required_controls = c("modelName","states","timeHorizon")
  for(required_control in required_controls){
    if(!(required_control %in% names(controls))) stop(paste0("Please specify '", required_control, "' in 'controls'."),call.=FALSE)
  }
  all_controls = c("modelName","dataSource","trunc_data","states","timeHorizon","fix_df","runs","iterlim","hessian","seed")
  missing_controls = setdiff(all_controls,names(controls))
  for(missing_control in missing_controls){
    warning(paste("The following control is not specified and set to default:", missing_control))
  }
  if("dataSource" %in% missing_controls) controls[["dataSource"]] = c(NA,NA)
  if("trunc_data" %in% missing_controls) controls[["trunc_data"]] = c(NA,NA)
  if("fix_df" %in% missing_controls) controls[["fix_df"]] = c(NA,NA)
  if("runs" %in% missing_controls) controls[["runs"]] = 200
  if("iterlim" %in% missing_controls) controls[["iterlim"]] = 500
  if("hessian" %in% missing_controls) controls[["hessian"]] = FALSE
  if("seed" %in% missing_controls) controls[["seed"]] = NULL
  for(redundant_control in setdiff(names(controls),all_controls)){
    warning(paste("The following control is not supported and will be ignored:", redundant_control))
  }
  controls_with_length_2 = c("dataSource","trunc_data","states","timeHorizon")
  for(control_with_length_2 in controls_with_length_2){
    if(length(controls[[control_with_length_2]])!=2) stop(paste0("'", control_with_length_2, "' in 'controls' must be a vector of length 2."),call.=FALSE)
  }
  numeric_controls = c("states","timeHorizon","runs","iterlim","seed")
  for(numeric_control in numeric_controls){
    if(!is.numeric(controls[[numeric_control]])) stop(paste0("'", numeric_control, "' in 'controls' must be numeric."),call.=FALSE)
  }
  return(controls)
}

# TODO: extend for HHMM
check_model = function(controls){
  attach(controls)
  on.exit(detach(controls))
  if(states[2]==0) {
    writeLines("Model: HMM")
    writeLines(paste("States:",states[1]))
    writeLines(paste("Time horizon:",timeHorizon[1]))
    writeLines("State dependent distributions: t-distributions",sep=" ")
    if(is.na(fix_df[1])) writeLines(paste("Degrees :",timeHorizon[1]))
  } else { 
    writeLines("Model: HHMM")
  }
}

# TODO: sim or emp data, truncation?, timeHorizon still valid?
check_data = function(controls){
  return(TRUE)
}

# TODO: runs, iterlim, hessian, seed
check_estimation = function(controls){

}

# TODO: does path already exists? overwriting?
check_save_path = function(controls){
  
}
