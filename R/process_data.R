#' Process data
#' @param controls A list of controls
#' @param sim_par A vector of model parameters for simulation, default \code{NULL}
#' @return A list of processed data information
process_data = function(controls,sim_par=NULL){
  if(is.null(controls[["controls_checked"]])){
    stop(sprintf("%s (%s)",exception("C.1")[2],exception("C.1")[1]),call.=FALSE)
  }
  ### process data
  if(controls[["sim"]]){
    data = simulate_data(controls,sim_par)
  }
  if(!controls[["sim"]]){
    data = read_data(controls)
  }
  message("Data processed")
  ### check for improper use of state-dependent gamma distribution
  if(controls[["model"]]=="HMM"){
    if(controls[["sdds"]][1]=="gamma" & any(data[["logReturns"]]<0)){
      stop(sprintf("%s (%s)",exception("C.7")[2],exception("C.7")[1]),call.=FALSE)
    }
  }
  if(controls[["model"]]=="HHMM"){
    if(controls[["sdds"]][1]=="gamma" & any(data[["logReturns"]][,1]<0)){
      stop(sprintf("%s (%s)",exception("C.7")[2],exception("C.7")[1]),call.=FALSE)
    }
    if(controls[["sdds"]][2]=="gamma" & any(data[["logReturns"]][,-1]<0)){
      stop(sprintf("%s (%s)",exception("C.7")[2],exception("C.7")[1]),call.=FALSE)
    }
  }
  ### print data characteristics
  if(controls[["sim"]]){
    if(controls[["model"]]=="HMM"){
      writeLines(sprintf("%13-s %s","sample size:",length(data[["logReturns"]])))
    }
    if(controls[["model"]]=="HHMM"){
      writeLines(sprintf("%13-s %s / %s","sample size:",dim(data[["logReturns"]])[1],length(data[["logReturns"]][,-1][!is.na(data[["logReturns"]][,-1])])))
    }
  } 
  if(!controls[["sim"]]){
    if(controls[["model"]]=="HMM"){
      writeLines(sprintf("%13-s %s","source:",controls[["data_source"]][1]))
      writeLines(sprintf("%13-s %s","column:",controls[["data_col"]][1]))
      writeLines(sprintf("%13-s %s to %s","horizon:",data[["dates"]][1],rev(data[["dates"]])[1]))
      writeLines(sprintf("%13-s %s","data points:",length(data[["logReturns"]])))
    }
    if(controls[["model"]]=="HHMM"){
      writeLines(sprintf("%13-s %s / %s","source:",controls[["data_source"]][1],controls[["data_source"]][2]))
      writeLines(sprintf("%13-s %s / %s","column:",controls[["data_col"]][1],controls[["data_col"]][2]))
      writeLines(sprintf("%13-s %s to %s","horizon:",data[["dates"]][1],rev(data[["dates"]])[1]))
      writeLines(sprintf("%13-s %s / %s","data points:",dim(data[["logReturns"]])[1],length(data[["logReturns"]][,-1][!is.na(data[["logReturns"]][,-1])])))
      writeLines(sprintf("%13-s %s","CS data type:",controls[["data_cs_type"]]))
      if(is.numeric(controls[["time_horizon"]][2])) writeLines(sprintf("%14-s%s","FS dim:",controls[["time_horizon"]][2]))
      if(controls[["time_horizon"]][2]=="w") writeLines(sprintf("%13-s %s","FS dim:","weekly"))
      if(controls[["time_horizon"]][2]=="m") writeLines(sprintf("%13-s %s","FS dim:","monthly"))
      if(controls[["time_horizon"]][2]=="q") writeLines(sprintf("%13-s %s","FS dim:","quarterly"))
      if(controls[["time_horizon"]][2]=="y") writeLines(sprintf("%13-s %s","FS dim:","yearly"))
    }
  }
  
  ### save data object
  check_saving(object = data, filetype = "rds", controls = controls)
  
  ### return data object
  return(data)
}