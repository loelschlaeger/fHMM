#' @title Data processing
#' @description Calls functions for processing or simulating data.
#' @param controls A list of controls.
#' @param sim_par A vector of model parameters for simulation.
#' @return A list of processed data information and on-screen information.

process_data = function(controls,sim_par){
  
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
  message("Data processed.")
  
  ### check for improper use of state-dependent gamma distribution
  if(controls[["model"]]=="HMM"){
    if(controls[["sdds"]][1]=="gamma" & any(data[["data"]]<0)){
      stop(sprintf("%s (%s)",exception("C.7")[2],exception("C.7")[1]),call.=FALSE)
    }
  }
  if(controls[["model"]]=="HHMM"){
    if(controls[["sdds"]][1]=="gamma" & any(data[["data"]][,1]<0)){
      stop(sprintf("%s (%s)",exception("C.7")[2],exception("C.7")[1]),call.=FALSE)
    }
    if(controls[["sdds"]][2]=="gamma" & any(data[["data"]][,-1]<0,na.rm=TRUE)){
      stop(sprintf("%s (%s)",exception("C.7")[2],exception("C.7")[1]),call.=FALSE)
    }
  }
  
  ### print data characteristics
  if(controls[["sim"]]){
    if(controls[["model"]]=="HMM"){
      writeLines(sprintf("%13-s %s","sample size:",length(data[["data"]])))
    }
    if(controls[["model"]]=="HHMM"){
      writeLines(sprintf("%13-s %s / %s","sample size:",dim(data[["data"]])[1],length(data[["data"]][,-1][!is.na(data[["data"]][,-1])])))
    }
  } 
  if(!controls[["sim"]]){
    if(controls[["model"]]=="HMM"){
      writeLines(sprintf("%18-s %s","data source:",controls[["data"]][["source"]][1]))
      writeLines(sprintf("%18-s %s","data column:",controls[["data"]][["column"]][1]))
      writeLines(sprintf("%18-s %s to %s","time horizon:",data[["dates"]][1],rev(data[["dates"]])[1]))
      writeLines(sprintf("%18-s %s","data points:",length(data[["data"]])))
      writeLines(sprintf("%18-s %s","log-returns:",controls[["data"]][["log_returns"]][1]))
    }
    if(controls[["model"]]=="HHMM"){
      writeLines(sprintf("%18-s %s / %s","data source:",controls[["data"]][["source"]][1],controls[["data"]][["source"]][2]))
      writeLines(sprintf("%18-s %s / %s","data column:",controls[["data"]][["column"]][1],controls[["data"]][["column"]][2]))
      writeLines(sprintf("%18-s %s to %s","time horizon:",data[["dates"]][1],rev(data[["dates"]])[1]))
      writeLines(sprintf("%18-s %s / %s","data points:",dim(data[["data"]])[1],length(data[["data"]][,-1][!is.na(data[["data"]][,-1])])))
      writeLines(sprintf("%18-s %s / %s","log-returns:",controls[["data"]][["log_returns"]][1],controls[["data"]][["log_returns"]][2]))
      writeLines(sprintf("%18-s %s","CS transformation:",controls[["data"]][["cs_transform"]]))
      if(is.numeric(controls[["horizon"]][2])) writeLines(sprintf("%18-s %s","FS dimension:",controls[["horizon"]][2]))
      if(controls[["horizon"]][2]=="w") writeLines(sprintf("%18-s %s","FS dimension:","weekly"))
      if(controls[["horizon"]][2]=="m") writeLines(sprintf("%18-s %s","FS dimension:","monthly"))
      if(controls[["horizon"]][2]=="q") writeLines(sprintf("%18-s %s","FS dimension:","quarterly"))
      if(controls[["horizon"]][2]=="y") writeLines(sprintf("%18-s %s","FS dimension:","yearly"))
    }
  }
  
  ### save data object
  check_saving(object = data, filetype = "rds", controls = controls)
  
  ### return data object
  return(data)
}