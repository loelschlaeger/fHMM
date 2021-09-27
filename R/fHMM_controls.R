#' Set and check controls for the fHMM package.
#' @description 
#' This function sets and checks the specification of controls for the fHMM 
#' package. 
#' @details 
#' See the vignettes on how to specify \code{controls}.
#' @param controls 
#' A list of controls.
#' @return 
#' An object of class \code{RprobitB_controls}. 
#' @export

set_controls = function(controls = NULL) {
  
  ### define names of all controls
  all_controls = c("path","model","states","sdds","horizon","data","fit")
  data_controls = c("source","column","truncate","cs_transform")
  fit_controls = c("runs","at_true","seed","accept","print.level","gradtol",
                   "steptol","stepmax","steptol","iterlim")
  
  ### initialize controls
  if(is.null(controls)) 
    controls = list()
  
  ### check redundant controls
  redundant_controls = setdiff(names(controls),all_controls)
  if(length(redundant_controls)>0 & is.null(controls[["controls_checked"]]))
    warning("C.4", call.=FALSE, immediate.=TRUE)
  
  ### set default control values
  if(!"path" %in% names(controls))                   
    controls[["path"]] = tempdir()
  if(!"model" %in% names(controls))                  
    controls[["model"]] = "hmm"
  if(!"states" %in% names(controls))                 
    controls[["states"]] = 2
  if(!"sdds" %in% names(controls))                   
    controls[["sdds"]] = "t"
  if(!"horizon" %in% names(controls))                
    controls[["horizon"]] = 1000
  
  if(!"data" %in% names(controls)){
    controls[["data"]] = NA
  } else {
    if(!"source" %in% names(controls[["data"]])){
      if(controls[["model"]] == "hmm")                 
        controls[["data"]][["source"]] = NA
      if(controls[["model"]] == "hhmm")                
        controls[["data"]][["source"]] = c(NA,NA)
    }
    if(!"column" %in% names(controls[["data"]])){
      if(controls[["model"]] == "hmm")                 
        controls[["data"]][["column"]] = NA
      if(controls[["model"]] == "hhmm")                
        controls[["data"]][["column"]] = c(NA,NA)
    }
    if(!"truncate" %in% names(controls[["data"]]))     
      controls[["data"]][["truncate"]] = c(NA,NA)
    if(!"cs_transform" %in% names(controls[["data"]])) 
      controls[["data"]][["cs_transform"]] = NA
    if(!"log_returns" %in% names(controls[["data"]])){
      if(controls[["model"]] == "hmm")                 
        controls[["data"]][["log_returns"]] = TRUE
      if(controls[["model"]] == "hhmm")                
        controls[["data"]][["log_returns"]] = c(TRUE,TRUE)
    }
  }
  
  if(!"fit" %in% names(controls))                    
    controls[["fit"]] = list()
  if(!"runs" %in% names(controls[["fit"]]))          
    controls[["fit"]][["runs"]] = 100
  if(!"at_true" %in% names(controls[["fit"]]))       
    controls[["fit"]][["at_true"]] = FALSE
  if(!"accept" %in% names(controls[["fit"]]))        
    controls[["fit"]][["accept"]] = c(1,2)
  if(!"print.level" %in% names(controls[["fit"]]))   
    controls[["fit"]][["print.level"]] = 0
  if(!"gradtol" %in% names(controls[["fit"]]))       
    controls[["fit"]][["gradtol"]] = 1e-6
  if(!"stepmax" %in% names(controls[["fit"]]))       
    controls[["fit"]][["stepmax"]] = 1
  if(!"steptol" %in% names(controls[["fit"]]))      
    controls[["fit"]][["steptol"]] = 1e-6
  if(!"iterlim" %in% names(controls[["fit"]]))       
    controls[["fit"]][["iterlim"]] = 200

  ### define control that defines if data gets simulated
  if(is.list(controls[["data"]])) {
    if(length(controls[["data"]]) == 0){
      controls[["sim"]] = TRUE
    } else {
      controls[["sim"]] = FALSE
    }
  } else if (is.null(controls[["data"]])) {
    controls[["sim"]] = TRUE
  } else if (is.na(controls[["data"]])) {
    controls[["sim"]] = TRUE
  } else {
    controls[["sim"]] = FALSE
  }
  
  ### check state-dependent distributions
  all_sdds = c("gamma","t")
  extract_dfs = function(x) grepl("^[t][\\(]([1-9][0-9]*|Inf)[\\)]$",x)
  controls[["fixed_dfs"]] = c(NA,NA)
  controls[["fixed_dfs"]][extract_dfs(controls[["sdds"]])] = as.numeric(sub("\\).*", "", sub(".*\\(", "", controls[["sdds"]][extract_dfs(controls[["sdds"]])])))
  controls[["sdds"]][extract_dfs(controls[["sdds"]])] = "t"
  
  ### function that checks if value is an integer
  is.integer = function(x) is.numeric(x) && x>=0 && x%%1==0
  
  ### check single controls
  if(!is.character(controls[["path"]])) 
    stop("The control 'path' must be a character.")
  if(!controls[["model"]] %in% c("hmm","hhmm")) 
    stop("The control 'model' must be one of 'hmm' or 'hhmm'.")
  if(controls[["model"]] == "hmm") 
    if(!(is.integer(controls[["states"]]) & length(controls[["states"]]) == 1 & all(controls[["states"]] >= 2))) 
      stop("The control 'states' must be an integer greater or equal 2.")
  if(controls[["model"]] == "hhmm")
    if(!(is.integer(controls[["states"]]) & length(controls[["states"]]) == 2 & all(controls[["states"]] >= 2))) 
      stop("The control 'states' must be a numeric vector of length 2 containing integers greater or equal 2.")
  if(controls[["model"]] == "hmm")
    if(!(length(controls[["sdds"]]) == 1 & controls[["sdds"]] %in% c("t","gamma")))
      stop("The control 'sdds' must be one of 't' or 'gamma'.")
  if(controls[["model"]] == "hhmm")
    if(!(length(controls[["sdds"]]) == 2 & all(controls[["sdds"]] %in% c("t","gamma"))))
      stop("The control 'sdds' must be a character vector of length 2 containing 't' or 'gamma'.")
  if(controls[["model"]]=="hmm")
    if(controls[["sim"]] & !is.integer(controls[["horizon"]]) & length(controls[["horizon"]]!=1))
      stop("The control 'horizon' must be an integer.")
  if(controls[["model"]]=="hhmm"){
    if(length(controls[["horizon"]])!=2)
      stop("The control 'horizon' must be a vector of length 2.")
    if(!(is.integer(controls[["horizon"]][2]) || controls[["horizon"]][2] %in% c("w","m","q","y")))
      stop("The second entry of the control 'horizon' must be an integer or one of 'w', 'm', 'q', 'y'.")
    if(controls[["sim"]] & !(is.integer(controls[["horizon"]][1])))
      stop("The first entry of the control 'horizon' must be an integer.")
  }
  
  ### check 'data' controls
  if(!controls[["sim"]]){
    if(controls[["model"]]=="hmm"){
      if(!(is.character(controls[["data"]][["source"]])) && length(controls[["data"]][["source"]] == 1))
        stop("The control 'source' must be a character.")
      if(is.na(controls[["data"]][["source"]]))
        stop("The control 'source' in 'data' has to be specified.")
      if(is.na(controls[["data"]][["column"]]))
        stop("The control 'column' in 'data' has to be specified.")
    }
    if(controls[["model"]]=="hhmm"){
      if(!(is.character(controls[["data"]][["source"]])) && length(controls[["data"]][["source"]] == 1))
        stop("The control 'source' must be a character vector of length two.")
      if(any(is.na(controls[["data"]][["source"]])))
        stop("The control 'source' in 'data' has to be specified.")
      if(!controls[["sim"]] & any(is.na(controls[["data"]][["column"]])))
        stop("The control 'column' in 'data' has to be specified.")
      if(!is.function(controls[["data"]][["cs_transform"]]))
        stop("The control 'cs_transform' in 'data' has to of class 'function'.")
    }
  }

  ### check 'fit' controls
  if(!is.integer(controls[["fit"]][["runs"]]))
    stop("The control 'runs' in 'fit' must be an integer.")
  if(!is.logical(controls[["fit"]][["at_true"]]))
    stop("The control 'at_true' in 'fit' must be an integer.")
  if(any(controls[["fit"]][["accept"]]=="all"))
    controls[["fit"]][["accept"]] = 1:5
  
  ### check if data paths are correct
  if(!controls[["sim"]]){
    for(i in c(1,2)){
      if(!is.na(controls[["data"]][["source"]][i])){
        if(!grepl(".csv$",controls[["data"]][["source"]][i])){
          controls[["data"]][["source"]][i] = paste0(controls[["data"]][["source"]][i],".csv")
        }
        if(!file.exists(paste0(controls[["path"]],"/data/",controls[["data"]][["source"]][i]))){
          stop(paste0("File '",controls[["path"]],"/data/",controls[["data"]][["source"]][i],"' not found."))
        }
        if(!controls[["data"]][["column"]][i] %in% colnames(read.csv(file=paste0("data/",controls[["data"]][["source"]][i])))){
          stop(paste0("Column '",controls[["data"]][["column"]][i],"' not found in the file '",controls[["path"]],"data/",controls[["data"]][["source"]][i],"'."))
        }
      }
    }
  }
  
  ### return controls
  class(controls) = "fHMM_controls"
  return(controls)
}
  
#' Print method for \code{fHMM_controls}.
#' @description 
#' This function is the print method for an object of class \code{fHMM_controls}.
#' @param x
#' An object of class \code{fHMM_controls}.
#' @param ...
#' Ignored.
#' @return
#' Returns \code{x} invisibly.
#' No return value.
#' @export

print.fHMM_controls = function(x, ...) {
  cat("Controls:\n")
  cat("> path:", x[["path"]], "\n")
  cat("> model type:", x[["model"]], "\n")
  cat("> data type:", ifelse(x[["sim"]],"simulated","empirical"), "\n")
  if(x[["model"]] == "hmm") {
    cat("> number of states:", x[["states"]][1], "\n")
    cat("> SDDs:", paste0(x[["sdds"]][1],
                          ifelse(!is.na(x[["fixed_dfs"]][1]),
                                 paste0("(",x[["fixed_dfs"]][1],")"),"")),"\n")
  }
  if(x[["model"]] == "hhmm") {
    cat("> number of states:", x[["states"]][1], x[["states"]][2], "\n")
    cat("> SDDs:", paste0(x[["sdds"]][1],
                          ifelse(!is.na(x[["fixed_dfs"]][1]),
                                 paste0("(",x[["fixed_dfs"]][1],")"),"")),
        paste0(x[["sdds"]][2],
               ifelse(!is.na(x[["fixed_dfs"]][2]),
                      paste0("(",x[["fixed_dfs"]][2],")"),"")),"\n")
  }
  cat("> number of runs:", x[["fit"]][["runs"]], 
      ifelse(x[["fit"]][["at_true"]],"(initialised at true values)",""),"\n")
  if(!is.null(x[["fit"]][["seed"]])) 
    cat("> seed:",x[["fit"]][["seed"]],"\n")
}