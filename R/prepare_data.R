#' Prepare data for the fHMM package.
#' @description 
#' This function simulates or reads financial data for the fHMM package.
#' @param controls 
#' An object of class \code{fHMM_controls}.
#' @param true_parameter 
#' An object of class \code{fHMM_parameters}.
#' @return 
#' An object of class \code{fHMM_data}.
#' @export

prepare_data = function(controls, true_parameter = set_parameter(controls)) {
  
  ### process data
  if(controls[["sim"]])
    data = simulate_data(controls, true_parameter)
  if(!controls[["sim"]])
    data = read_data(controls)
  
  ### check for improper use of state-dependent gamma distribution
  if(controls[["model"]]=="hmm"){
    if(controls[["sdds"]][1]=="gamma" & any(data[["data"]]<0))
      stop("C.7")
  }
  if(controls[["model"]]=="hhmm"){
    if(controls[["sdds"]][1]=="gamma" & any(data[["data"]][,1]<0))
      stop("C.7")
    if(controls[["sdds"]][2]=="gamma" & any(data[["data"]][,-1]<0,na.rm=TRUE))
      stop("C.7")
  }
  
  ### build and return object of class 'fHMM_data'
  data = data
  class(data) = "fHMM_data"
  return(data)
}