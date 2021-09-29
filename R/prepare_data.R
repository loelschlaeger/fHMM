#' Prepare data for the fHMM package.
#' @description 
#' This function simulates or reads financial data for the fHMM package.
#' @param controls 
#' An object of class \code{fHMM_controls}.
#' @param true_parameters
#' An object of class \code{fHMM_parameters}, used as simulation parameters.
#' @return 
#' An object of class \code{fHMM_data}.
#' @export

prepare_data = function(controls, true_parameters = set_parameters(controls)) {
  
  ### check inputs
  if(class(controls) != "fHMM_controls")
    stop("Not of class 'fHMM_controls'.")
  if(class(true_parameters) != "fHMM_parameters")
    stop("Not of class 'fHMM_parameters'.")
  
  ### process data
  if(controls[["sim"]]){
    data = simulate_data(controls, true_parameters)
  } else {
    data = read_data(controls)
  }
  
  ### build and return object of class 'fHMM_data'
  data = data
  class(data) = "fHMM_data"
  return(data)
}