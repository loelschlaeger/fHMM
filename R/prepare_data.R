#' Prepare data for the fHMM package.
#' @description 
#' This function simulates or reads financial data for the fHMM package.
#' @param controls 
#' An object of class \code{fHMM_controls}.
#' @param true_parameters
#' An object of class \code{fHMM_parameters}, used as simulation parameters.
#' @param seed
#' Set a seed for the data simulation.
#' @return 
#' An object of class \code{fHMM_data}, which is a list containing the following 
#' elements:
#' \itemize{
#'  \item The matrix of the \code{dates} if \code{simulated = FALSE} and
#'        \code{controls$data$data_column} is specified,
#'  \item the matrix of the \code{time_points} if \code{simulated = TRUE} or
#'        \code{controls$data$data_column} is not specified,
#'  \item the matrix of the simulated \code{markov_chain} if 
#'        \code{simulated = TRUE},
#'  \item the matrix of the simulated or empirical \code{data} used for estimation,
#'  \item the matrix \code{time_series} of empirical data before the transformation
#'        to log-returns if \code{simulated = FALSE},
#'  \item the vector of fine-scale chunk sizes \code{T_star} if
#'        \code{controls$hierarchy = TRUE},
#'  \item the input \code{controls},
#'  \item the \code{true_parameters}.
#' }
#' @export

prepare_data = function(controls, true_parameters = NULL, seed = NULL) {
  
  ### check inputs
  if(class(controls) != "fHMM_controls")
    stop("'controls' is not of class 'fHMM_controls'.")
  
  ### process data
  if(controls[["simulated"]]){
    if(is.null(true_parameters))
      true_parameters = fHMM_parameters(controls, seed = seed)
    if(class(true_parameters) != "fHMM_parameters")
      stop("'true_parameters' is not of class 'fHMM_parameters'.")
    data = simulate_data(controls, true_parameters, seed = seed)
  } else {
    data = read_data(controls)
  }
  
  ### build and return object of class 'fHMM_data'
  data = list("dates" = data$dates,
              "time_points" = data$time_points,
              "markov_chain" = data$markov_chain,
              "data" = data$data,
              "time_series" = data$time_series,
              "T_star" = data$T_star,
              "controls" = controls,
              "true_parameters" = true_parameters)
  class(data) = "fHMM_data"
  return(data)
}