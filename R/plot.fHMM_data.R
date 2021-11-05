#' Plot method for \code{fHMM_data}.
#' @description 
#' This function is the plot method for an object of class \code{fHMM_data}.
#' @param x
#' An object of class \code{fHMM_data}.
#' @param events
#' ...
#' @param ...
#' Ignored.
#' @return
#' No return value.
#' @export

plot.fHMM_model = function(x, events = NULL, ...) {
  
  ### check input
  if(!class(x) == "fHMM_data")
    stop("'x' is not of class 'fHMM_data'.")
  if(!is.null(events))
    if(!is.list(events))
      stop("...")
  
  ### visualization
  plot_ts(data = x)
  
}