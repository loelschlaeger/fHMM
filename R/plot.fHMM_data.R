#' Plot method for an object of class \code{fHMM_data}.
#' @description 
#' This function is the plot method for an object of class \code{fHMM_data}.
#' @param x
#' An object of class \code{fHMM_data}.
#' @param events
#' Either \code{NULL} or an object of class \code{fHMM_events}.
#' @param ...
#' Ignored.
#' @return
#' No return value.
#' @export

plot.fHMM_data = function(x, events = NULL, ...) {
  
  ### check input
  if(!class(x) == "fHMM_data")
    stop("'x' is not of class 'fHMM_data'.")
  if(!is.null(events))
    if(!class(events) == "fHMM_events")
      stop("'events' is not of class 'fHMM_events'.")
  
  ### visualization
  plot_ts(data = x, decoding = NULL, colors = NULL, events = events, 
          predict = NULL)
  
}