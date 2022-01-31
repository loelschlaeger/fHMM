#' Plot method for an object of class \code{fHMM_data}.
#' 
#' @description
#' This function is the plot method for an object of class \code{fHMM_data}.
#' 
#' @param x
#' An object of class \code{fHMM_data}.
#' @param events
#' Either \code{NULL} or an object of class \code{fHMM_events}.
#' @param ...
#' Ignored.
#' 
#' @return
#' No return value. Draws a plot to the current device.
#' 
#' @export

plot.fHMM_data <- function(x, events = NULL, ...) {

  ### check input
  if (!class(x) == "fHMM_data") {
    stop("'x' is not of class 'fHMM_data'.")
  }
  if (!is.null(events)) {
    if (!class(events) == "fHMM_events") {
      stop("'events' is not of class 'fHMM_events'.")
    }
    if(x$controls$simulated){
      events = NULL
      warning("Can't have 'events' for simulated data.")
    }
  }

  ### visualization
  plot_ts(
    data = x, decoding = NULL, colors = NULL, events = events
  )
}
