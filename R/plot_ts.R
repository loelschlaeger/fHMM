#' Visualize time series.
#' @description  
#' This function visualizes the data time series. 
#' @param data 
#' An object of class \code{fHMM_data}.
#' @param decoding 
#' Either \code{NULL} or an object of class \code{fHMM_decoding}.
#' @param colors 
#' Either \code{NULL} or an object of class \code{fHMM_colors}. 
#' Ignored if \code{decoding = NULL}.
#' @param events 
#' Either \code{NULL} or an object of class \code{fHMM_events}.
#' @param predict
#' Either \code{NULL} or an object of class \code{fHMM_predict}.
#' @return 
#' No return value. Draws a plot to the current device.

plot_ts = function(data, decoding = NULL, colors = NULL, events = NULL, 
                   predict = NULL){
  
  plot(x = data$time_points, y = data$data, type = "h")
  
 
}