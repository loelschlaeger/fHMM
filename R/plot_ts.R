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
  
  if(!data$controls$hierarchy){
    
    if(data$controls$simulated){
  
      plot(x = data$time_points, y = data$data, type = "h")
      
    } else {
      
      par(las = 1, mar = c(0,1,1,1), oma = c(3,3,0,0))
      
      layout(matrix(1:2, nrow = 2))
   
      plot(x = as.Date(data$dates), y = data$time_series, type = "l",
           xaxt = "n", xlab = "", ylab = "")
      
      plot(x = as.Date(data$dates), y = data$data, type = "h", xlab = "",
           ylab = "")
      
      
      
    }
    
  } else {
    
    stop("Not implemented yet.")
     
  }
  
 
}