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

plot_ts <- function(data, decoding = NULL, colors = NULL, events = NULL,
                    predict = NULL) {

  ### reset of 'par' settings
  oldpar <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(oldpar)))

  ### helper functions
  add_events <- function(events, labels = FALSE) {
    abline(v = as.Date(events$dates))
    if (labels) {
      axis(3, as.Date(events$dates), seq_along(events$dates))
      mtext(paste0(seq_along(events$dates), ": ", events$labels, collapse = "; "),
        side = 3, line = 2
      )
    }
  }
  add_decoding <- function(nstates, decoding, x, y, colors) {
    for(s in seq_len(nstates)){
      points(x = x[decoding==s], y = y[decoding==s], col = colors[s], pch=20)
    }
  }
  
  ### plot
  if (!data$controls$hierarchy) {
    if (data$controls$simulated) {
      par(las = 1)

      plot(
        x = data$time_points, y = data$data, type = "h", col = "grey",
        xlab = "time points", ylab = "", main = "Data time series"
      )
      
      if(!is.null(decoding))
        add_decoding(nstates = data$controls$states[1], decoding = decoding,
                     x = data$time_points, y = data$data, colors = colors)
      
      if(!is.null(predict)){
        plotrix::plotCI(x = max(data$time_points) + seq_len(nrow(predict$data)),              
                        y = predict$data[,"estimate"],
                        li = predict$data[,"lb"],
                        ui = predict$data[,"ub"],
                        add = TRUE, gap = TRUE)
        
      }
      
    } else {
      par(las = 1, mar = c(0, 1, 1, 1), oma = c(3, 3, 0, 0))

      layout(matrix(1:2, nrow = 2))

      plot(
        x = as.Date(data$dates), y = data$time_series, type = "l",
        col = "grey",
        xaxt = "n", xlab = "", ylab = ""
      )

      add_events(events)

      plot(
        x = as.Date(data$dates), y = data$data, type = "h", xlab = "",
        col = "grey",
        ylab = ""
      )

      add_events(events, labels = TRUE)
    }
  } else {
    
    
    if (data$controls$simulated) {
      
      par(las = 1)
      
      layout(matrix(1:2, nrow = 2))

      plot(
        x = data$time_points[, 1], y = data$data[, 1], type = "h", col = "grey",
        xlab = "time points", ylab = "", main = "Coarse-scale data time series"
      )

      plot(
        x = data$time_points[, -1], y = data$data[, -1], type = "h", col = "grey",
        xlab = "time points", ylab = "", main = "Fine-scale data time series"
      )
    } else {
      
      par(las = 1, mar = c(0, 1, 1, 1), oma = c(3, 3, 0, 0))
      
      layout(matrix(1:4, nrow = 2, ncol = 2))

      plot(
        x = as.Date(data$dates[,1]), y = data$data[, 1], type = "h", col = "grey",
        xlab = "time points", ylab = "", main = "Coarse-scale data time series"
      )
      
      add_events(events)
      
      plot(
        x = as.Date(data$dates[,-1]), y = data$data[, -1], type = "h", col = "grey",
        xlab = "time points", ylab = "", main = "Fine-scale data time series"
      )
      
      add_events(events, labels = FALSE)
      
      plot(
        x = as.Date(data$dates[,1]), y = data$time_series[, 1], type = "l", col = "grey",
        xlab = "time points", ylab = "", main = "Coarse-scale data time series"
      )
      
      add_events(events)
      
      plot(
        x = as.Date(data$dates[,-1]), y = data$time_series[, -1], type = "l", col = "grey",
        xlab = "time points", ylab = "", main = "Fine-scale data time series"
      )
      
      add_events(events, labels = FALSE)
    }
  }
}
