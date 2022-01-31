#' Visualize time series.
#' 
#' @description
#' This function visualizes the data time series.
#' 
#' @param data
#' An object of class \code{fHMM_data}.
#' @param decoding
#' Either \code{NULL} or an object of class \code{fHMM_decoding}.
#' @param colors
#' Either \code{NULL} or an object of class \code{fHMM_colors}.
#' Ignored if \code{decoding = NULL}.
#' @param events
#' Either \code{NULL} or an object of class \code{fHMM_events}.
#' 
#' @return
#' No return value. Draws a plot to the current device.
#' 
#' @keywords
#' internal
#' 
#' @importFrom graphics par abline mtext points layout plot.new text
#' @importFrom grDevices rgb

plot_ts <- function (data, decoding, colors, events) {
  controls <- data$controls
  if (!controls[["hierarchy"]]) {
    T = length(data[["data"]])
  }
  if (controls[["hierarchy"]]) {
    T = dim(data[["data"]])[1]
    if(!is.null(decoding)){
      decoding_cs = rep(decoding[, 1], times = data[["T_star"]])
      decoding_fs = as.vector(t(decoding[, -1]))[!is.na(as.vector(t(decoding[,-1])))]
    }
    cs_data = data[["data"]][, 1]
    fs_data = as.vector(t(data[["data"]][, -1]))[!is.na(as.vector(t(data[["data"]][,-1])))]
  }
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(suppressWarnings(graphics::par(oldpar)))
  mar <- c(5.1, 5.1, 3.1, 2.1) ### bottom, left, top, and right
  if(!is.null(events)){
    ### add margin at the bottom for events
    mar <- mar + c(2,0,0,0)
  }
  if (!controls[["simulated"]] || controls[["hierarchy"]]){
    ### add margin at the right for second axis
    mar <- mar + c(0,0,0,4)
  }
  par(las = 1, mar = mar, bty = "n")
  if (!controls[["simulated"]]) {
    if (!controls[["hierarchy"]]) {
      ydata = data[["time_series"]]
      ymax = ceiling(max(ydata))
      xdata = as.Date(data[["dates"]])
    }
    if (controls[["hierarchy"]]) {
      ydata = as.numeric(t(data[["time_series"]][, -1, drop = FALSE]))
      ydata = ydata[!is.na(ydata)]
      ymax = ceiling(max(ydata))
      xdata = as.vector(t(data[["dates"]][, -1, drop = FALSE]))
      xdata = xdata[!is.na(xdata)]
      xdata = as.Date(xdata)
    }
    xmin = as.Date(format(as.Date(head(xdata, n = 1)), "%Y-01-01"))
    xmax = as.Date(paste0(as.numeric(format(as.Date(tail(xdata, n = 1)), "%Y")) + 1, "-01-01")) 
    ymin = -ymax
    plot(xdata, ydata, type = "l", 
         xlim = c(xmin, xmax), ylim = c(1.2 * ymin, 1.2 * ymax), col = "lightgrey", xlab = "", 
         ylab = "", xaxt = "n", yaxt = "n", cex.lab = 2, cex.main = 2)
    if (!controls[["hierarchy"]]) {
      data_lab = controls[["data"]][["data_column"]][1]
    }
    if (controls[["hierarchy"]]) {
      data_lab = controls[["data"]][["data_column"]][2]
    }
    mtext("Year", side = 1, line = 2.5, cex = 1.25)
    markdates = seq(xmin, xmax, by = "year")
    markdates = markdates[1:length(markdates)%%2 == 1]
    axis(1, markdates, format(markdates, "%Y"))
    y_ticks = signif(seq(floor(min(ydata)), ymax, length.out = 3), digits = 3)
    axis(4, y_ticks)
    mtext(data_lab, side = 4, line = 3.5, at = mean(y_ticks), 
          cex = 1.25, las = 3)
    if (!controls[["hierarchy"]]) {
      if(!is.null(decoding)) {
        for (s in seq_len(controls[["states"]][1])) {
          points(xdata[decoding == s], ydata[decoding == s], col = colors[s], pch = 20)
        }
      }
    }
    if (controls[["hierarchy"]]) {
      if(!is.null(decoding)){
        for (cs in seq_len(controls[["states"]][1])) {
          for (fs in seq_len(controls[["states"]][2])) {
            points(xdata[decoding_cs == cs & decoding_fs == fs], 
                   ydata[decoding_cs == cs & decoding_fs == fs], col = colors[["fs"]][cs,fs], pch = 20)
          }
        }
      }
    }
    par(new = TRUE, las = 1)
    ymax_factor = 3
  }
  if (controls[["simulated"]]) {
    xmin = 1
    if (!controls[["hierarchy"]]) {
      xmax = length(data[["data"]])
    }
    if (controls[["hierarchy"]]) {
      xmax = length(fs_data)
    }
    x_values = seq_len(xmax)
    ymax_factor = 1
  }
  if(!controls[["simulated"]]){
    x_values = xdata
  }
  if (!controls[["hierarchy"]]) {
    ymin = min(data[["data"]])
    ymax = max(data[["data"]])
    plot(x_values, data[["data"]], type = "h", col = "lightgrey", 
         xlab = "", ylab = "", xaxt = "n", yaxt = "n", 
         xlim = c(xmin, xmax), ylim = c(ymin, ymax * ymax_factor))
  }
  if (controls[["hierarchy"]]) {
    ymin = min(fs_data)
    ymax = max(fs_data)
    plot(x_values, fs_data, type = "h", col = "lightgrey", 
         xlab = "", ylab = "", xaxt = "n", 
         yaxt = "n", xlim = c(xmin, xmax), ylim = c(ymin, ymax * ymax_factor))
  }
  if (!controls[["simulated"]]) {
    if (!controls[["hierarchy"]]) {
      mtext("Time series data", side = 2, line = 3.5, at = 0, cex = 1.25, las = 3)
    }
    if (controls[["hierarchy"]]) {
      mtext("Fine-scale data", side = 2, line = 3.5, at = 0, cex = 1.25, las = 3)
    }
  }
  if (controls[["simulated"]]) {
    mtext("Index", side = 1, line = 2.5, cex = 1.25)
    if (!controls[["hierarchy"]]) {
      mtext("Simulated data", side = 2, line = 3.5, 
            cex = 1.25, las = 3, at = mean(c(ymin, ymax)))
    }
    if (controls[["hierarchy"]]) {
      mtext("Simulated fine-scale data", side = 2, 
            line = 3.5, cex = 1.25, las = 3, at = mean(c(ymin, ymax)))
    }
    axis(1, c(xmin, xmax))
  }
  if (ymin < 0 & 0 < ymax) {
    axis(2, c(ymin, 0, ymax), labels = sprintf("%.2g", c(ymin, 0, ymax)))
  }
  if (0 <= ymin || 0 >= ymax) {
    axis(2, c(ymin, ymax), labels = sprintf("%.2g", c(ymin, ymax)))
  }
  if (!controls[["hierarchy"]] & !is.null(decoding)) {
    for (s in seq_len(controls[["states"]][1])) {
      points(x_values[decoding == s], data[["data"]][decoding == s], col = colors[s], pch = 20)
    }
  }
  if (controls[["hierarchy"]] & !is.null(decoding)) {
    for (cs in seq_len(controls[["states"]][1])) {
      for (fs in seq_len(controls[["states"]][2])) {
        points(x_values[decoding_cs == cs & decoding_fs == fs], fs_data[decoding_cs == cs & decoding_fs == fs], col = colors[["fs"]][cs,fs], pch = 20)
      }
    }
  }
  if (!controls[["simulated"]] & !is.null(events)) {
    events[["labels"]] = events[["labels"]][events[["dates"]] > xmin & events[["dates"]] < xmax]
    events[["dates"]] = events[["dates"]][events[["dates"]] > xmin & events[["dates"]] < xmax]
    if(length(events[["dates"]]) == 0){
      warning("No events fall in the considered time period.") 
    } else {
      for (l in seq_len(length(events[["dates"]]))) {
        if (events[["dates"]][l] > xmin & events[["dates"]][l] < xmax) {
          abline(v = as.Date(events[["dates"]][l]))
          graphics::text(x = as.Date(events[["dates"]][l]), y = ymin, labels = l, pos = 2, cex = 1.25)
        }
      }
      mtext(paste0(seq_len(length(events[["labels"]])), ": ", events[["labels"]], collapse = "   "), 
            side = 1, line = 4, cex = 1.25)
    }
  }
  if (!controls[["hierarchy"]] & !is.null(decoding)) {
    legend(legend = paste("State", seq_len(controls[["states"]][1])), 
           col = colors, pch = 20, cex = 1.25, 
           x = "topleft", bg = grDevices::rgb(1, 1, 1, 0.5))
  }
  if (controls[["hierarchy"]] & !is.null(decoding)) {
    eg = expand.grid(seq_len(controls[["states"]][2]), 
                     seq_len(controls[["states"]][1]))
    legend(legend = c(paste("Coarse-scale state", seq_len(controls[["states"]][1])), 
                      paste0("Fine-scale state ", eg[, 1], " in coarse-scale state ", eg[, 2])), 
           col = c(colors[["cs"]], as.vector(unlist(colors[["fs"]]))), 
           pt.lwd = c(rep(3, controls[["states"]][1]), rep(1, dim(eg)[1])), 
           pch = c(rep(1, controls[["states"]][1]), rep(20, dim(eg)[1])), 
           pt.cex = c(rep(3, controls[["states"]][1]), rep(2, dim(eg)[1])), 
           cex = 1.25, bg = grDevices::rgb(1, 1, 1, 0.5), x = "topleft")
  }
  if (controls[["hierarchy"]]) {
    par(new = TRUE)
    ymin = min(cs_data)
    ymax = max(cs_data)
    x_values_cs = x_values[round(seq(1, length(x_values), length.out = T))]
    plot(x_values_cs, cs_data, type = "c", xlab = "", ylab = "", xaxt = "n", 
         yaxt = "n", xlim = c(xmin, xmax), ylim = c(ymin, ymax * ymax_factor * 1.5))
    if(is.null(decoding)){
      points(x_values_cs, cs_data, pch = 1, cex = 3, lwd = 2)
    }
    if(!is.null(decoding)){
      for (cs in seq_len(controls[["states"]][1])) {
        points(x_values_cs[decoding[, 1] == cs], cs_data[decoding[,1] == cs], 
               col = colors[["cs"]][cs], pch = 1, cex = 3, lwd = 2)
      }
    }
    if (ymin < 0 & 0 < ymax) {
      axis(4, c(ymin, 0, ymax), labels = sprintf("%.2g",  c(ymin, 0, ymax)))
    }
    if (0 <= ymin || 0 >= ymin) {
      axis(4, c(ymin, ymax), labels = sprintf("%.2g", c(ymin, ymax)))
    }
    if (controls[["simulated"]]) {
      mtext("Simulated coarse-scale data", side = 4, line = 3.5, at = mean(c(ymin, ymax)), cex = 1.25, las = 3)
    }
    if (!controls[["simulated"]]) {
      mtext("Coarse-scale data", side = 4, line = 3.5, at = mean(c(ymin, ymax)), cex = 1.25, las = 3)
    }
  }
}