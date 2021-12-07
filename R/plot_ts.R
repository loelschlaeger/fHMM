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
#' @return
#' No return value. Draws a plot to the current device.
#' @keywords
#' internal

plot_ts <- function(data, decoding = NULL, colors = NULL, events = NULL) {

  ### reset of 'par' settings
  oldpar <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(oldpar)))

  ### truncate events
  if (!is.null(events)) {
    from <- min(data$dates, na.rm = TRUE)
    to <- max(data$dates, na.rm = TRUE)
    events <- data.frame(dates = events$dates, labels = events$labels)
    events <- events[events$dates >= from & events$dates <= to, ]
    if (nrow(events) == 0) {
      events <- NULL
    }
  }

  ### helper functions
  add_events <- function() {
    if (!is.null(events)) {
      abline(v = as.Date(events$dates))
      mtext(seq_along(events$dates), at = as.Date(events$dates), line = -1, adj = 1)
    }
  }
  add_event_labels <- function() {
    if (!is.null(events)) {
      mtext(paste0(seq_along(events$dates), ": ", events$labels, collapse = "; "), line = -3)
    }
  }
  add_decoding <- function(nstates, decoding, x, y, colors) {
    for (s in seq_len(nstates)) {
      points(x = x[decoding == s], y = y[decoding == s], col = colors[s], pch = 20)
    }
  }

  ### plot
  if (!data$controls$hierarchy) {
    if (data$controls$simulated) {
      plot(
        x = data$time_points, y = data$data, type = "h", col = "grey",
        xlab = "time points", ylab = "", main = "Data time series"
      )
      if (!is.null(decoding)) {
        add_decoding(
          nstates = data$controls$states[1], decoding = decoding,
          x = data$time_points, y = data$data, colors = colors
        )
      }
    } else {
      par(mar = c(0, 1, 1, 1), oma = c(3, 3, 0, 0))
      if (is.null(events)) {
        layout(matrix(1:2, nrow = 2))
      } else {
        layout(matrix(1:3, nrow = 3), heights = c(5, 5, 1))
      }
      plot(
        x = as.Date(data$dates), y = data$time_series, type = "l",
        col = "grey",
        xaxt = "n", xlab = "", ylab = ""
      )
      if (!is.null(decoding)) {
        add_decoding(
          nstates = data$controls$states[1], decoding = decoding,
          x = as.Date(data$dates), y = data$time_series, colors = colors
        )
      }
      add_events()
      plot(
        x = as.Date(data$dates), y = data$data, type = "h", xlab = "",
        col = "grey",
        ylab = ""
      )
      if (!is.null(decoding)) {
        add_decoding(
          nstates = data$controls$states[1], decoding = decoding,
          x = as.Date(data$dates), y = data$data, colors = colors
        )
      }
      add_events()
      if (!is.null(events)) {
        plot.new()
        add_event_labels()
      }
    }
  } else {
    if (data$controls$simulated) {
      layout(matrix(1:2, ncol = 2))
      plot(
        x = data$time_points[, 1], y = data$data[, 1], type = "h", col = "grey",
        xlab = "time points", ylab = "", main = "Coarse-scale data time series"
      )
      if (!is.null(decoding)) {
        add_decoding(
          nstates = data$controls$states[1], decoding = decoding[, 1],
          x = data$time_points[, 1], y = data$data[, 1], colors = colors[, 1]
        )
      }
      plot(
        x = as.vector(t(data$time_points[, -1])), y = as.vector(t(data$data[, -1])), type = "h", col = "grey",
        xlab = "time points", ylab = "", main = "Fine-scale data time series"
      )
      if (!is.null(decoding)) {
        for (i in 1:nrow(data$data)) {
          add_decoding(
            nstates = data$controls$states[2], decoding = decoding[i, -1],
            x = data$time_points[i, -1], y = data$data[i, -1],
            colors = colors[decoding[i, 1], -1]
          )
        }
      }
    } else {
      par(mar = c(0, 2, 2, 1), oma = c(3, 2, 2, 0))
      if (is.null(events)) {
        layout(matrix(1:4, nrow = 2, ncol = 2))
      } else {
        layout(matrix(c(1, 2, 5, 3, 4, 5), nrow = 3, ncol = 2), heights = c(5, 5, 1))
      }
      plot(
        x = as.Date(data$dates[, 1]), y = data$data[, 1], type = "h", col = "grey",
        main = "Coarse-scale data time series", xaxt = "n", xlab = "", ylab = ""
      )
      if (!is.null(decoding)) {
        add_decoding(
          nstates = data$controls$states[1], decoding = decoding[, 1],
          x = as.Date(data$dates[, 1]), y = data$data[, 1], colors = colors[, 1]
        )
      }
      add_events()
      plot(
        x = as.Date(data$dates[, 1]), y = data$time_series[, 1], type = "l", col = "grey",
        xlab = "time points", ylab = "", main = ""
      )
      if (!is.null(decoding)) {
        add_decoding(
          nstates = data$controls$states[1], decoding = decoding[, 1],
          x = as.Date(data$dates[, 1]), y = data$time_series[, 1], colors = colors[, 1]
        )
      }
      add_events()
      plot(
        x = as.Date(t(data$dates[, -1])), y = as.vector(t(data$data[, -1])), type = "h", col = "grey",
        main = "Fine-scale data time series", xaxt = "n", xlab = "", ylab = ""
      )
      if (!is.null(decoding)) {
        for (i in 1:nrow(data$data)) {
          add_decoding(
            nstates = data$controls$states[2], decoding = decoding[i, -1],
            x = as.Date(data$dates[i, -1]), y = data$data[i, -1],
            colors = colors[decoding[i, 1], -1]
          )
        }
      }
      add_events()
      plot(
        x = as.Date(t(data$dates[, -1])), y = as.vector(t(data$time_series[, -1])), type = "l", col = "grey",
        xlab = "time points", ylab = "", main = ""
      )
      if (!is.null(decoding)) {
        for (i in 1:nrow(data$data)) {
          add_decoding(
            nstates = data$controls$states[2], decoding = decoding[i, -1],
            x = as.Date(data$dates[i, -1]), y = data$time_series[i, -1],
            colors = colors[decoding[i, 1], -1]
          )
        }
      }
      add_events()
      if (!is.null(events)) {
        plot.new()
        add_event_labels()
      }
    }
  }
}
