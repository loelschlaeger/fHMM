#' Plot method for an object of class \code{fHMM_data}
#'
#' @description
#' This function is the plot method for an object of class \code{fHMM_data}.
#'
#' @param x
#' An object of class \code{fHMM_data}.
#' @param events
#' Either \code{NULL} or an object of class \code{\link{fHMM_events}}.
#' @param ...
#' Ignored.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export

plot.fHMM_data <- function(x, events = NULL, ...) {

  ### check input
  if (!inherits(x, "fHMM_data")) {
    stop("'x' is not of class 'fHMM_data'.", call. = FALSE)
  }
  if (!is.null(events)) {
    if (!inherits(events, "fHMM_events")) {
      stop("'events' is not of class 'fHMM_events'.", call. = FALSE)
    }
    if (x$controls$simulated) {
      events <- NULL
      warning("Can't have 'events' for simulated data.", call. = FALSE)
    }
  }

  ### visualization
  plot_ts(data = x, decoding = NULL, colors = NULL, events = events)
}


#' Plot method for an object of class \code{\link{fHMM_model}}
#'
#' @description
#' This function is the plot method for an object of class 
#' \code{\link{fHMM_model}}.
#'
#' @param x
#' An object of class \code{\link{fHMM_model}}.
#' @param plot_type
#' A character (vector), specifying the type of plot and can be one (or more) of
#' \itemize{
#'   \item \code{"ll"} for a visualization of the likelihood values in the
#'         different optimization runs,
#'   \item \code{"sdds"} for a visualization of the estimated state-dependent
#'         distributions,
#'   \item \code{"pr"} for a visualization of the model's (pseudo-) residuals,
#'   \item \code{"ts"} for a visualization of the financial time series.
#' }
#' @param events
#' An object of class \code{\link{fHMM_events}}.
#' @inheritParams fHMM_colors
#' @param ...
#' Ignored.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export

plot.fHMM_model <- function(x, plot_type = "ts", events = NULL, colors = NULL, ...) {

  ### check input
  if (!inherits(x, "fHMM_model")) {
    stop("'x' is not of class 'fHMM_model'.", call. = FALSE)
  }
  plot_type <- intersect(plot_type, c("ll", "sdds", "pr", "ts"))
  if (!is.null(events)) {
    if (!inherits(events, "fHMM_events")) {
      stop("'events' is not of class 'fHMM_events'.", call. = FALSE)
    }
    if (x$data$controls$simulated) {
      events <- NULL
      warning("Can't have 'events' for simulated data.", call. = FALSE)
    }
  }

  ### create and check colors
  colors <- fHMM_colors(controls = x$data$controls, colors = colors)

  ### visualizations
  if ("ll" %in% plot_type) {
    plot_ll(lls = x$lls)
  }
  if ("sdds" %in% plot_type) {
    plot_sdds(
      est = parUncon2par(x$estimate, x$data$controls),
      true = x$data$true_parameters, controls = x$data$controls, colors = colors
    )
  }
  if ("pr" %in% plot_type) {
    if (is.null(x$residuals)) {
      warning("Residuals are not available, please call 'compute_residuals()' first.")
    } else {
      plot_pr(residuals = x$residuals, hierarchy = x$data$controls$hierarchy)
    }
  }
  if ("ts" %in% plot_type) {
    plot_ts(
      data = x$data, decoding = x$decoding, colors = colors, events = events
    )
  }
}

#' Visualization of log-likelihood values
#'
#' @description
#' This function plots the log-likelihood values of the different estimation runs.
#'
#' @param lls
#' A numeric vector of log-likelihood values.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal
#'
#' @importFrom graphics axis points

plot_ll <- function(lls) {
  if (length(lls) <= 5) {
    plot(lls,
      xaxt = "n", yaxt = "n", xlab = "Estimation run", ylab = "",
      main = "Log-likelihoods", pch = 16,
      ylim = c(floor(min(lls, na.rm = TRUE)), ceiling(max(lls, na.rm = TRUE)))
    )
    graphics::axis(1, las = 1, at = seq_len(length(lls)), labels = seq_len(length(lls)))
  } else {
    plot(lls,
      yaxt = "n", xlab = "Estimation run", ylab = "",
      main = "Log-likelihoods", pch = 16,
      ylim = c(floor(min(lls, na.rm = TRUE)), ceiling(max(lls, na.rm = TRUE)))
    )
  }
  graphics::points(
    x = which.max(lls), y = lls[which.max(lls)], pch = 16, cex = 1.25,
    col = "red"
  )
  graphics::axis(2,
    las = 1, at = unique(round(lls[!is.na(lls)])),
    labels = unique(round(lls[!is.na(lls)]))
  )
}

#' Visualize pseudo residuals
#'
#' @description
#' This function visualizes the pseudo residuals.
#'
#' @param residuals
#' An object of class \code{fHMM_residuals}.
#' @param hierarchy
#' The element \code{controls$hierarchy}.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal
#'
#' @importFrom graphics hist curve abline layout mtext title
#' @importFrom stats dnorm qqnorm acf

plot_pr <- function(residuals, hierarchy) {

  ### check input
  stopifnot(inherits(residuals, "fHMM_residuals"))

  ### reset of 'par' settings
  oldpar <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(oldpar)))

  ### define helper function for plotting residuals
  helper_pr <- function(residuals) {

    ### remove bad residuals
    residuals <- residuals[!is.na(residuals) & is.finite(residuals)]

    ### residual plot
    plot(residuals,
      ylim = c(floor(min(residuals)), ceiling(max(residuals))),
      main = "Residual plot",
      ylab = "Pseudo-residuals",
      las = 1,
      pch = 3
    )

    ### histogram with normal density
    graphics::hist(residuals,
      freq = FALSE,
      breaks = 25,
      col = "lightgrey",
      xlim = c(floor(min(residuals)), ceiling(max(residuals))),
      main = "Histogram with normal density",
      xlab = "Pseudo-residuals",
      las = 1
    )
    x <- seq(floor(min(residuals)), ceiling(max(residuals)), 0.01)
    graphics::curve(stats::dnorm(x), add = TRUE, lwd = 2)

    ### qq-plot
    stats::qqnorm(residuals,
      ylim = c(floor(min(residuals)), ceiling(max(residuals))),
      xlim = c(floor(min(residuals)), ceiling(max(residuals))),
      main = "Normal Q-Q plot",
      ylab = "Quantiles of pseudo-residuals",
      xlab = "Normal quantiles",
      las = 1,
      pch = 20
    )
    graphics::abline(a = 0, b = 1)

    ### acf plot
    stats::acf(residuals,
      main = "Autocorrelation plot",
      ylab = "Autocorrelation of pseudo-residuals",
      xlab = "Lag",
      las = 1
    )
  }

  ### create plots
  if (!hierarchy) {
    graphics::layout(matrix(1:4, 2, 2))
    helper_pr(residuals)
  } else {
    oma <- c(0, 0, 1, 0) ### bottom, left, top, and right
    par(oma = oma, bty = "n")
    graphics::layout(matrix(1:8, 2, 4, byrow = TRUE))
    helper_pr(residuals = residuals[, 1])
    graphics::title("Coarse-scale (top row) and fine-scale pseudo-residuals (bottom row)", line = 0, outer = TRUE)
    helper_pr(residuals = as.vector(residuals[, -1]))
  }
}

#' Visualization of estimated state-dependent distributions
#'
#' @description
#' This function plots the estimated state-dependent distributions.
#'
#' @param est
#' An object of class \code{fHMM_parameters} with estimated parameters.
#' @param true
#' Either \code{NULL} or an object of class \code{fHMM_parameters} with true
#' parameters.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @param colors
#' An object of class \code{fHMM_colors}.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords internal
#'
#' @importFrom graphics par lines legend layout
#' @importFrom stats dt dgamma

plot_sdds <- function(est, true = NULL, controls, colors) {

  ### check input
  stopifnot(inherits(est, "fHMM_parameters"))
  stopifnot(is.null(true) || inherits(true, "fHMM_parameters"))
  stopifnot(inherits(controls, "fHMM_controls"))
  stopifnot(inherits(colors, "fHMM_colors"))

  ### reset of 'par' settings
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(suppressWarnings(graphics::par(oldpar)))

  ### define densities
  density <- function(name, x, sigma, mu, df) {
    if (name == "t") {
      (1 / sigma) * stats::dt(x = (x - mu) / sigma, df = df)
    } else if (name == "lnorm") {
      stats::dlnorm(x = x, meanlog = mu, sdlog = sigma)
    } else if (name == "gamma") {
      stats::dgamma(x = x, shape = mu^2 / sigma^2, scale = sigma^2 / mu)
    }
  }

  ### helper function to plot sdds
  helper_sdds <- function(name, nstates, colors, main, est, true = NULL) {

    ### define x-limits
    xmin <- min(est$mus - 3 * est$sigmas)
    xmax <- max(est$mus + 3 * est$sigmas)
    if (!is.null(true)) {
      xmin <- min(xmin, min(true$mus - 3 * true$sigmas))
      xmax <- max(xmax, max(true$mus + 3 * true$sigmas))
    }
    if (name == "gamma") {
      xmin <- 0.01
    }

    ### compute densities
    length.out <- 1e4
    x <- seq(from = xmin, to = xmax, length.out = length.out)
    f.x <- list()
    for (s in 1:nstates) {
      f.x[[s]] <- density(
        name = name, x = x, mu = est$mus[s], sigma = est$sigmas[s],
        df = est$dfs[s]
      )
    }
    f.x_true <- list()
    if (!is.null(true)) {
      for (s in 1:nstates) {
        f.x_true[[s]] <- density(
          name = name, x = x, mu = true$mus[s], sigma = true$sigmas[s],
          df = true$dfs[s]
        )
      }
    }
    ylim <- max(round(c(0, max(sapply(f.x, max))), 1), 100)

    ### define x range and initialize plot
    plot(0,
      type = "n", xlim = c(xmin, xmax),
      ylim = round(c(0, max(sapply(c(f.x, f.x_true), max))), 1),
      xlab = "", ylab = "", main = main
    )

    ### plot densities
    for (s in 1:nstates) {
      graphics::lines(x, f.x[[s]], col = colors[s], lty = 1, lwd = 2)
      if (!is.null(true)) {
        graphics::lines(x, f.x_true[[s]], lty = 2, col = colors[s], lwd = 2)
      }
    }

    ### add legend
    if (!is.null(true)) {
      graphics::legend("topright", c("estimated", "true"), lwd = 2, lty = 1:2)
    }
  }

  ### plot sdds
  if (controls$hierarchy) {
    graphics::layout(matrix(c(rep(1, controls$states[1]), (1:controls$states[1]) + 1),
      nrow = 2, byrow = TRUE
    ))
  }
  helper_sdds(
    name = est$sdds[[1]]$name, nstates = controls$states[1],
    colors = if (controls$hierarchy) colors[["cs"]] else colors,
    main = "State-dependent distributions",
    est = list(
      "mus" = est$mus, "sigmas" = est$sigmas,
      "dfs" = est$dfs
    ),
    true = if (!is.null(true)) {
      list(
        "mus" = true$mus, "sigmas" = true$sigmas,
        "dfs" = true$dfs
      )
    } else {
      NULL
    }
  )
  if (controls$hierarchy) {
    for (s in 1:controls$states[1]) {
      helper_sdds(
        name = est$sdds[[2]]$name, nstates = controls$states[2],
        colors = colors[["fs"]][s, ],
        main = paste("Coarse-scale state", s),
        est = list(
          "mus" = est$mus_star[[s]],
          "sigmas" = est$sigmas_star[[s]],
          "dfs" = est$dfs_star[[s]]
        ),
        true = if (!is.null(true)) {
          list(
            "mus" = true$mus_star[[s]],
            "sigmas" = true$sigmas_star[[s]],
            "dfs" = true$dfs_star[[s]]
          )
        } else {
          NULL
        }
      )
    }
  }
}

#' Visualize time series
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
#' Either \code{NULL} or an object of class \code{\link{fHMM_events}}.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords internal
#'
#' @importFrom graphics par abline mtext points layout plot.new text
#' @importFrom grDevices rgb

plot_ts <- function(data, decoding, colors, events) {
  controls <- data$controls
  if (!controls[["hierarchy"]]) {
    T <- length(data[["data"]])
  }
  if (controls[["hierarchy"]]) {
    T <- dim(data[["data"]])[1]
    if (!is.null(decoding)) {
      decoding_cs <- rep(decoding[, 1], times = data[["T_star"]])
      decoding_fs <- as.vector(t(decoding[, -1]))[!is.na(as.vector(t(decoding[, -1])))]
    }
    cs_data <- data[["data"]][, 1]
    fs_data <- as.vector(t(data[["data"]][, -1]))[!is.na(as.vector(t(data[["data"]][, -1])))]
  }
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(suppressWarnings(graphics::par(oldpar)))
  mar <- c(5.1, 5.1, 3.1, 2.1) ### bottom, left, top, and right
  if (!is.null(events)) {
    ### add margin at the bottom for events
    mar <- mar + c(2, 0, 0, 0)
  }
  if (!controls[["simulated"]] || controls[["hierarchy"]]) {
    ### add margin at the right for second axis
    mar <- mar + c(0, 0, 0, 4)
  }
  par(las = 1, mar = mar, bty = "n")
  if (!controls[["simulated"]]) {
    if (!controls[["hierarchy"]]) {
      ydata <- data[["time_series"]]
      ymax <- ceiling(max(ydata))
      xdata <- as.Date(data[["dates"]])
    }
    if (controls[["hierarchy"]]) {
      ydata <- as.numeric(t(data[["time_series"]][, -1, drop = FALSE]))
      ydata <- ydata[!is.na(ydata)]
      ymax <- ceiling(max(ydata))
      xdata <- as.vector(t(data[["dates"]][, -1, drop = FALSE]))
      xdata <- xdata[!is.na(xdata)]
      xdata <- as.Date(xdata)
    }
    xmin <- as.Date(format(as.Date(head(xdata, n = 1)), "%Y-01-01"))
    xmax <- as.Date(paste0(as.numeric(format(as.Date(tail(xdata, n = 1)), "%Y")) + 1, "-01-01"))
    ymin <- -ymax
    plot(xdata, ydata,
      type = "l",
      xlim = c(xmin, xmax), ylim = c(1.2 * ymin, 1.2 * ymax), col = "lightgrey", xlab = "",
      ylab = "", xaxt = "n", yaxt = "n", cex.lab = 2, cex.main = 2
    )
    if (!controls[["hierarchy"]]) {
      data_lab <- controls[["data"]][["data_column"]][1]
    }
    if (controls[["hierarchy"]]) {
      data_lab <- controls[["data"]][["data_column"]][2]
    }
    mtext("Year", side = 1, line = 2.5, cex = 1.25)
    markdates <- seq(xmin, xmax, by = "year")
    markdates <- markdates[1:length(markdates) %% 2 == 1]
    axis(1, markdates, format(markdates, "%Y"))
    y_ticks <- signif(seq(floor(min(ydata)), ymax, length.out = 3), digits = 3)
    axis(4, y_ticks)
    mtext(data_lab,
      side = 4, line = 3.5, at = mean(y_ticks),
      cex = 1.25, las = 3
    )
    if (!controls[["hierarchy"]]) {
      if (!is.null(decoding)) {
        for (s in seq_len(controls[["states"]][1])) {
          points(xdata[decoding == s], ydata[decoding == s], col = colors[s], pch = 20)
        }
      }
    }
    if (controls[["hierarchy"]]) {
      if (!is.null(decoding)) {
        for (cs in seq_len(controls[["states"]][1])) {
          for (fs in seq_len(controls[["states"]][2])) {
            points(xdata[decoding_cs == cs & decoding_fs == fs],
              ydata[decoding_cs == cs & decoding_fs == fs],
              col = colors[["fs"]][cs, fs], pch = 20
            )
          }
        }
      }
    }
    par(new = TRUE, las = 1)
    ymax_factor <- 3
  }
  if (controls[["simulated"]]) {
    xmin <- 1
    if (!controls[["hierarchy"]]) {
      xmax <- length(data[["data"]])
    }
    if (controls[["hierarchy"]]) {
      xmax <- length(fs_data)
    }
    x_values <- seq_len(xmax)
    ymax_factor <- 1
  }
  if (!controls[["simulated"]]) {
    x_values <- xdata
  }
  if (!controls[["hierarchy"]]) {
    ymin <- min(data[["data"]])
    ymax <- max(data[["data"]])
    if (ymin > 0 && ymax < 10) {
      ymin <- 0
    }
    plot(x_values, data[["data"]],
      type = "l", col = "lightgrey",
      xlab = "", ylab = "", xaxt = "n", yaxt = "n",
      xlim = c(xmin, xmax), ylim = c(ymin, ymax * ymax_factor)
    )
  }
  if (controls[["hierarchy"]]) {
    ymin <- min(fs_data)
    ymax <- max(fs_data)
    plot(x_values, fs_data,
      type = "h", col = "lightgrey",
      xlab = "", ylab = "", xaxt = "n",
      yaxt = "n", xlim = c(xmin, xmax), ylim = c(ymin, ymax * ymax_factor)
    )
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
      mtext("Simulated data",
        side = 2, line = 3.5,
        cex = 1.25, las = 3, at = mean(c(ymin, ymax))
      )
    }
    if (controls[["hierarchy"]]) {
      mtext("Simulated fine-scale data",
        side = 2,
        line = 3.5, cex = 1.25, las = 3, at = mean(c(ymin, ymax))
      )
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
        points(x_values[decoding_cs == cs & decoding_fs == fs], fs_data[decoding_cs == cs & decoding_fs == fs], col = colors[["fs"]][cs, fs], pch = 20)
      }
    }
  }
  if (!controls[["simulated"]] & !is.null(events)) {
    events[["labels"]] <- events[["labels"]][events[["dates"]] > xmin & events[["dates"]] < xmax]
    events[["dates"]] <- events[["dates"]][events[["dates"]] > xmin & events[["dates"]] < xmax]
    if (length(events[["dates"]]) == 0) {
      warning("No events fall in the considered time period.", call. = FALSE)
    } else {
      for (l in seq_len(length(events[["dates"]]))) {
        if (events[["dates"]][l] > xmin & events[["dates"]][l] < xmax) {
          abline(v = as.Date(events[["dates"]][l]))
          graphics::text(x = as.Date(events[["dates"]][l]), y = ymin, labels = l, pos = 2, cex = 1.25)
        }
      }
      mtext(paste0(seq_len(length(events[["labels"]])), ": ", events[["labels"]], collapse = "   "),
        side = 1, line = 4, cex = 1.25
      )
    }
  }
  if (!controls[["hierarchy"]] & !is.null(decoding)) {
    legend(
      legend = paste("State", seq_len(controls[["states"]][1])),
      col = colors, pch = 20, cex = 1.25,
      x = "topleft", bg = grDevices::rgb(1, 1, 1, 0.5)
    )
  }
  if (controls[["hierarchy"]] & !is.null(decoding)) {
    eg <- expand.grid(
      seq_len(controls[["states"]][2]),
      seq_len(controls[["states"]][1])
    )
    legend(
      legend = c(
        paste("Coarse-scale state", seq_len(controls[["states"]][1])),
        paste0("Fine-scale state ", eg[, 1], " in coarse-scale state ", eg[, 2])
      ),
      col = c(colors[["cs"]], as.vector(unlist(colors[["fs"]]))),
      pt.lwd = c(rep(3, controls[["states"]][1]), rep(1, dim(eg)[1])),
      pch = c(rep(1, controls[["states"]][1]), rep(20, dim(eg)[1])),
      pt.cex = c(rep(3, controls[["states"]][1]), rep(2, dim(eg)[1])),
      cex = 1.25, bg = grDevices::rgb(1, 1, 1, 0.5), x = "topleft"
    )
  }
  if (controls[["hierarchy"]]) {
    par(new = TRUE)
    ymin <- min(cs_data)
    ymax <- max(cs_data)
    x_values_cs <- x_values[round(seq(1, length(x_values), length.out = T))]
    plot(x_values_cs, cs_data,
      type = "c", xlab = "", ylab = "", xaxt = "n",
      yaxt = "n", xlim = c(xmin, xmax), ylim = c(ymin, ymax * ymax_factor * 1.5)
    )
    if (is.null(decoding)) {
      points(x_values_cs, cs_data, pch = 1, cex = 3, lwd = 2)
    }
    if (!is.null(decoding)) {
      for (cs in seq_len(controls[["states"]][1])) {
        points(x_values_cs[decoding[, 1] == cs], cs_data[decoding[, 1] == cs],
          col = colors[["cs"]][cs], pch = 1, cex = 3, lwd = 2
        )
      }
    }
    if (ymin < 0 & 0 < ymax) {
      axis(4, c(ymin, 0, ymax), labels = sprintf("%.2g", c(ymin, 0, ymax)))
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
