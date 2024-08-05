#' Plot method for an object of class \code{fHMM_data}
#'
#' @description
#' This function is the plot method for an object of class \code{fHMM_data}.
#'
#' @param x
#' An object of class \code{fHMM_data}.
#' @inheritParams plot.fHMM_model
#'
#' @return
#' No return value. Draws a plot to the current device.
#' 
#' @examples 
#' plot(dax_model_3t$data, title = "DAX time series")
#'
#' @export

plot.fHMM_data <- function(
    x, events = NULL, title = NULL, from = NULL, to = NULL, ...
  ) {

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
  if (!is.null(title)) {
    if (!(is.character(title) && length(title) == 1)) {
      stop("'title' must be a single 'character' (or 'NULL').", call. = FALSE)
    }
  }

  ### visualization
  plot_ts(
    data = x, decoding = NULL, colors = NULL, events = events, title = title, 
    from = from, to = to
  )
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
#' @param title
#' Optionally a \code{character} for a custom title.
#' @inheritParams fHMM_colors
#' @inheritParams plot_ll
#' @param from
#' Optionally a \code{character}, a date in format \code{"YYYY-MM-DD"}, 
#' setting the lower date bound for plotting. 
#' By default, \code{from = NULL}, i.e. no lower bound.
#' @param to
#' Optionally a \code{character}, a date in format \code{"YYYY-MM-DD"}, 
#' setting the upper date bound for plotting. 
#' By default, \code{to = NULL}, i.e. no upper bound.
#' @param ...
#' Currently not used.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export

plot.fHMM_model <- function(
    x, plot_type = "ts", events = NULL, colors = NULL, ll_relative = TRUE, 
    title = NULL, from = NULL, to = NULL, ...
  ) {

  ### check input
  if (!inherits(x, "fHMM_model")) {
    stop("'x' is not of class 'fHMM_model'.", call. = FALSE)
  }
  plot_type <- intersect(plot_type, c("ll", "sdds", "pr", "ts"))
  if (length(plot_type) == 0) {
    stop("'plot_type' is misspecified, please see the documentation.", 
         call. = FALSE)
  }
  if (!is.null(events)) {
    if (!inherits(events, "fHMM_events")) {
      stop("'events' is not of class 'fHMM_events'.", call. = FALSE)
    }
    if (x$data$controls$simulated) {
      events <- NULL
      warning("Can't have 'events' for simulated data.", call. = FALSE)
    }
  }
  if (!is.null(title)) {
    if (!(is.character(title) && length(title) == 1)) {
      stop("'title' must be a single 'character' (or 'NULL').", call. = FALSE)
    }
  }

  ### create and check colors
  colors <- fHMM_colors(controls = x$data$controls, colors = colors)

  ### visualizations
  if ("ll" %in% plot_type) {
    plot_ll(lls = x$lls, ll_relative = ll_relative)
  }
  if ("sdds" %in% plot_type) {
    plot_sdds(
      est = parUncon2par(x$estimate, x$data$controls),
      true = x$data$true_parameters, controls = x$data$controls, 
      colors = colors
    )
  }
  if ("pr" %in% plot_type) {
    if (is.null(x$residuals)) {
      warning(
        "Residuals are not available, please call 'compute_residuals()' first.", 
        call. = FALSE
      )
    } else {
      plot_pr(
        residuals = x$residuals, hierarchy = x$data$controls$hierarchy
      )
    }
  }
  if ("ts" %in% plot_type) {
    plot_ts(
      data = x$data, decoding = x$decoding, colors = colors, events = events,
      title = title, from = from, to = to
    )
  }
}

#' Visualization of log-likelihood values
#'
#' @description
#' This function plots the log-likelihood values of the different optimization 
#' runs.
#'
#' @param lls
#' A \code{numeric} vector of log-likelihood values.
#' @param ll_relative
#' A \code{logical}, set to \code{TRUE} (default) to plot the differences from
#' the best log-likelihood value. Set to \code{FALSE} to plot the absolute 
#' values.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords internal

plot_ll <- function(lls, ll_relative = TRUE) {
  if (!isTRUE(ll_relative) && !isFALSE(ll_relative)) {
    stop("'ll_relative' must be 'TRUE' or 'FALSE'.", call. = FALSE)
  }
  max_ll_absolute <- max(lls, na.rm = TRUE)
  if (ll_relative) {
    lls <- lls - max_ll_absolute
  }
  max_ll <- max(lls, na.rm = TRUE)
  min_ll <- min(lls, na.rm = TRUE)
  main <- ifelse(ll_relative, "Relative log-likelihoods", "Log-likelihood values") 
  if (length(lls) <= 5) {
    plot(lls,
      xaxt = "n", yaxt = "n", xlab = "Estimation run", ylab = "",
      main = main, pch = 16,
      ylim = c(floor(min_ll), ceiling(max_ll))
    )
    graphics::axis(1, las = 1, at = seq_len(length(lls)), labels = seq_len(length(lls)))
  } else {
    plot(lls,
      yaxt = "n", xlab = "Estimation run", ylab = "",
      main = main, pch = 16,
      ylim = c(floor(min_ll), ceiling(max_ll))
    )
  }
  graphics::points(
    x = which.max(lls), y = lls[which.max(lls)], pch = 16, cex = 1.25,
    col = "red"
  )
  ll_unique_sorted <- unique(sort(round(lls[!is.na(lls)]), decreasing = TRUE))
  at <- ll_unique_sorted
  labels <- as.character(c(round(max_ll_absolute), ll_unique_sorted[-1]))
  if (ll_relative) {
    labels[1] <- "max"
  }
  graphics::axis(2, las = 1, at = at, labels = labels)
  abline(h = max_ll, col = "red")
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
#' @keywords internal

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
      ylim = c(floor(min(residuals, na.rm = TRUE)), 
               ceiling(max(residuals, na.rm = TRUE))),
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
      xlim = c(floor(min(residuals, na.rm = TRUE)), 
               ceiling(max(residuals, na.rm = TRUE))),
      main = "Histogram with normal density",
      xlab = "Pseudo-residuals",
      las = 1
    )
    x <- seq(floor(min(residuals, na.rm = TRUE)), 
             ceiling(max(residuals, na.rm = TRUE)), 0.01)
    graphics::curve(stats::dnorm(x), add = TRUE, lwd = 2)

    ### qq-plot
    stats::qqnorm(residuals,
      ylim = c(floor(min(residuals, na.rm = TRUE)), 
               ceiling(max(residuals, na.rm = TRUE))),
      xlim = c(floor(min(residuals, na.rm = TRUE)), 
               ceiling(max(residuals, na.rm = TRUE))),
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
    main <- "Coarse-scale (top row) and fine-scale pseudo-residuals (bottom row)"
    graphics::title(main, line = 0, outer = TRUE)
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
#' @inheritParams plot.fHMM_model
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords internal

plot_sdds <- function(est, true = NULL, controls, colors) {

  ### check input
  stopifnot(inherits(est, "fHMM_parameters"))
  stopifnot(is.null(true) || inherits(true, "fHMM_parameters"))
  stopifnot(inherits(controls, "fHMM_controls"))
  stopifnot(inherits(colors, "fHMM_colors"))

  ### reset of 'par' settings
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(suppressWarnings(graphics::par(oldpar)))
  par(las = 1)

  ### define densities
  density <- function(name, x, sigma, mu, df) {
    if (name == "t") {
      (1 / sigma) * stats::dt(x = (x - mu) / sigma, df = df)
    } else if (name == "normal") {
      stats::dnorm(x = x, mean = mu, sd = sigma)
    } else if (name == "lognormal") {
      stats::dlnorm(x = x, meanlog = mu, sdlog = sigma)
    } else if (name == "gamma") {
      stats::dgamma(x = x, shape = mu^2 / sigma^2, scale = sigma^2 / mu)
    } else if (name == "poisson") {
      stats::dpois(x = x, lambda = mu)
    } else {
      stop("Unknown state-dependent distribution", call. = FALSE)
    }
  }

  ### helper function to plot sdds
  helper_sdds <- function(
    name, nstates, colors, main, est, true = NULL, xlim_fix = NULL
  ) {

    ### define x-limits
    if (!is.null(xlim_fix)) {
      xmin <- xlim_fix[1]
      xmax <- xlim_fix[2]
    } else {
      xmin <- min(est$mu - 3 * est$sigma, na.rm = TRUE)
      xmax <- max(est$mu + 3 * est$sigma, na.rm = TRUE)
      if (!is.null(true)) {
        xmin <- min(xmin, min(true$mu - 3 * true$sigma, na.rm = TRUE), na.rm = TRUE)
        xmax <- max(xmax, max(true$mu + 3 * true$sigma, na.rm = TRUE), na.rm = TRUE)
      }
      if (name == "gamma") {
        xmin <- 0.01
      }
    }

    ### compute densities
    length.out <- 1e4
    x <- seq(from = xmin, to = xmax, length.out = length.out)
    f.x <- list()
    for (s in 1:nstates) {
      f.x[[s]] <- density(
        name = name, x = x, mu = est$mu[s], sigma = est$sigma[s],
        df = est$df[s]
      )
    }
    f.x_true <- list()
    if (!is.null(true)) {
      for (s in 1:nstates) {
        f.x_true[[s]] <- density(
          name = name, x = x, mu = true$mu[s], sigma = true$sigma[s],
          df = true$df[s]
        )
      }
    }
    ylim <- max(round(c(0, max(sapply(f.x, max))), 1), 100)

    ### define x range and initialize plot
    plot(0,
      type = "n", xlim = c(xmin, xmax),
      ylim = round(c(0, max(sapply(c(f.x, f.x_true), max))), 1),
      xlab = "", ylab = ""
    )
    title(main = main, line = 1)

    ### plot densities
    for (s in 1:nstates) {
      graphics::lines(x, f.x[[s]], col = colors[s], lty = 1, lwd = 2)
      if (!is.null(true)) {
        graphics::lines(x, f.x_true[[s]], lty = 2, col = colors[s], lwd = 2)
      }
    }

    ### add legend
    if (!is.null(true)) {
      graphics::legend(
        "topright", c("estimated", "true"), lwd = 2, lty = 1:2,
        bg = grDevices::rgb(1, 1, 1, 0.5)
      )
    }
  }

  ### plot sdds
  if (controls$hierarchy) {
    par(mar = c(2, 2, 3, 1), oma = c(2, 2, 0, 1))
    graphics::layout(
      matrix(
        c(rep(1, controls$states[1]), (1:controls$states[1]) + 1), 
        nrow = 2, byrow = TRUE
      )
    )
  }
  helper_sdds(
    name = est$sdds[[1]]$name, nstates = controls$states[1],
    colors = if (controls$hierarchy) colors[["cs"]] else colors,
    main = "State-dependent distributions",
    est = list(
      "mu" = est$mu, "sigma" = est$sigma,
      "df" = est$df
    ),
    true = if (!is.null(true)) {
      list(
        "mu" = true$mu, "sigma" = true$sigma,
        "df" = true$df
      )
    } else {
      NULL
    }
  )
  if (controls$hierarchy) {
    legend(
      legend = paste("Coarse-scale state", seq_len(controls[["states"]][1])),
      col = colors[["cs"]], pch = 20, cex = 1.25,
      x = "topleft", bg = grDevices::rgb(1, 1, 1, 0.5)
    )
    for (s in 1:controls$states[1]) {
      helper_sdds(
        name = est$sdds[[2]]$name, nstates = controls$states[2],
        colors = colors[["fs"]][s, ],
        main = paste("Conditional on coarse-scale state", s),
        est = list(
          "mu" = est$mu_star[[s]],
          "sigma" = est$sigma_star[[s]],
          "df" = est$df_star[[s]]
        ),
        true = if (!is.null(true)) {
          list(
            "mu" = true$mu_star[[s]],
            "sigma" = true$sigma_star[[s]],
            "df" = true$df_star[[s]]
          )
        } else {
          NULL
        },
        xlim_fix = c(
          min(mapply(function(x,y) x - 3*y, est$mu_star, est$sigma_star), na.rm = TRUE),
          max(mapply(function(x,y) x + 3*y, est$mu_star, est$sigma_star), na.rm = TRUE)
        )
      )
      legend(
        legend = paste("Fine-scale state", seq_len(controls[["states"]][2])),
        col = colors[["fs"]][s,], pch = 20, cex = 1.25,
        x = "topleft", bg = grDevices::rgb(1, 1, 1, 0.5)
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
#' @inheritParams plot.fHMM_model
#' 
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords internal

plot_ts <- function(
    data, decoding, colors, events = NULL, title = NULL, from = NULL, to = NULL
  ) {
  controls <- data$controls
  if (!controls[["hierarchy"]]) {
    T <- length(data[["data"]])
  }
  if (controls[["hierarchy"]]) {
    T <- dim(data[["data"]])[1]
    if (!is.null(decoding)) {
      decoding_cs <- rep(decoding[, 1], times = data[["T_star"]])
      decoding_fs <- stats::na.omit(as.vector(t(decoding[, -1])))
    }
    cs_data <- data[["data"]][, 1]
    fs_data <- stats::na.omit(as.vector(t(data[["data"]][, -1])))
  }
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(suppressWarnings(graphics::par(oldpar)))
  ### bottom, left, top, and right
  mar <- c(5.1, 5.1, 3.1, 2.1)
  if (is.null(decoding)) {
    mar[3] <- 1.1
  }
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
      ymin <- floor(min(ydata))
      ylim <- c(ymin - (ymax - ymin), ymax)
      xdata <- as.Date(data[["dates"]])
    }
    if (controls[["hierarchy"]]) {
      ydata <- as.numeric(t(data[["time_series"]][, -1, drop = FALSE]))
      ydata <- ydata[!is.na(ydata)]
      ymax <- ceiling(max(ydata))
      ymin <- floor(min(ydata))
      xdata <- as.vector(t(data[["dates"]][, -1, drop = FALSE]))
      xdata <- xdata[!is.na(xdata)]
      xdata <- as.Date(xdata)
      ylim <- c(ymin - (ymax - ymin) * 2, ymax)
    }
    if (is.null(from)) {
      xmin <- as.Date(paste0(find_closest_year(xdata[1]), "-01-01"))
    } else {
      xmin <- check_date(from)
    }
    if (is.null(to)) {
      xmax <- as.Date(paste0(find_closest_year(tail(xdata, n = 1)), "-01-01"))
    } else {
      xmax <- check_date(to)
    }
    plot(
      xdata, ydata, type = "l", xlim = c(xmin, xmax), ylim = ylim, 
      col = "black", xlab = "", ylab = "", xaxt = "n", yaxt = "n", 
      cex.lab = 2, cex.main = 2
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
    y_ticks <- signif(seq(floor(min(ydata, na.rm = TRUE)), ymax, length.out = 3), digits = 3)
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
  }
  if (!controls[["hierarchy"]]) {
    ymin <- min(data[["data"]], na.rm = TRUE)
    ymax <- max(data[["data"]], na.rm = TRUE)
    if (ymin > 0 && ymax < 10) {
      ymin <- 0
    }
  } else {
    ymin <- min(fs_data, na.rm = TRUE)
    ymax <- max(fs_data, na.rm = TRUE)
  }
  if (controls[["simulated"]]) {
    xmin <- 1
    if (!controls[["hierarchy"]]) {
      xmax <- length(data[["data"]])
      ylim <- c(ymin, ymax) 
    } else {
      xmax <- length(fs_data)
      ylim <- c(ymin - (ymax - ymin), ymax)
    }
    x_values <- seq_len(xmax)
  } else {
    x_values <- xdata
    if (!controls[["hierarchy"]]) {
      ylim <- c(ymin, ymax + (ymax - ymin)) 
    } else {
      ylim <- c(ymin - (ymax - ymin), ymax + (ymax - ymin))
    }
  }
  if (!controls[["hierarchy"]]) {
    plot(x_values, data[["data"]],
      type = "l", col = "black",
      xlab = "", ylab = "", xaxt = "n", yaxt = "n",
      xlim = c(xmin, xmax), 
      ylim = ylim
    )
  } else {
    plot(x_values, fs_data,
      type = "h", col = "black",
      xlab = "", ylab = "", xaxt = "n",
      yaxt = "n", xlim = c(xmin, xmax), 
      ylim = ylim 
    )
  }
  if (!controls[["simulated"]]) {
    if (!controls[["hierarchy"]]) {
      text <- ifelse(controls$data$logreturns, "Log-returns", "Time series data")
      mtext(text, side = 2, line = 3.5, at = 0, cex = 1.25, las = 3)
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
    axis(2, c(ymin, 0, ymax), labels = signif(c(ymin, 0, ymax), 2))
  }
  if (0 <= ymin || 0 >= ymax) {
    axis(2, c(ymin, ymax), labels = signif(c(ymin, ymax), 2))
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
      x = "topleft"
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
        paste0("Fine-scale state ", rep(1:controls[["states"]][2], each = controls[["states"]][1]))
      ),
      col = c(colors[["cs"]], as.vector(colors[["fs"]])),
      pt.lwd = c(rep(3, controls[["states"]][1]), rep(1, dim(eg)[1])),
      pch = c(rep(16, controls[["states"]][1]), rep(20, dim(eg)[1])),
      pt.cex = c(rep(2, controls[["states"]][1]), rep(2, dim(eg)[1])),
      cex = 1.25, x = "topleft", ncol = controls[["states"]][2] + 1
    )
  }
  if (controls[["hierarchy"]]) {
    par(new = TRUE)
    ymin <- min(cs_data, na.rm = TRUE)
    ymax <- max(cs_data, na.rm = TRUE)
    x_values_cs <- x_values[round(seq(1, length(x_values), length.out = T))]
    plot(x_values_cs, cs_data,
      type = "l", xlab = "", ylab = "", xaxt = "n",
      yaxt = "n", 
      xlim = c(xmin, xmax), 
      ylim = c(ymin, ymax + (ymax - ymin) * 2) 
    )
    if (is.null(decoding)) {
      points(x_values_cs, cs_data, pch = 16, cex = 1, lwd = 2)
    }
    if (!is.null(decoding)) {
      for (cs in seq_len(controls[["states"]][1])) {
        points(x_values_cs[decoding[, 1] == cs], cs_data[decoding[, 1] == cs],
          col = colors[["cs"]][cs], pch = 16, cex = 2, lwd = 2
        )
      }
    }
    if (ymin < 0 & 0 < ymax) {
      axis(4, c(ymin, 0, ymax), labels = signif(c(ymin, 0, ymax), 2))
    }
    if (0 <= ymin || 0 >= ymin) {
      axis(4, c(ymin, ymax), labels = signif(c(ymin, ymax), 2))
    }
    if (controls[["simulated"]]) {
      mtext("Simulated coarse-scale data", side = 4, line = 3.5, at = mean(c(ymin, ymax)), cex = 1.25, las = 3)
    }
    if (!controls[["simulated"]]) {
      mtext("Coarse-scale data", side = 4, line = 3.5, at = mean(c(ymin, ymax)), cex = 1.25, las = 3)
    }
  }
  if (!is.null(title)) {
    title(main = title)
  } else {
    title(main = ifelse(is.null(decoding), "Time series", "Decoded time series"))
  } 
}
