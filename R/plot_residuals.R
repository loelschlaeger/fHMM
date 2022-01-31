#' Visualize pseudo residuals.
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
  stopifnot(class(residuals) == "fHMM_residuals")

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
