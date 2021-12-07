#' Plot method for an object of class \code{fHMM_model}.
#' @description
#' This function is the plot method for an object of class \code{fHMM_model}.
#' @param x
#' An object of class \code{fHMM_model}.
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
#' An object of class \code{fHMM_events}.
#' @inheritParams fHMM_colors
#' @param ...
#' Ignored.
#' @return
#' No return value. Draws a plot to the current device.
#' @export

plot.fHMM_model <- function(x, plot_type = "ts", events = NULL, colors = NULL, ...) {

  ### check input
  if (!class(x) == "fHMM_model") {
    stop("'x' is not of class 'fHMM_model'.")
  }
  plot_type <- intersect(plot_type, c("ll", "sdds", "pr", "ts"))
  if (!is.null(events)) {
    if(x$data$controls$simulated){
      warning("cannot display events")
      events = NULL
    }
    if (!is.list(events)) {
      stop("...")
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
      warning("'residuals not available.'")
    } else {
      plot_pr(x$residuals, hierarchy = x$data$controls$hierarchy)
    }
  }
  if ("ts" %in% plot_type) {
    plot_ts(
      data = x$data, decoding = x$decoding, colors = colors, events = events
    )
  }
}
