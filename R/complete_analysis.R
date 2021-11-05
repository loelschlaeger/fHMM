#' Wrapper for the functionality of the fHMM package.
#' @description 
#' This function executes the following functionalities of the fHMM package and
#' saves the results in \code{path}:
#' \itemize{
#'   \item \code{\link{set_controls}}
#'   \item \code{\link{prepare_data}}
#'   \item \code{\link{summary.fHMM_data}}
#'   \item \code{\link{plot.fHMM_data}}
#'   \item \code{\link{fit_model}}
#'   \item \code{\link{decode_states}}
#'   \item \code{\link{compute_residuals}}
#'   \item \code{\link{summary.fHMM_model}}
#'   \item \code{\link{plot.fHMM_model}}
#'   \item \code{\link{predict}}
#' }
#' @param path
#' A character vector containing a single path name where the model results get
#' saved. Tilde expansion (see \link[base]{path.expand}) is done.
#' @param verbose
#' If \code{TRUE}, information are printed to the current device.
#' @inheritParams set_controls
#' @return
#' No return value. If \code{verbose = TRUE}, information are printed to the
#' current device. The following files are saved in \code{path}:
#' \itemize{
#'   \item protocol.txt (a )
#'   \item controls.rds (the output of \code{set_controls(controls)})
#'   \item data.rds (the output of \code{prepare_data(controls)})
#'   \item data.pdf ()
#'   \item model.rds (the output of 
#'         \code{compute_residuals(decode_states(fit_model(data)))})
#'   \item \code{t}.pdf, where \code{t in plot_type}
#' }

complete_analysis = function(path = paste0(tempdir(), "/", Sys.time()), 
                             controls = NULL, plot_type, events, future,
                             verbose = FALSE, seed = NULL) {
  path = path.expand(path)
  sink(dir, split = verbose)
  on.exit(sink())
  controls = set_controls(controls)
  print(controls)
  data = prepare_data(controls)
  print(summary(data))
  plot(data)
  model = compute_residuals(decode_states(fit_model(data)))
  print(summary(model))
  plot(model, type = type, events = events)
  predict(model, time_points = future)
}