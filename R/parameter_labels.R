#' Create labels for estimated parameters
#'
#' @description
#' This helper function creates labels for the estimated HMM parameters.
#'
#' @param controls
#' An object of class \code{fHMM_controls}.
#'
#' @param expected_length
#' The expected output length. If \code{NULL} (default), this is not checked.
#'
#' @return
#' A \code{character} vector of parameter labels.
#' 
#' @keywords internal

parameter_labels <- function(controls, expected_length = NULL) {

  ### check input
  if (!inherits(controls,"fHMM_controls")) {
    stop("'controls' is not of class 'fHMM_controls'.", call. = FALSE)
  }
  if (!checkmate::test_count(expected_length, positive = TRUE, null.ok = TRUE)) {
    stop("'expected_length' must be a positive integer.", call. = FALSE)
  }

  ### helper function for tpm labels
  tpm_labels <- function(dim) {
    out <- outer(1:dim, 1:dim, paste, sep = ".")
    return(out[row(out) != col(out)])
  }
  par_types <- function(i) {
    switch(
      controls[["sdds"]][[i]]$name,
      "normal" = c("mu", "sigma"),
      "lognormal" = c("mu", "sigma"),
      "t" = c("mu", "sigma", "df"),
      "gamma" = c("mu", "sigma"),
      "poisson" = c("mu")
    )
  }

  ### create parameter labels
  labels <- paste0("Gamma_", tpm_labels(controls$states[1]))
  for (par in par_types(1)) {
    if (is.null(controls[["sdds"]][[1]]$pars[[par]])) {
      labels <- c(labels, paste0(par, "_", 1:controls$states[1]))
    }
  }
  if (controls[["hierarchy"]]) {
    for (i in 1:controls$states[1]) {
      labels <- c(labels, paste0("Gamma*", i, "_", tpm_labels(controls$states[2])))
      for (par in par_types(2)) {
        if (is.null(controls[["sdds"]][[2]]$pars[[par]])) {
          labels <- c(labels, paste0(par, "*", i, "_", 1:controls$states[2]))
        }
      }
    }
  }

  ### check and return parameter labels
  if (!is.null(expected_length)) {
    stopifnot(length(labels) == expected_length) 
  }
  return(labels)
}
