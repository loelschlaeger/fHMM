#' Create labels for estimated parameters
#'
#' @description
#' This helper function creates labels for the estimated HMM parameters.
#'
#' @param controls \[`fHMM_controls`\]\cr
#' An object of class \code{fHMM_controls}.
#'
#' @param expected_length \[`NULL` | `integer(1)`\]\cr
#' The expected output length. If \code{NULL} (default), this is not checked.
#'
#' @return
#' A \code{character} vector of parameter labels.
#' 
#' @keywords internal

parameter_labels <- function(controls, expected_length = NULL) {

  ### check inputs
  oeli::input_check_response(
    check = if (inherits(controls, "fHMM_controls")) {
      TRUE
    } else {
      "'controls' is not of class 'fHMM_controls'."
    },
    var_name = "controls"
  )
  oeli::input_check_response(
    check = if (
      checkmate::test_count(
        expected_length, positive = TRUE, null.ok = TRUE
      )
    ) {
      TRUE
    } else {
      "'expected_length' must be a positive integer."
    },
    var_name = "expected_length"
  )

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
      labels <- c(
        labels,
        paste0("Gamma*", i, "_", tpm_labels(controls$states[2]))
      )
      for (par in par_types(2)) {
        if (is.null(controls[["sdds"]][[2]]$pars[[par]])) {
          labels <- c(labels, paste0(par, "*", i, "_", 1:controls$states[2]))
        }
      }
    }
  }

  ### check and return parameter labels
  if (!is.null(expected_length)) {
    oeli::input_check_response(
      check = if (length(labels) == expected_length) {
        TRUE
      } else {
        "'expected_length' does not match the number of labels."
      },
      var_name = "expected_length"
    )
  }
  return(labels)
}
