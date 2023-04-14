#' fHMM: Fitting Hidden Markov Models to Financial Data
#'
#' @description 
#' This package provides tools for modeling financial data via
#' the (hierarchical) hidden Markov model.
#'
#' @docType package
#'
#' @name fHMM
#'
#' @useDynLib fHMM, .registration=TRUE
#'
#' @importFrom Rcpp evalCpp
#'
#' @keywords internal
NULL

#' @noRd
#' @importFrom cli style_hyperlink
#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg) {
  msg <- c(
    paste0("Thanks for using {fHMM} version ", utils::packageVersion("fHMM")), "!\n",
    "See ", cli::style_hyperlink("https://loelschlaeger.de/fHMM", "https://loelschlaeger.de/fHMM") ," for help."
  )
  packageStartupMessage(msg)
  invisible()
}
