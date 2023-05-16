#' fHMM: Fitting Hidden Markov Models to Financial Data
#'
#' @description 
#' The \{fHMM\} package provides tools for modeling financial data with
#' the (hierarchical) hidden Markov model.
#' 
#' Please see the [package website](https://loelschlaeger.de/fHMM) 
#' for more details.
#'
#' @docType package
#' @name fHMM
#' @useDynLib fHMM, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @keywords internal
NULL

#' @noRd
#' @importFrom cli style_hyperlink
#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg) {
  msg <- c(
    paste0(
      "Thanks for using {fHMM} version ", utils::packageVersion("fHMM")
    ), "!\n",
    "Documentation: ", 
    cli::style_hyperlink(
      "https://loelschlaeger.de/fHMM", "https://loelschlaeger.de/fHMM"
    )
  )
  packageStartupMessage(msg)
  invisible()
}
