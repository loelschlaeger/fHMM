#' @references 
#' Oelschläger, L. and Adam, T. "Detecting bearish and bullish markets in 
#' financial time series using  hierarchical hidden Markov models" 
#' (2021, Statistical Modelling)
#' @useDynLib fHMM, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' @noRd
#' @importFrom cli style_hyperlink
#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg) {
  msg <- c(
    "Thanks for using {fHMM} version ", paste0(utils::packageVersion("fHMM")),
    "!\n", "Documentation: ", 
    cli::style_hyperlink(
      "https://loelschlaeger.de/fHMM", "https://loelschlaeger.de/fHMM"
    )
  )
  packageStartupMessage(msg)
  invisible()
}