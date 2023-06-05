#' fHMM: Fitting Hidden Markov Models to Financial Data
#'
#' @description 
#' The \{fHMM\} package provides tools for modeling financial data with
#' the (hierarchical) hidden Markov model.
#' 
#' The package offers different state-dependent distributions (including 
#' normal, log-normal, t, Gamma, and Poisson), state decoding, model selection, 
#' model checking, and prediction.
#' 
#' Please see the [package website](https://loelschlaeger.de/fHMM) 
#' for more details.
#'
#' @author 
#' - Lennart Oelschläger, \email{oelschlaeger.lennart@gmail.com}
#' - Timo Adam, \email{ta59@st-andrews.ac.uk}
#' - Rouven Michels, \email{r.michels@uni-bielefeld.de}
#' 
#' @references 
#' Oelschläger, L. and Adam, T. "Detecting bearish and bullish markets in 
#' financial time series using  hierarchical hidden Markov models" 
#' (2021, Statistical Modelling)
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
    "Thanks for using {fHMM} version ", paste0(utils::packageVersion("fHMM")),
    "!\n", "Documentation: ", 
    cli::style_hyperlink(
      "https://loelschlaeger.de/fHMM", "https://loelschlaeger.de/fHMM"
    )
  )
  packageStartupMessage(msg)
  invisible()
}
