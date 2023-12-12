#' @useDynLib fHMM, .registration=TRUE
#' @keywords internal
"_PACKAGE"

#' @noRd

.onAttach <- function(lib, pkg) {
  msg <- c(
    paste0("Thanks for using {fHMM} version ", utils::packageVersion("fHMM")), "!\n",
    "See ", cli::style_hyperlink("https://loelschlaeger.de/fHMM", "https://loelschlaeger.de/fHMM") ," for help."
  )
  packageStartupMessage(msg)
  invisible()
}

## usethis namespace: start
#' @importFrom cli style_hyperlink
#' @importFrom Rcpp evalCpp
#' @importFrom stats rgamma
#' @importFrom stats rlnorm
#' @importFrom stats rnorm
#' @importFrom stats rpois
#' @importFrom stats rt
#' @importFrom utils head
#' @importFrom utils packageVersion
#' @importFrom utils tail
## usethis namespace: end
NULL
