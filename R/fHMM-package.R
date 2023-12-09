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
#' @importFrom utils tail
#' @importFrom cli style_hyperlink
#' @importFrom utils packageVersion
#' @importFrom Rcpp evalCpp
## usethis namespace: end
NULL
