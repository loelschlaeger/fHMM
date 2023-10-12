#' @useDynLib fHMM, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @keywords internal
"_PACKAGE"

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