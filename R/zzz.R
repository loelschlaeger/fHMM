#' @noRd
#' @importFrom utils packageVersion

.onAttach <- function(lib, pkg) {
  # startup message
  msg <- c(
    paste0("Thanks for using {fHMM} version ", utils::packageVersion("fHMM")), "!\n",
    "See https://loelschlaeger.de/fHMM for help.\n",
    "Type 'citation(\"fHMM\")' for citing this R package."
  )
  packageStartupMessage(msg)
  invisible()
}
