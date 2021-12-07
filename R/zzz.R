.onAttach <- function(lib, pkg) {
  # startup message
  msg <- c(
    paste0(
      "Thanks for using fHMM version ", packageVersion("fHMM")
    ), ", have fun!\n",
    "See https://loelschlaeger.github.io/fHMM for help.\n",
    "Type 'citation(\"fHMM\")' for citing this R package."
  )
  packageStartupMessage(msg)
  invisible()
}
