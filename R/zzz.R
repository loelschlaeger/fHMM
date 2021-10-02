.onAttach = function(lib, pkg) {
  # startup message
  msg = c(paste0(
    "fHMM ", packageVersion("fHMM")),
    "\nType 'citation(\"fHMM\")' for citing this R package in publications.",
    "\nSee https://loelschlaeger.github.io/fHMM/ for references.")
  packageStartupMessage(msg)      
  invisible()
}
