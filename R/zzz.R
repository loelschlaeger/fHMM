.onAttach = function(lib, pkg) {
  # startup message
  msg = c(paste0(
    "fHMM version ", packageVersion("fHMM")),
    "\nType 'citation(\"fHMM\")' for citing this R package in publications.",
    "\nSee https://github.com/loelschlaeger/fHMM/ for references.")
  packageStartupMessage(msg)      
  invisible()
}
