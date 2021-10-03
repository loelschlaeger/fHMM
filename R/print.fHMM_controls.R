#' Print method for \code{fHMM_controls}.
#' @description 
#' This function is the print method for an object of class \code{fHMM_controls}.
#' @param x
#' An object of class \code{fHMM_controls}.
#' @param ...
#' Ignored.
#' @return
#' Returns \code{x} invisibly.
#' @export

print.fHMM_controls = function(x, ...) {
  cat("fHMM controls:\n")
  cat("* hierarchy:", x[["hierarchy"]], "\n")
  cat("* data type:", ifelse(x[["simulated"]],"simulated","empirical"), "\n")
  cat("* number of states:", x[["states"]], "\n")
  cat("* sdds: "); print(x[["sdds"]]); cat("\n")
  cat("* number of runs:", x[["fit"]][["runs"]], 
      ifelse(x[["fit"]][["at_true"]],"(initialised at true values)",""),"\n")
  return(invisible(x))
}
