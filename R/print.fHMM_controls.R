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
  cat("Controls:\n")
  cat("* hierarchy:", x[["hierarchy"]], "\n")
  cat("* data type:", ifelse(x[["simulated"]],"simulated","empirical"), "\n")
  if(!x[["hierarchy"]]){
    cat("* number of states:", x[["states"]][1], "\n")
    cat("* SDDs:", x[["sdds"]][1])
  } else {
    cat("* number of states:", x[["states"]][1], x[["states"]][2], "\n")
    cat("* SDDs:", x[["sdds"]][1], x[["sdds"]][2])
  }
  cat("* number of runs:", x[["fit"]][["runs"]], 
      ifelse(x[["fit"]][["at_true"]],"(initialised at true values)",""),"\n")
  return(invisible(x))
}
