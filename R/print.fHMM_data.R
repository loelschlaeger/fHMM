#' Print method for \code{fHMM_data}.
#' @description
#' This function is the print method for an object of class \code{fHMM_data}.
#' @param x
#' An object of class \code{fHMM_data}.
#' @param ...
#' Ignored.
#' @return
#' Returns \code{x} invisibly.
#' @export

print.fHMM_data <- function(x, ...) {
  cat("fHMM", ifelse(x$controls$simulated, "simulated", "empirical"), "data\n")
  return(invisible(x))
}
