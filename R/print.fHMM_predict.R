#' Print method for \code{fHMM_predict}.
#' @description
#' This function is the print method for an object of class \code{fHMM_predict}.
#' @param x
#' An object of class \code{fHMM_predict}.
#' @param ...
#' Ignored.
#' @return
#' Returns \code{x} invisibly.
#' @noRd

print.fHMM_predict <- function(x, ...) {
  print(cbind(x$states,x$data))
  return(invisible(x))
}
