#' Print method for \code{fHMM_trouble}.
#' @description 
#' This function is the print method for an object of class \code{fHMM_trouble}.
#' @param x
#' An object of class \code{fHMM_trouble}.
#' @param ...
#' Ignored.
#' @return
#' No return value.
#' @export

print.fHMM_trouble = function(x, ...) {
  cat(x$code, ":", x$response, "\n")
  cat(x$debugging, "\n")
}
