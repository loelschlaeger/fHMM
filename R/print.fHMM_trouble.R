#' Print method for \code{fHMM_trouble}.
#' @description
#' This function is the print method for an object of class \code{fHMM_trouble}.
#' @param x
#' An object of class \code{fHMM_trouble}.
#' @param ...
#' Ignored.
#' @return
#' Returns invisibly \code{x}.
#' @noRd
#' @export

print.fHMM_trouble <- function(x, ...) {
  cat(crayon::red("\U2716", x$code, ":", x$response, "\n"))
  cat(crayon::blue("\U2139", x$debugging, "\n"))
  return(invisible(x))
}
