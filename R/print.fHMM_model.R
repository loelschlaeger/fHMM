#' Print method for \code{fHMM_model}.
#' @description 
#' This function is the print method for an object of class \code{fHMM_model}.
#' @param x
#' An object of class \code{fHMM_model}.
#' @param ...
#' Ignored.
#' @return
#' Returns \code{x} invisibly.
#' @export

print.fHMM_model = function(x, ...) {
  cat("fHMM fitted model:\n")
  cat("* total estimation time:", x$estimation_time, units(x$estimation_time), 
      "\n")
  cat("* accepted runs:", sum(!is.na(x$lls)), "of", length(x$lls), "\n")
  cat("* log-likelihood:", x$ll, "\n")
  cat("* decoding:", !is.null(x$decoding), "\n")
  cat("* residuals:", !is.null(x$residuals))
  return(invisible(x))
}
