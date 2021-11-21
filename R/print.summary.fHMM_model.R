#' Print method for the summary of \code{fHMM_model}.
#' @param x
#' An object of class \code{summary.fHMM_model}.
#' @param ...
#' Ignored.
#' @return 
#' Returns \code{x} invisibly.
#' @export

print.summary.fHMM_model = function(x, digits = 4, ...) {
  cat("Summary of fHMM model\n\n")
  print(x$model_info)
  cat("\nState-dependent distributions:\n")
  print(x$sdds)
  cat("\n")
  cat("\nEstimates:\n")
  print(x$estimates_table, digits = digits)
  if(!is.null(x$decoding_table)){
    cat("\nStates:\n")
    print(x$decoding_table, digits = digits)
  }
  if(!is.null(x$res_summary)){
    cat("\nResiduals:\n")
    print(x$res_summary, digits = digits)
  }
  return(invisible(x))
}