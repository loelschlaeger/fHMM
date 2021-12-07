#' Print method for the summary of \code{fHMM_data}.
#' @param x
#' An object of class \code{summary.fHMM_data}.
#' @param ...
#' Ignored.
#' @return
#' Returns \code{x} invisibly.
#' @noRd

print.summary.fHMM_data <- function(x, ...) {
  cat("Summary of fHMM", ifelse(x$simulated, "simulated", "empirical"), "data\n")
  cat("* number of observations:", x$data_size, "\n")
  if (x$hierarchy) {
    cat("* fine-scale dimension:", x$fs_dim, "\n")
  }
  if (!x$simulated) {
    cat("* data source:", x$data_source, "\n")
    cat("* date column:", x$data_column, "\n")
    cat("* log returns:", x$log_returns, "\n")
    if (x$hierarchy) {
      cat("* coarse-scale merge:", deparse1(x$cs_merge, collapse = ""))
    }
  }
  return(invisible(x))
}
