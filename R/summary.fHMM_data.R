#' @noRd
#' @export

summary.fHMM_data <- function(object, ...) {

  ### meta data
  simulated <- object$controls[["simulated"]]
  hierarchy <- object$controls[["hierarchy"]]

  ### data dimensionality
  data_size <- if (!hierarchy) {
    length(object[["data"]])
  } else {
    c(
      length(object[["data"]][, 1]),
      length(object[["data"]][, -1][!is.na(object[["data"]][, -1])])
    )
  }
  fs_dim <- if (hierarchy) {
    if (!is.na(object$controls$horizon[2])) {
      object$controls$horizon[2]
    } else {
      object$controls$period
    }
  } else {
    NULL
  }

  ### data origin
  data_source <- if (simulated) NULL else basename(object$controls$data$file)
  data_column <- if (simulated) NULL else object$controls$data$date_column

  ### data transformations
  log_returns <- if (!simulated) object$controls$data$logreturns else NULL
  cs_merge <- if (!simulated & hierarchy) object$controls$data$merge else NULL

  ### build and return summary
  out <- list(
    "simulated" = simulated,
    "hierarchy" = hierarchy,
    "data_size" = data_size,
    "fs_dim" = fs_dim,
    "data_source" = data_source,
    "data_column" = data_column,
    "log_returns" = log_returns,
    "cs_merge" = cs_merge
  )
  class(out) <- "summary.fHMM_data"
  return(out)
}

#' @noRd
#' @export

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
