#' Constructor of an \code{fHMM_data} object
#'
#' @description
#' This function constructs an object of class \code{fHMM_data}, which contains
#' the financial data for modeling.
#' 
#' @param dates \[`any`\]\cr
#' The dates in the empirical case.
#' @param time_points \[`any`\]\cr
#' The time points in the simulated case.
#' @param markov_chain \[`any`\]\cr
#' The states in the simulated case.
#' @param data \[`any`\]\cr
#' The data for modeling.
#' @param time_series \[`any`\]\cr
#' The data before transformation.
#' @param T_star \[`NULL` | `integer()`\]\cr
#' The fine-scale chunk sizes.
#' @param controls \[`fHMM_controls`\]\cr
#' The \code{fHMM_controls} object.
#' @param true_parameters \[`NULL` | `fHMM_parameters`\]\cr
#' The \code{fHMM_parameters} object in the simulated case.
#' 
#' @return 
#' An object of class \code{fHMM_data}, which is a \code{list} containing 
#' the following elements:
#' \itemize{
#'  \item The \code{matrix} of the \code{dates} if \code{simulated = FALSE} and
#'        \code{controls$data$data_column} is specified,
#'  \item the \code{matrix} of the \code{time_points} if
#'        \code{simulated = TRUE}
#'        or \code{controls$data$data_column} is not specified,
#'  \item the \code{matrix} of the simulated \code{markov_chain} if
#'        \code{simulated = TRUE},
#'  \item the \code{matrix} of the simulated or empirical \code{data} used for 
#'        estimation,
#'  \item the \code{matrix} \code{time_series} of empirical data before the 
#'        transformation to log-returns if \code{simulated = FALSE},
#'  \item the \code{vector} of fine-scale chunk sizes \code{T_star} if
#'        \code{controls$hierarchy = TRUE},
#'  \item the input \code{controls},
#'  \item the \code{true_parameters}.
#' }

fHMM_data <- function(
    dates, time_points, markov_chain, data, time_series, T_star, controls,
    true_parameters
) {
  structure(
    list(
      "dates" = dates,
      "time_points" = time_points,
      "markov_chain" = markov_chain,
      "data" = data,
      "time_series" = time_series,
      "T_star" = T_star,
      "controls" = controls,
      "true_parameters" = true_parameters
    ),
    class = "fHMM_data"
  )
}

#' Prepare data
#'
#' @description
#' This function simulates or reads financial data for the \{fHMM\} package.
#'
#' @param controls \[`fHMM_controls`\]\cr
#' An object of class \code{fHMM_controls}.
#' @param true_parameters \[`NULL` | `fHMM_parameters`\]\cr
#' An object of class \code{fHMM_parameters}, used as simulation parameters.
#' By default, \code{true_parameters = NULL}, i.e., sampled true parameters.
#' @param seed \[`NULL` | `integer(1)`\]\cr
#' Set a seed for the data simulation.
#' No seed by default.
#'
#' @return
#' An object of class \code{\link{fHMM_data}}.
#'
#' @examples
#' controls <- set_controls()
#' data <- prepare_data(controls)
#' class(data)
#' summary(data)
#' 
#' @export

prepare_data <- function(controls, true_parameters = NULL, seed = NULL) {

  ### check inputs
  oeli::input_check_response(
    check = if (inherits(controls, "fHMM_controls")) {
      TRUE
    } else {
      "'controls' is not of class 'fHMM_controls'."
    },
    var_name = "controls"
  )

  ### process data
  if (controls[["simulated"]]) {
    if (is.null(true_parameters)) {
      true_parameters <- fHMM_parameters(controls, seed = seed)
    }
    oeli::input_check_response(
      check = if (inherits(true_parameters, "fHMM_parameters")) {
        TRUE
      } else {
        "'true_parameters' is not of class 'fHMM_parameters'."
      },
      var_name = "true_parameters"
    )
    data <- simulate_hmm(
      controls = controls, true_parameters = true_parameters, seed = seed
    )
  } else {
    data <- read_data(controls)
  }

  ### build and return object of class 'fHMM_data'
  fHMM_data(
    dates = data$dates,
    time_points = data$time_points,
    markov_chain = data$markov_chain,
    data = data$data,
    time_series = data$time_series,
    T_star = data$T_star,
    controls = controls,
    true_parameters = true_parameters
  )
}

#' @rdname fHMM_data
#' @param x \[`fHMM_data`\]\cr
#' An object of class \code{fHMM_data}.
#' @param ...
#' Currently not used.
#' @exportS3Method 

print.fHMM_data <- function(x, ...) {
  cat("fHMM", ifelse(x$controls$simulated, "simulated", "empirical"), "data\n")
  return(invisible(x))
}

#' @rdname fHMM_data
#' @param object \[`fHMM_data`\]\cr
#' An object of class \code{fHMM_data}.
#' @param ...
#' Currently not used.
#' @exportS3Method 

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
  data_source <- if (simulated) {
    NULL
  } else if (object$controls$data$data_inside) {
    "data.frame"
  } else {
    basename(object$controls$data$file)
  }
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
#' @exportS3Method 

print.summary.fHMM_data <- function(x, ...) {
  cat("Summary of fHMM", ifelse(x$simulated, "simulated", "empirical"), 
      "data\n")
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
