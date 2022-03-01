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


#' @noRd
#' @export

summary.fHMM_model <- function(object, alpha = 0.05, ...) {

  ### model information
  simulated <- object$data$controls$simulated
  hierarchy <- object$data$controls$hierarchy
  no_par <- length(object$estimate)
  data_size <- length(as.vector(object$data$data))
  ll <- object$ll
  aic <- -2 * ll + 2 * no_par
  bic <- -2 * ll + no_par * log(data_size)
  model_info <- data.frame(
    simulated, hierarchy,
    "LL" = ll, "AIC" = aic, "BIC" = bic
  )

  ### state-dependent distributions
  sdds <- parUncon2par(object$estimate, object$data$controls)$sdds

  ### parameter estimates
  estimates_table <- coef.fHMM_model(object, alpha)

  ### states
  if (!is.null(object$decoding)) {
    if (simulated) {
      if (!hierarchy) {
        decoding_table <- table(object$data$markov_chain, object$decoding,
          dnn = c("true", "decoded")
        )
      } else {
        decoding_table_cs <- table(object$data$markov_chain[, 1],
          object$decoding[, 1],
          dnn = c("true", "decoded")
        )
        decoding_table_fs <- table(object$data$markov_chain[, -1],
          object$decoding[, -1],
          dnn = c("true", "decoded")
        )
        decoding_table <- list(
          "coarse-scale" = decoding_table_cs,
          "fine-scale" = decoding_table_fs
        )
      }
    } else {
      if (!hierarchy) {
        decoding_table <- table(object$decoding, dnn = "decoded")
      } else {
        decoding_table_cs <- table(object$decoding[, 1], dnn = "decoded")
        decoding_table_fs <- table(object$decoding[, -1], dnn = "decoded")
        decoding_table <- list(
          "coarse-scale" = decoding_table_cs,
          "fine-scale" = decoding_table_fs
        )
      }
    }
  } else {
    decoding_table <- NULL
  }

  ### residuals
  if (!is.null(object$residuals)) {
    if (!hierarchy) {
      res_summary <- summary(object$residuals)
    } else {
      res_summary_cs <- summary(object$residuals[, 1])
      res_summary_fs <- summary(as.vector(object$residuals[, -1]))
      res_summary <- list(
        "coarse-scale" = res_summary_cs,
        "fine-scale" = res_summary_fs
      )
    }
  } else {
    res_summary <- NULL
  }

  ### build and return summary
  out <- list(
    "no_par" = no_par,
    "data_size" = data_size,
    "model_info" = model_info,
    "sdds" = sdds,
    "estimates_table" = estimates_table,
    "decoding_table" = decoding_table,
    "res_summary" = res_summary
  )
  class(out) <- "summary.fHMM_model"
  return(out)
}

#' @noRd
#' @export

print.summary.fHMM_model <- function(x, digits = 4, ...) {
  cat("Summary of fHMM model\n\n")
  print(x$model_info)
  cat("\nState-dependent distributions:\n")
  print(x$sdds)
  cat("\n")
  cat("\nEstimates:\n")
  print(x$estimates_table, digits = digits)
  if (!is.null(x$decoding_table)) {
    cat("\nStates:\n")
    print(x$decoding_table, digits = digits)
  }
  if (!is.null(x$res_summary)) {
    cat("\nResiduals:\n")
    print(x$res_summary, digits = digits)
  }
  return(invisible(x))
}

#' Model coefficients
#'
#' @description
#' This function returns the estimated model coefficients and an \code{alpha}
#' confidence interval.
#'
#' @param object
#' An object of class \code{fHMM_model}.
#' @param ...
#' Ignored.
#' @inheritParams compute_ci
#'
#' @return
#' A data frame.
#'
#' @export

coef.fHMM_model <- function(object, alpha = 0.05, ...) {
  ci <- compute_ci(object, alpha)
  estimates_table <- data.frame(lapply(ci, as.vector))
  if (object$data$controls$simulated) {
    true <- par2parCon(object$data$true_parameters, object$data$controls)
    estimates_table <- cbind(estimates_table, true = as.vector(true))
  }
  rownames(estimates_table) <- parameter_labels(
    controls = object$data$controls, expected_length = nrow(estimates_table)
  )
  return(estimates_table)
}

