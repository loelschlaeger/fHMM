#' Summary method for an object of class \code{fHMM_model}.
#' @description
#' This function is the summary method for an object of class \code{fHMM_model}.
#' @param object
#' An object of class \code{fHMM_model}.
#' @inheritParams compute_ci
#' @param ...
#' Ignored.
#' @return
#' An object of class \code{summary.fHMM_model}.
#' @noRd

summary.fHMM_model <- function(object, ci_level = 0.05, ...) {

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
  ci <- compute_ci(object, ci_level)
  estimates_table <- data.frame(lapply(ci, as.vector))
  if (simulated) {
    true <- par2parCon(object$data$true_parameters, object$data$controls)
    estimates_table <- cbind(estimates_table, true = as.vector(true))
  }
  rownames(estimates_table) <- parameter_labels(
    controls = object$data$controls, expected_length = nrow(estimates_table)
  )

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
