#' @noRd
#' @export

summary.fHMM_model <- function(object, alpha = 0.05, ...) {

  ### model information
  simulated <- object$data$controls$simulated
  hierarchy <- object$data$controls$hierarchy
  no_par <- npar(object)
  data_size <- nobs(object)
  ll <- logLik(object)
  aic <- AIC(object)
  bic <- BIC(object)
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
#' A \code{data.frame}.
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

#' @exportS3Method 
#' @importFrom stats AIC

AIC.fHMM_model <- function(object, ..., k = 2) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  ll <- sapply(models, logLik.fHMM_model)
  npar <- sapply(models, npar)
  aic <- mapply(function(ll, npar) -2 * ll + 2 * npar, ll, npar)
  return(aic)
}

#' @exportS3Method 
#' @importFrom stats BIC

BIC.fHMM_model <- function(object, ...) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  ll <- sapply(models, logLik)
  npar <- sapply(models, npar)
  nobs <- sapply(models, nobs)
  bic <- mapply(function(ll, npar, nobs) -2 * ll + npar * log(nobs), ll, npar, 
                nobs)
  return(bic)
}

#' @exportS3Method 
#' @importFrom stats nobs

nobs.fHMM_model <- function(object, ...) {
  return(length(as.vector(object$data$data)))
}

#' @exportS3Method 
#' @importFrom stats logLik

logLik.fHMM_model <- function(object, ...) {
  return(object$ll)
}

#' Number of model parameters
#'
#' @description
#' This function extracts the number of model parameters of an \code{fHMM_model}
#' object.
#'
#' @param object
#' An object of class \code{fHMM_model}.
#'
#' @param ...
#' Optionally more objects of class \code{fHMM_model}.
#'
#' @return
#' Either a numeric value (if just one object is provided) or a numeric vector.
#'
#' @examples
#' data("dax_model_3t", package = "fHMM")
#' data("dax_model_2n", package = "fHMM")
#' npar(dax_model_3t, dax_model_2n)
#'
#' @export

npar <- function(object, ...) {
  UseMethod("npar")
}

#' @export
#' @rdname npar

npar.fHMM_model <- function(object, ...) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  npar <- sapply(models, function(x) length(x$estimate))
  return(npar)
}
