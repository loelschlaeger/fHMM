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

#' Akaike's Information Criterion
#'
#' @description
#' This function calculates Akaike's Information Criterion (AIC) for an
#' \code{fHMM_model} object.
#'
#' @details
#' The AIC is computed as
#' \deqn{-2 \cdot \text{LL} + k \cdot \text{npar},}
#' where \eqn{\text{LL}} is the model's log-likelihood value at the estimated
#' parameters, \eqn{k} is the penalty per parameter (\eqn{k = 2} for the
#' classical AIC), and \eqn{npar} is the number of parameters in the fitted 
#' model.
#' The AIC quantifies the trade-off between over- and under-fitting, where
#' smaller values are preferred.
#'
#' @param object
#' An object of class \code{fHMM_model}.
#'
#' @param ...
#' Optionally more objects of class \code{fHMM_model}.
#'
#' @param k
#' A numeric, the penalty per parameter. The default is \code{k = 2} for the
#' classical AIC.
#'
#' @return
#' Either a numeric value (if just one object is provided) or a numeric vector.
#'
#' @examples
#' data("dax_model_3t", package = "fHMM")
#' AIC(dax_model_3t)
#'
#' @export

AIC <- function(object, ..., k) {
  UseMethod("AIC")
}

#' @export
#' @rdname AIC

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

#' Bayesian Information Criterion
#'
#' @description
#' This function calculates the Bayesian Information Criterion (BIC) or
#' Schwarz Information Criterion for an \code{fHMM_model} object.
#'
#' @details
#' The BIC is computed as
#' \deqn{-2 \cdot \text{LL} + \text{npar} \cdot \ln{\text{nobs}},}
#' where \eqn{\text{LL}} is the model's log-likelihood value at the estimated
#' parameters, \eqn{npar} is the number of parameters in the fitted model,
#' and \eqn{\text{nobs}} is the number of data points. The BIC quantifies the
#' trade-off between over- and under-fitting, where smaller values are 
#' preferred.
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
#' BIC(dax_model_3t)
#'
#' @export

BIC <- function(object, ...) {
  UseMethod("BIC")
}

#' @export
#' @rdname BIC

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

#' Number of observations
#'
#' @description
#' This function extracts the number of observations from an \code{fHMM_model}
#' object.
#'
#' @param object
#' An object of class \code{fHMM_model}.
#'
#' @param ...
#' Ignored.
#'
#' @return
#' An integer.
#'
#' @examples
#' data("dax_model_3t", package = "fHMM")
#' nobs(dax_model_3t)
#'
#' @export

nobs <- function(object, ...) {
  UseMethod("nobs")
}

#' @export
#' @rdname nobs

nobs.fHMM_model <- function(object, ...) {
  return(length(as.vector(object$data$data)))
}

#' Log-likelihood value
#'
#' @description
#' This function computes the log-likelihood value of an \code{fHMM_model}
#' object.
#'
#' @param object
#' An object of class \code{fHMM_model}.
#' @param ...
#' Ignored.
#'
#' @return
#' A numeric.
#'
#' @export
#'
#' @examples
#' data("dax_model_3t", package = "fHMM")
#' logLik(dax_model_3t)

logLik <- function(object, ...) {
  UseMethod("logLik")
}

#' @export
#' @rdname logLik

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
