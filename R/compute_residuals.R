#' Compute (pseudo-) residuals
#'
#' @description
#' This function computes (pseudo-) residuals of an \code{\link{fHMM_model}} 
#' object.
#'
#' @param x
#' An object of class \code{\link{fHMM_model}}.
#' @param verbose
#' Set to \code{TRUE} (default) to print progress messages.
#'
#' @return
#' An object of class \code{\link{fHMM_model}} with residuals included.
#'
#' @export
#'
#' @examples
#' compute_residuals(dax_model_3t)
#' summary(residuals(dax_model_3t))

compute_residuals <- function(x, verbose = TRUE) {

  ### check input
  if (!inherits(x,"fHMM_model")) {
    stop("'x' must be of class 'fHMM_model'.", call. = FALSE)
  }
  if (!isTRUE(verbose) && !isFALSE(verbose)) {
    stop("'verbose' must be either TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(x$decoding)) {
    warning(
      paste(
        "Cannot compute residuals without decoding.",
        "Please call 'decode_states()' first."
      ), 
      immediate. = TRUE, call. = FALSE
    )
    return(x)
  }

  ### function that computes the residuals
  cr <- function(data, mu, sigma, df, sdd_name, decoding) {
    stopifnot(length(data) == length(decoding))
    out <- rep(NA_real_, length(data))
    for (t in seq_along(data)) {
      if (sdd_name == "t") {
        Fxt <- stats::pt(
          q = (data[t] - mu[decoding[t]]) / sigma[decoding[t]],
          df = df[decoding[t]]
        )
      } else if (sdd_name == "gamma") {
        Fxt <- stats::pgamma(
          q = data[t],
          shape = mu[decoding[t]]^2 / sigma[decoding[t]]^2,
          scale = sigma[decoding[t]]^2 / mu[decoding[t]]
        )
      } else if (sdd_name == "lognormal") {
        Fxt <- stats::plnorm(
          q = data[t],
          meanlog = mu[decoding[t]],
          sdlog = sigma[decoding[t]]
        )
      } else if (sdd_name == "normal") {
        Fxt <- stats::pnorm(
          q = data[t],
          mean = mu[decoding[t]],
          sd = sigma[decoding[t]]
        )
      } else if (sdd_name == "poisson") {
        Fxt <- stats::ppois(
          q = data[t],
          lambda = mu[decoding[t]]
        )
      } else {
        stop("Unknown state-dependent distribution", call. = FALSE)
      }
      out[t] <- stats::qnorm(Fxt)
    }
    return(out)
  }

  ### compute residuals
  par <- parUncon2par(x$estimate, x$data$controls)
  if (!x$data$controls$hierarchy) {
    residuals <- cr(
      data = x$data$data, mu = par$mu, sigma = par$sigma,
      df = par$df, sdd_name = x$data$controls$sdds[[1]]$name,
      decoding = x$decoding
    )
  } else {
    residuals <- matrix(NA_real_, nrow = nrow(x$data$data), 
                        ncol = ncol(x$data$data))
    residuals[, 1] <- cr(
      data = x$data$data[, 1], mu = par$mu,
      sigma = par$sigma, df = par$df,
      sdd_name = x$data$controls$sdds[[1]]$name,
      decoding = x$decoding[, 1]
    )
    for (t in seq_len(nrow(residuals))) {
      curr <- x$decoding[t, 1]
      residuals[t, -1] <- cr(
        data = x$data$data[t, -1],
        mu = par$mu_star[[curr]],
        sigma = par$sigma_star[[curr]],
        df = par$df_star[[curr]],
        sdd_name = x$data$controls$sdds[[2]]$name,
        decoding = x$decoding[t, -1]
      )
    }
  }

  ### save residuals in 'x' and return 'x'
  if (verbose) {
    message("Computed residuals")
  }
  class(residuals) <- "fHMM_residuals"
  x$residuals <- residuals
  return(x)
}
