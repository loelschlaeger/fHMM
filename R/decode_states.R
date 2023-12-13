#' Decode the underlying hidden state sequence
#'
#' @description
#' This function decodes the (most likely) underlying hidden state sequence by
#' applying the Viterbi algorithm for global decoding.
#'
#' @references
#' <https://en.wikipedia.org/wiki/Viterbi_algorithm>
#'
#' @param x
#' An object of class \code{\link{fHMM_model}}.
#' @param verbose
#' Set to \code{TRUE} to print progress messages.
#'
#' @return
#' An object of class \code{\link{fHMM_model}} with decoded state sequence 
#' included.
#'
#' @export
#'
#' @examples
#' decode_states(dax_model_3t)
#' plot(dax_model_3t, type = "ts")

decode_states <- function(x, verbose = TRUE) {

  ### check input
  if (!inherits(x,"fHMM_model")) {
    stop("'x' must be of class 'fHMM_model'.", call. = FALSE)
  }
  if (!isTRUE(verbose) && !isFALSE(verbose)) {
    stop("'verbose' must be either TRUE or FALSE.", call. = FALSE)
  }

  ### apply Viterbi algorithm
  par <- parUncon2par(x$estimate, x$data$controls)
  if (!x$data$controls$hierarchy) {
    decoding <- viterbi(
      observations = x$data$data,
      nstates = x$data$controls$states[1],
      Gamma = par$Gamma, mu = par$mu, sigma = par$sigma,
      df = par$df, sdd = par$sdd[[1]]$name
    )
  } else {
    decoding <- matrix(NA_real_, ncol = ncol(x$data$data), 
                       nrow = nrow(x$data$data))
    decoding[, 1] <- viterbi(
      observations = x$data$data[, 1],
      nstates = x$data$controls$states[1],
      Gamma = par$Gamma, mu = par$mu,
      sigma = par$sigma, df = par$df,
      sdd = par$sdd[[1]]$name
    )
    for (t in seq_len(nrow(decoding))) {
      curr <- decoding[t, 1]
      observations <- x$data$data[t, -1][!is.na(x$data$data[t, -1])]
      decoding[t, -1] <- c(
        viterbi(
          observations = observations,
          nstates = x$data$controls$states[2],
          Gamma = par$Gamma_star[[curr]],
          mu = par$mu_star[[curr]],
          sigma = par$sigma_star[[curr]],
          df = par$df_star[[curr]],
          sdd = par$sdd[[2]]$name
        ),
        rep(NA_real_, max(x$data$T_star) - x$data$T_star[t])
      )
    }
  }

  ### save decoding in 'x' and return 'x'
  if (verbose) message("Decoded states")
  x$decoding <- decoding
  return(x)
}

#' @rdname decode_states
#' @param observations
#' A \code{numeric} \code{vector} of state-dependent observations.
#' @param nstates
#' The number of states.
#' @param sdd
#' A \code{character}, specifying the state-dependent distribution. One of 
#' \itemize{
#'   \item \code{"normal"} (the normal distribution),
#'   \item \code{"lognormal"} (the log-normal distribution),
#'   \item \code{"t"} (the t-distribution),
#'   \item \code{"gamma"} (the gamma distribution),
#'   \item \code{"poisson"} (the Poisson distribution).
#' }
#' @param Gamma
#' A transition probability \code{matrix} of dimension \code{nstates}.
#' @param mu
#' A \code{numeric} vector of expected values for the state-dependent 
#' distribution in the different states of length \code{nstates}.
#' 
#' For the gamma- or Poisson-distribution, \code{mu} must be positive.
#' 
#' @param sigma
#' A positive \code{numeric} vector of standard deviations for the 
#' state-dependent distribution in the different states of length \code{nstates}. 
#' 
#' Not relevant in case of a state-dependent Poisson distribution.
#' 
#' @param df
#' A positive \code{numeric} vector of degrees of freedom for the 
#' state-dependent distribution in the different states of length \code{nstates}. 
#' 
#' Only relevant in case of a state-dependent t-distribution.
#' 
#' @examples
#' viterbi(
#'   observations = c(1, 1, 1, 10, 10, 10),
#'   nstates      = 2,
#'   sdd          = "poisson",
#'   Gamma        = matrix(0.5, 2, 2),
#'   mu           = c(1, 10)
#' )
#' 
#' @export

viterbi <- function(
    observations, nstates, sdd, Gamma, mu, sigma = NULL, df = NULL
  ) {
  T <- length(observations)
  delta <- oeli::stationary_distribution(Gamma, soft_fail = TRUE)
  allprobs <- matrix(0, nstates, T)
  for (n in seq_len(nstates)) {
    if (sdd == "t") {
      allprobs[n, ] <- (1 / sigma[n]) * stats::dt(
        (observations - mu[n]) / sigma[n],
        df[n]
      )
    }
    if (sdd == "gamma") {
      allprobs[n, ] <- stats::dgamma(observations,
                                     shape = mu[n]^2 / sigma[n]^2,
                                     scale = sigma[n]^2 / mu[n]
      )
    }
    if (sdd == "normal") {
      allprobs[n, ] <- stats::dnorm(observations, mean = mu[n], sd = sigma[n])                            
    }
    if (sdd == "lognormal") {
      allprobs[n, ] <- stats::dlnorm(observations, meanlog = mu[n], 
                                     sdlog = sigma[n])                            
    }
    if (sdd == "poisson") {
      allprobs[n, ] <- stats::dpois(observations, lambda = mu[n])                            
    }
  }
  xi <- matrix(0, nstates, T)
  for (n in seq_len(nstates)) {
    xi[n, 1] <- log(delta[n]) + log(allprobs[n, 1])
  }
  for (t in seq_len(T)[-1]) {
    for (n in seq_len(nstates)) {
      xi[n, t] <- max(xi[, t - 1] + log(Gamma[, n])) + log(allprobs[n, t])
    }
  }
  iv <- numeric(T)
  iv[T] <- which.max(xi[, T])
  for (t in rev(seq_len(T - 1))) {
    iv[t] <- which.max(xi[, t] + log(Gamma[, iv[t + 1]]))
  }
  return(iv)
}







