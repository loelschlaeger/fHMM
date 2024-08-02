#' Simulate data
#'
#' @description
#' This helper function simulates HMM data.
#'
#' @inheritParams set_controls
#' @inheritParams prepare_data
#'
#' @return
#' A \code{list} containing the following elements:
#' \itemize{
#'   \item \code{time_points}, the \code{vector} (or \code{matrix} in the 
#'         hierarchical case) of time points,
#'   \item \code{markov_chain}, the \code{vector} (or \code{matrix} in the 
#'         hierarchical case) of the simulated states,
#'   \item \code{data}, the \code{vector} (or \code{matrix} in the hierarchical
#'         case) of the simulated state-dependent observations,
#'   \item \code{T_star}, the \code{numeric} vector of fine-scale chunk sizes 
#'         in the hierarchical case
#' }
#' 
#' @examples
#' simulate_hmm(states = 2, sdds = "normal", horizon = 10)
#' 
#' @export

simulate_hmm <- function(
    controls = list(), 
    hierarchy = FALSE, 
    states = if (!hierarchy) 2 else c(2, 2), 
    sdds = if (!hierarchy) "normal" else c("normal", "normal"), 
    horizon = if (!hierarchy) 100 else c(100, 30),
    period = NA, 
    true_parameters = fHMM_parameters(
      controls = controls, hierarchy = hierarchy, states = states, sdds = sdds
    ), 
    seed = NULL
  ) {

  ### check inputs
  controls <- set_controls(
    controls = controls, hierarchy = hierarchy, states = states, sdds = sdds,
    horizon = horizon, period = period
  )
  if (!inherits(true_parameters, "fHMM_parameters")) {
    stop("'true_parameters' is not of class 'fHMM_parameters'.", call. = FALSE)
  }
  if (!controls$simulated) {
    stop("'controls$simulated' is not 'TRUE'.", call. = FALSE)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### simulate data
  if (!controls[["hierarchy"]]) {
    markov_chain <- oeli::simulate_markov_chain(
      Gamma = true_parameters$Gamma,
      T = controls[["horizon"]][1]
    )
    data <- simulate_observations(
      markov_chain = markov_chain,
      sdd = controls[["sdds"]][[1]]$name,
      mu = true_parameters$mu,
      sigma = true_parameters$sigma,
      df = true_parameters$df,
      seed = seed
    )
    time_points <- 1:controls[["horizon"]][1]
  } else {
    T_star <- compute_T_star(
      horizon = controls[["horizon"]],
      period = controls[["period"]]
    )
    markov_chain <- matrix(NA_real_,
      nrow = controls[["horizon"]][1],
      ncol = max(T_star) + 1
    )
    data <- matrix(NA_real_,
      nrow = controls[["horizon"]][1],
      ncol = max(T_star) + 1
    )
    time_points <- matrix(NA_real_,
      nrow = controls[["horizon"]][1],
      ncol = max(T_star) + 1
    )
    markov_chain[, 1] <- oeli::simulate_markov_chain(
      Gamma = true_parameters$Gamma,
      T = controls[["horizon"]][1]
    )
    data[, 1] <- simulate_observations(
      markov_chain = markov_chain[, 1],
      sdd = controls[["sdds"]][[1]]$name,
      mu = true_parameters$mu,
      sigma = true_parameters$sigma,
      df = true_parameters$df,
      seed = seed
    )
    time_points[, 1] <- utils::head(c(1, cumsum(T_star) + 1), -1)
    for (t in 1:controls[["horizon"]][1]) {
      S_t <- markov_chain[t, 1]
      mc <- oeli::simulate_markov_chain(
        Gamma = true_parameters$Gamma_star[[S_t]],
        T = max(T_star)
      )
      if (T_star[t] < max(T_star)) {
        mc[T_star[t]:max(T_star)] <- NA
      }
      markov_chain[t, -1] <- mc
      data[t, -1] <- simulate_observations(
        markov_chain = markov_chain[t, -1][!is.na(markov_chain[t, -1])],
        sdd = controls[["sdds"]][[2]]$name,
        mu = true_parameters$mu_star[[S_t]],
        sigma = true_parameters$sigma_star[[S_t]],
        df = true_parameters$df_star[[S_t]],
        seed = if (!is.null(seed)) seed + t else NULL,
        total_length = max(T_star)
      )
      time_points[t, -1] <- c(
        time_points[t, 1] - 1 + (1:T_star[t]),
        rep(NA_integer_, max(T_star) - T_star[t])
      )
    }
  }

  ### return simulated data
  out <- list(
    "time_points"  = time_points,
    "markov_chain" = markov_chain,
    "data"         = data,
    "T_star"       = if (controls[["hierarchy"]]) T_star else NULL
  )
  return(out)
}

#' Simulate state-dependent observations
#'
#' @description
#' This function simulates state-dependent observations.
#'
#' @param markov_chain
#' A \code{numeric} vector of states of a Markov chain.
#' @param sdd
#' A \code{character}, the name of the state-dependent distribution.
#' @param mu
#' A \code{numeric} vector of expected values.
#' @param sigma
#' A \code{numeric} vector of standard deviations (if any).
#' @param df
#' A \code{numeric} vector of degrees of freedom (if any).
#' @param seed
#' Sets a seed for the observation sampling.
#' @param total_length
#' An \code{integer}, the total length of the output vector.
#' Must be greater or equal than \code{length(markov_chain)}.
#'
#' @return
#' A \code{numeric} vector of length \code{total_length}, where the first
#' \code{length(markov_chain)} elements are numeric values and the last
#' \code{total_length - length(markov_chain)} elements are \code{NA_real_}.
#'
#' @keywords internal

simulate_observations <- function(
    markov_chain, sdd, mu, sigma = NULL, df = NULL, seed = NULL,
    total_length = length(markov_chain)
  ) {

  ### check inputs
  checkmate::assert_integerish(markov_chain, lower = 1, any.missing = FALSE)
  checkmate::assert_number(total_length, lower = length(markov_chain))

  ### set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### simulate observations
  T <- length(markov_chain)
  observations <- numeric(T)
  for (t in 1:T) {
    s <- markov_chain[t]
    if (sdd == "normal") {
      observations[t] <- stats::rnorm(1, mu[s], sigma[s])
    }
    if (sdd == "t") {
      observations[t] <- stats::rt(1, df[s]) * sigma[s] + mu[s]
    }
    if (sdd == "gamma") {
      observations[t] <- stats::rgamma(1,
        shape = mu[s]^2 / sigma[s]^2,
        scale = sigma[s]^2 / mu[s]
      )
    }
    if (sdd == "lognormal") {
      observations[t] <- stats::rlnorm(1, meanlog = mu[s], sdlog = sigma[s])
    }
    if (sdd == "poisson") {
      observations[t] <- stats::rpois(1, lambda = mu[s])
    }
  }

  ### append NAs
  observations <- c(observations, rep(NA_real_, total_length - T))

  ### return observations
  return(observations)
}
