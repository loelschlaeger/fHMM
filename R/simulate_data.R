#' Simulate data
#'
#' @description
#' This function simulates data for the {fHMM} package.
#'
#' @inheritParams prepare_data
#'
#' @return
#' A \code{list} containing the following elements:
#' \itemize{
#'  \item the \code{matrix} of \code{time_points},
#'  \item the \code{matrix} of the simulated \code{markov_chain},
#'  \item the \code{matrix} of the simulated \code{data},
#'  \item the \code{numeric} vector of fine-scale chunk sizes \code{T_star} if
#'        \code{controls$hierarchy = TRUE}.
#' }
#'
#' @keywords
#' internal
#'
#' @importFrom utils head

simulate_data <- function(controls, true_parameters, seed = NULL) {

  ### check inputs
  if (!inherits(controls, "fHMM_controls")) {
    stop("'controls' is not of class 'fHMM_controls'.", call. = FALSE)
  }
  if (!inherits(true_parameters, "fHMM_parameters")) {
    stop("'true_parameters' is not of class 'fHMM_parameters'.", call. = FALSE)
  }
  if (!controls$simulated) {
    stop("'controls$simulated' is not 'TRUE'.", call. = FALSE)
  }

  ### simulate data
  if (!controls[["hierarchy"]]) {
    markov_chain <- simulate_markov_chain(
      Gamma = true_parameters$Gamma,
      T = controls[["horizon"]][1],
      seed = seed
    )
    data <- simulate_observations(
      markov_chain = markov_chain,
      sdd = controls[["sdds"]][[1]]$name,
      mus = true_parameters$mus,
      sigmas = true_parameters$sigmas,
      dfs = true_parameters$dfs,
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
    markov_chain[, 1] <- simulate_markov_chain(
      Gamma = true_parameters$Gamma,
      T = controls[["horizon"]][1],
      seed = seed
    )
    data[, 1] <- simulate_observations(
      markov_chain = markov_chain[, 1],
      sdd = controls[["sdds"]][[1]]$name,
      mus = true_parameters$mus,
      sigmas = true_parameters$sigmas,
      dfs = true_parameters$dfs,
      seed = seed
    )
    time_points[, 1] <- utils::head(c(1, cumsum(T_star) + 1), -1)
    for (t in 1:controls[["horizon"]][1]) {
      S_t <- markov_chain[t, 1]
      markov_chain[t, -1] <- simulate_markov_chain(
        Gamma = true_parameters$Gammas_star[[S_t]],
        T = T_star[t],
        seed = seed + t,
        total_length = max(T_star)
      )
      data[t, -1] <- simulate_observations(
        markov_chain = markov_chain[t, -1][!is.na(markov_chain[t, -1])],
        sdd = controls[["sdds"]][[2]]$name,
        mus = true_parameters$mus_star[[S_t]],
        sigmas = true_parameters$sigmas_star[[S_t]],
        dfs = true_parameters$dfs_star[[S_t]],
        seed = seed + t,
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
#' A \code{character}, the name of the state-dependent distribution, one of 
#' \code{"t"}, \code{"gamma"}, and \code{"lnorm"}.
#' @param mus
#' A \code{numeric} vector of expected values.
#' @param sigmas
#' A \code{numeric} vector of standard deviations.
#' @param dfs
#' A \code{numeric} vector of degrees of freedom.
#' Only relevant if \code{sdd = "t"}.
#' @param seed
#' Set a seed.
#' @param total_length
#' An \code{integer}, the total length of the output vector.
#' Must be greater or equal than \code{length(markov_chain)}.
#'
#' @return
#' A \code{numeric} vector of length \code{total_length}, where the first
#' \code{length(markov_chain)} elements are numeric values and the last
#' \code{total_length - length(markov_chain)} elements are \code{NA_real_}.
#'
#' @keywords
#' internal
#'
#' @importFrom stats rt rgamma

simulate_observations <- function(
    markov_chain, sdd, mus, sigmas, dfs = NULL, seed = NULL,
    total_length = length(markov_chain)
  ) {

  ### check inputs
  if (!all(is_number(markov_chain, int = TRUE, pos = TRUE))) {
    stop("'markov_chain' must be a numberic vector of Markov chain states.",
      call. = FALSE
    )
  }
  if (!(length(sdd) == 1 && sdd %in% c("t", "gamma", "lnorm"))) {
    stop("'sdd' must be one of 't' or 'gamma'.", call. = FALSE)
  }
  if (!all(is_number(mus))) {
    stop("'mus' must be a numberic vector.", call. = FALSE)
  }
  if (!all(is_number(sigmas, pos = TRUE))) {
    stop("'sigmas' must be a positive numeric vector.", call. = FALSE)
  }
  if (!(length(mus) == length(sigmas))) {
    stop("'mus' and 'sigmas' must be of the same length.", call. = FALSE)
  }
  if (sdd != "t") {
    if (!is.null(dfs)) {
      stop("'dfs' must only be specified if 'sdd' = 't'.", call. = FALSE)
    }
  } else {
    if (is.null(dfs)) {
      stop("'dfs' must be specified if 'sdd' = 't'.", call. = FALSE)
    }
    if (!(all(is_number(dfs, pos = TRUE)) && length(dfs) == length(mus))) {
      stop(paste(
        "'dfs' must be a positive number vector of length equal to",
        "'mus' and 'sigmas'."
      ), call. = FALSE)
    }
  }
  if (!is_number(total_length) || length(total_length) != 1 ||
    total_length < length(markov_chain)) {
    stop(paste(
      "'total_length' must be an integer greater or equal than",
      "'length(markov_chain)'."
    ), call. = FALSE)
  }

  ### set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### simulate observations
  T <- length(markov_chain)
  observations <- numeric(T)
  for (t in 1:T) {
    s <- markov_chain[t]
    if (sdd == "t") {
      observations[t] <- stats::rt(1, dfs[s]) * sigmas[s] + mus[s]
    }
    if (sdd == "gamma") {
      observations[t] <- stats::rgamma(1,
        shape = mus[s]^2 / sigmas[s]^2,
        scale = sigmas[s]^2 / mus[s]
      )
    }
    if (sdd == "lnorm") {
      observations[t] <- stats::rlnorm(1, meanlog = mus[s], sdlog = sigmas[s])
    }
  }

  ### append NA's
  observations <- c(observations, rep(NA_real_, total_length - T))

  ### return observations
  return(observations)
}
