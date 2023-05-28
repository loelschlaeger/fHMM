#' Simulate data from a HMM
#'
#' @description
#' This function simulates time series data from a hidden Markov model.
#'
#' @inheritParams set_controls
#' @param true_parameters
#' An object of class \code{\link{fHMM_parameters}}, containing the model
#' parameter values that are used for the data simulation.
#' 
#' used as simulation parameters.
#' By default, \code{true_parameters = NULL}, i.e., sampled true parameters.
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
#' @keywords internal
#'
#' @importFrom utils head

simulate_hmm <- function(
    controls = list(), 
    hierarchy = FALSE, 
    states = if (!hierarchy) 2 else c(2, 2), 
    sdds = if (!hierarchy) "normal" else c("normal", "normal"), 
    horizon = if (!hierarchy) 100 else c(100, 30),
    period = if (hierarchy && is.na(horizon[2])) "m" else NA, 
    true_parameters = fHMM_parameters(
      controls = controls, hierarchy = hierarchy, states = states, sdds = sdds
    ), 
    seed = NULL
) {
  controls <- set_controls(
    controls = controls, hierarchy = hierarchy, states = states, sdds = sdds,
    horizon = horizon, period = period, seed = seed
  )
  if (!inherits(true_parameters, "fHMM_parameters")) {
    stop("'true_parameters' is not of class 'fHMM_parameters'.", call. = FALSE)
  }
  T <- controls[["horizon"]][1]
  markov_chain <- simulate_markov_chain(
    Gamma = true_parameters[["Gamma"]], T = T, seed = seed
  )
  observations <- rep(NA_real_, T)
  time_points <- 1:T
  sdd <- controls[["sdds"]][[1]]$sample()
  for (t in time_points) {
    observations[t] <- sdd(n = 1, state = markov_chain[t])
    # TODO: continue work from here
  }
  if (controls[["hierarchy"]]) {
    
    
  }
  list(
    "time_points"  = time_points,
    "markov_chain" = markov_chain,
    "data"         = data,
    "T_star"       = if (controls[["hierarchy"]]) T_star else NULL
  )
  
  

  # ### simulate data
  # if (!controls[["hierarchy"]]) {
  #   markov_chain <- simulate_markov_chain(
  #     Gamma = true_parameters$Gamma,
  #     T = controls[["horizon"]][1],
  #     seed = seed
  #   )
  #   data <- simulate_observations(
  #     markov_chain = markov_chain,
  #     sdd = controls[["sdds"]][[1]]$name,
  #     mus = true_parameters$mus,
  #     sigmas = true_parameters$sigmas,
  #     dfs = true_parameters$dfs,
  #     seed = seed
  #   )
  #   time_points <- 1:controls[["horizon"]][1]
  # } else {
  #   T_star <- compute_T_star(
  #     horizon = controls[["horizon"]],
  #     period = controls[["period"]]
  #   )
  #   markov_chain <- matrix(NA_real_,
  #                          nrow = controls[["horizon"]][1],
  #                          ncol = max(T_star) + 1
  #   )
  #   data <- matrix(NA_real_,
  #                  nrow = controls[["horizon"]][1],
  #                  ncol = max(T_star) + 1
  #   )
  #   time_points <- matrix(NA_real_,
  #                         nrow = controls[["horizon"]][1],
  #                         ncol = max(T_star) + 1
  #   )
  #   markov_chain[, 1] <- simulate_markov_chain(
  #     Gamma = true_parameters$Gamma,
  #     T = controls[["horizon"]][1],
  #     seed = seed
  #   )
  #   data[, 1] <- simulate_observations(
  #     markov_chain = markov_chain[, 1],
  #     sdd = controls[["sdds"]][[1]]$name,
  #     mus = true_parameters$mus,
  #     sigmas = true_parameters$sigmas,
  #     dfs = true_parameters$dfs,
  #     seed = seed
  #   )
  #   time_points[, 1] <- utils::head(c(1, cumsum(T_star) + 1), -1)
  #   for (t in 1:controls[["horizon"]][1]) {
  #     S_t <- markov_chain[t, 1]
  #     markov_chain[t, -1] <- simulate_markov_chain(
  #       Gamma = true_parameters$Gammas_star[[S_t]],
  #       T = T_star[t],
  #       seed = seed + t,
  #       total_length = max(T_star)
  #     )
  #     data[t, -1] <- simulate_observations(
  #       markov_chain = markov_chain[t, -1][!is.na(markov_chain[t, -1])],
  #       sdd = controls[["sdds"]][[2]]$name,
  #       mus = true_parameters$mus_star[[S_t]],
  #       sigmas = true_parameters$sigmas_star[[S_t]],
  #       dfs = true_parameters$dfs_star[[S_t]],
  #       seed = seed + t,
  #       total_length = max(T_star)
  #     )
  #     time_points[t, -1] <- c(
  #       time_points[t, 1] - 1 + (1:T_star[t]),
  #       rep(NA_integer_, max(T_star) - T_star[t])
  #     )
  #   }
  # }
}

#' Simulate Markov chain
#'
#' @description
#' This helper function simulates a Markov chain.
#'
#' @param Gamma
#' A transition probability \code{matrix}.
#' @param T
#' An \code{integer}, the length of the Markov chain.
#' @param delta
#' A \code{numeric} probability vector, the initial distribution. 
#' If not specified, \code{delta} is set to the stationary distribution of
#' \code{Gamma}.
#' @inheritParams set_controls
#'
#' @return
#' A \code{numeric} vector of length \code{T} with states.
#' 
#' @examples
#' \dontrun{
#' Gamma <- sample_tpm(dim = 3)
#' T <- 10
#' simulate_markov_chain(Gamma = Gamma, T = T)
#' }

simulate_markov_chain <- function(
    Gamma, T, delta = stationary_distribution(Gamma), seed = NULL
) {
  stopifnot(is_tpm(Gamma))
  stopifnot(length(T) == 1, is_number(T, int = TRUE, pos = TRUE))
  stopifnot(is.numeric(delta), length(delta) == nrow(Gamma))
  if (!is.null(seed)) {
    set.seed(seed)
  }
  N <- length(delta)
  markov_chain <- numeric(T)
  markov_chain[1] <- sample(1:N, 1, prob = delta)
  for (t in 2:T) {
    markov_chain[t] <- sample(1:N, 1, prob = Gamma[markov_chain[t - 1], ])
  }
  markov_chain
  return(markov_chain)
}


