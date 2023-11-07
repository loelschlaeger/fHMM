#' Simulate HMM data
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
#'   \item \code{observations}, the \code{vector} (or \code{matrix} in the 
#'         hierarchical case) of the simulated  observations
#'   \item \code{states}, the \code{vector} (or \code{matrix} in the 
#'         hierarchical case) of the simulated states
#'   \item \code{parameter}, the \code{\link{fHMM_parameters}} object
#' }
#'
#' @export
#'
#' @examples
#' ### HMM data
#' simulate_hmm(states = 2, sdds = "normal", horizon = 20)
#' 
#' ### HHMM data
#' simulate_hmm(
#'   hierarchy = TRUE, states = c(3, 3), sdds = c("gamma", "poisson"),
#'   horizon = c(10, NA), period = "w"
#' )
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
  if (!is.null(controls[["seed"]])) {
    set.seed(controls[["seed"]])
  }
  T <- controls[["horizon"]][1]
  states <- oeli::simulate_markov_chain(Gamma = true_parameters[["Gamma"]], T = T)
  observations <- rep(NA_real_, T)
  sdd <- controls[["sdds"]][[1]]$sample
  for (t in seq_along(observations)) {
    observations[t] <- sdd(
      n = 1, state = states[t], mu = true_parameters[["mu"]],
      sigma = true_parameters[["sigma"]], df = true_parameters[["df"]]
    )
  }
  if (controls[["hierarchy"]]) {
    T_star <- compute_T_star(
      horizon = controls[["horizon"]],
      period = controls[["period"]]
    )
  }
  list(
    "observations" = observations, "stats" = states, 
    "parameter" = true_parameters
  )
  
  

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



