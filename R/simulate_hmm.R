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
#'   \item \code{observations}, the \code{vector} of the simulated observations
#'   \item \code{states}, the \code{vector} of the simulated states
#'   \item \code{parameter}, the \code{\link{fHMM_parameters}} object
#' }
#' 
#' In the hierarchical case, also the following elements:
#' \itemize{
#'   \item \code{observations_fs}, a \code{list} with the simulated fine-scale
#'         observations corresponding to each (coarse-scale) observation
#'   \item \code{states_fs}, a \code{list} with the simulated fine-scale
#'         states corresponding to each (coarse-scale) state
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
#'   horizon = c(5, NA), period = "w"
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
  .fHMM_seed(controls)
  sdd <- controls[["sdds"]][[1]]$sample
  states <- oeli::simulate_markov_chain(
    Gamma = true_parameters[["Gamma"]], T = controls[["horizon"]][1]
  )
  observations <- rep(NA_real_, controls[["horizon"]][1])
  for (t in seq_along(observations)) {
    observations[t] <- sdd(
      n = 1, state = states[t], mu = true_parameters[["mu"]],
      sigma = true_parameters[["sigma"]], df = true_parameters[["df"]]
    )
  }
  if (.fHMM_hierarchical(controls)) {
    observations_fs <- list()
    states_fs <- list()
    T_star <- compute_T_star(controls)
    sdd <- controls[["sdds"]][[2]]$sample
    for (t in seq_along(observations)) {
      states_fs[[t]] <- oeli::simulate_markov_chain(
        Gamma = true_parameters[["Gamma_star"]][[states[t]]], T = T_star[t]
      )
      observations_fs[[t]] <- rep(NA_real_, T_star[t])
      for (tt in seq_along(observations_fs[[t]])) {
        observations_fs[[t]][tt] <- sdd(
          n = 1, 
          state = states_fs[[t]][tt], 
          mu = true_parameters[["mu_star"]][[states[t]]],
          sigma = true_parameters[["sigma"]][[states[t]]], 
          df = true_parameters[["df"]][[states[t]]]
        )
      }
    }
    list(
      "observations" = observations, "states" = states, 
      "parameter" = true_parameters, "observations_fs" = observations_fs,
      "states_fs" = states_fs
    )
  } else {
    list(
      "observations" = observations, "states" = states, 
      "parameter" = true_parameters
    ) 
  }
}

#' Compute lengths of fine-scale data chunks
#'
#' @description
#' This helper function computes the lengths of the fine-scale data chunks. 
#' This is only relevant in the hierarchical case.
#'
#' @inheritParams set_controls
#' @param dates
#' A \code{character} vector of dates of empirical fine-scale data (if any).
#' By default, \code{dates = NULL}.
#'
#' @return
#' An \code{integer} vector of fine-scale chunk lengths.
#' 
#' @keywords internal
#'
#' @importFrom stats dbinom

compute_T_star <- function(
  controls = list(), 
  hierarchy = TRUE,
  horizon = if (!hierarchy) 100 else c(100, 30),
  period = if (hierarchy && is.na(horizon[2])) "m" else NA, 
  dates = NULL, 
  seed = NULL
) {
  controls <- set_controls(
    controls = controls, hierarchy = hierarchy, horizon = horizon, 
    period = period, seed = seed
  )
  if (!.fHMM_hierarchical(controls)) {
    stop(
      "The function 'compute_T_star()' is only relevant for hierarchical HMMs.",
      call. = FALSE
    )
  }
  if (is.null(dates)) {
    .fHMM_seed(controls)
    if (!is.na(controls[["horizon"]][2])) {
      T_star <- rep(controls[["horizon"]][2], controls[["horizon"]][1])
    } else {
      if (controls[["period"]] == "w") {
        size <- 5
      }
      if (controls[["period"]] == "m") {
        size <- 25
      }
      if (controls[["period"]] == "q") {
        size <- 70
      }
      if (controls[["period"]] == "y") {
        size <- 260
      }
      T_star <- sample(
        1:size, controls[["horizon"]][1], replace = TRUE, 
        prob = stats::dbinom(1:size, size, 0.9)
      )
    }
  } else {
    dates_overview <- data.frame(
      "w" = as.numeric(strftime(dates, format = "%W")),
      "m" = as.numeric(strftime(dates, format = "%m")),
      "q" = as.numeric(substr(quarters(dates), 2, 2)),
      "y" = as.numeric(strftime(dates, format = "%Y"))
    )
    if (!is.na(controls[["horizon"]][2])) {
      T_star <- rep(
        controls[["horizon"]][2], 
        floor(length(dates) / controls[["horizon"]][2])
      )
    } else {
      if (controls[["period"]] == "w") {
        T_star <- vector()
        for (y in unique(dates_overview[["y"]])) {
          dates_overview_subset <- dates_overview[dates_overview[["y"]] == y, ]
          T_star <- c(T_star, as.vector(table(dates_overview_subset[["w"]])))
        }
      }
      if (controls[["period"]] == "m") {
        T_star <- vector()
        for (y in unique(dates_overview[["y"]])) {
          dates_overview_subset <- dates_overview[dates_overview[["y"]] == y, ]
          T_star <- c(T_star, as.vector(table(dates_overview_subset[["m"]])))
        }
      }
      if (controls[["period"]] == "q") {
        T_star <- vector()
        for (y in unique(dates_overview[["y"]])) {
          dates_overview_subset <- dates_overview[dates_overview[["y"]] == y, ]
          T_star <- c(T_star, as.vector(table(dates_overview_subset[["q"]])))
        }
      }
      if (controls[["period"]] == "y") {
        T_star <- as.vector(table(dates_overview[["y"]]))
      }
    }
  }
  as.integer(T_star)
}


