#' Simulate data for the fHMM package.
#' 
#' @description
#' This function simulates financial data for the fHMM package.
#' 
#' @inheritParams prepare_data
#' 
#' @return
#' A list containing the following elements:
#' \itemize{
#'  \item the matrix of \code{time_points},
#'  \item the matrix of the simulated \code{markov_chain},
#'  \item the matrix of the simulated \code{data},
#'  \item the vector of fine-scale chunk sizes \code{T_star} if \code{controls$hierarchy = TRUE}.
#' }
#' 
#' @keywords
#' internal
#' 
#' @importFrom utils head

simulate_data <- function(controls, true_parameters, seed = NULL) {

  ### check inputs
  if (class(controls) != "fHMM_controls") {
    stop("'controls' is not of class 'fHMM_controls'.")
  }
  if (class(true_parameters) != "fHMM_parameters") {
    stop("'true_parameters' is not of class 'fHMM_parameters'.")
  }
  if (!controls$simulated) {
    stop("'controls$simulated' is not 'TRUE'.")
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
    T_star <- compute_T_star(horizon = controls[["horizon"]], period = controls[["period"]])
    markov_chain <- matrix(NA, nrow = controls[["horizon"]][1], ncol = max(T_star) + 1)
    data <- matrix(NA, nrow = controls[["horizon"]][1], ncol = max(T_star) + 1)
    time_points <- matrix(NA, nrow = controls[["horizon"]][1], ncol = max(T_star) + 1)
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
      time_points[t, -1] <- c(time_points[t, 1] - 1 + (1:T_star[t]), rep(NA, max(T_star) - T_star[t]))
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
