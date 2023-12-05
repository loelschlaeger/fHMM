#' Log-likelihood function of an (H)HMM
#'
#' @description
#' This function computes the log-likelihood value of a (hierarchical) hidden 
#' Markov model for given observations and parameter values.
#'
#' @inheritParams parameter_transformations
#' @param observations
#' A \code{numeric} \code{vector} of time-series data.
#' 
#' In the hierarchical case (\code{hierarchy = TRUE}), a \code{list} with
#' two elements:
#' - the first element is a \code{numeric} \code{vector} of coarse-scale data
#' - the second element is a \code{list} of the same length as the coarse-scale
#'   data, and each element is a \code{numeric} \code{vector} of fine-scale data
#'   corresponding to the coarse-scale data.
#' @inheritParams set_controls
#' @param negative
#' Either \code{TRUE} to return the negative log-likelihood value (useful for
#' optimization) or \code{FALSE} (default), else.
#'
#' @return
#' The log-likelihood value.
#'
#' @examples
#' ### HMM log-likelihood 
#' controls <- set_controls(states = 2, sdds = "normal")
#' parameters <- fHMM_parameters(controls)
#' parUncon <- par2parUncon(parameters, controls)
#' observations <- 1:10
#' ll_hmm(parUncon, observations, controls)
#' 
#' ### HHMM log-likelihood 
#' controls <- set_controls(
#'   hierarchy = TRUE, states = c(2, 2), sdds = c("normal", "normal")
#' )
#' parameters <- fHMM_parameters(controls)
#' parUncon <- par2parUncon(parameters, controls)
#' observations <- list(
#'   1:10,                                  # fine-scale data
#'   replicate(10, 1:10, simplify = FALSE)  # coarse-scale data
#' )
#' ll_hmm(parUncon, observations, controls)
#'
#' @export

ll_hmm <- function(
    parUncon, 
    observations, 
    controls = list(),
    hierarchy = FALSE,
    states = if (!hierarchy) 2 else c(2, 2),
    sdds = if (!hierarchy) "normal" else c("normal", "normal"), 
    negative = FALSE
  ) {
  class(parUncon) <- "parUncon"
  controls <- set_controls(
    controls = controls, hierarchy = hierarchy, states = states, sdds = sdds
  )
  parameters <- parUncon2par(parUncon, controls)
  if (controls[["hierarchy"]]) {
    observations_fs <- observations[[2]]
    observations <- observations[[1]]
  }
  ll_fs <- matrix(0, nrow = controls[["states"]][1], ncol = length(observations))
  if (controls[["hierarchy"]]) {
    controls_fs <- list(
      hierarchy = FALSE,
      states = controls[["states"]][2],
      sdds = controls[["sdds"]][[2]][["distr_class"]]
    )
    for (m in seq_len(controls[["states"]][1])) {
      parUncon_m <- par2parUncon(
        par = fHMM_parameters(
          controls = controls_fs,
          Gamma = parameters[["Gamma_star"]][[m]],
          mu = parameters[["mu_star"]][[m]],
          sigma = parameters[["sigma_star"]][[m]],
          df = parameters[["df_star"]][[m]]
        ), 
        controls = controls_fs
      )
      for (t in seq_along(observations)) {
        ll_fs[m, t] <- ll_hmm(
          parUncon = parUncon_m, 
          observations = observations_fs[[t]],
          controls = controls_fs
        )
      }
    }
  }
  allprobs <- allprobs(
    observations = observations, 
    sdd = controls[["sdds"]][[1]], 
    state = controls[["states"]][1], 
    "mu" = parameters[["mu"]], 
    "sigma" = parameters[["sigma"]], 
    "df" = parameters[["df"]]
  )
  ll <- LL_HMM_Rcpp(
    allprobs = allprobs, 
    ll_fs = ll_fs,
    Gamma = parameters[["Gamma"]], 
    delta = oeli::stationary_distribution(parameters[["Gamma"]]), 
    N = controls[["states"]][1], 
    T = length(observations)
  )
  ifelse(negative, -ll, ll)
}

