#' Log-likelihood function of an (H)HMM
#'
#' @description
#' This function computes the log-likelihood value off a (hierarchical) hidden 
#' Markov model for given observations and parameter values.
#'
#' @inheritParams parameter_transformations
#' @param observations
#' The vector of the simulated or empirical data used for estimation.
#' @inheritParams set_controls
#' @param negative
#' Either \code{TRUE} to return the negative log-likelihood value (useful for
#' optimization) or \code{FALSE} (default), else.
#'
#' @return
#' The log-likelihood value.
#'
#' @examples
#' controls <- set_controls(states = 2, sdds = "normal")
#' parameters <- fHMM_parameters(controls)
#' parUncon <- par2parUncon(parameters, controls)
#' observations <- 1:10
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
    Gamma = parameters[["Gamma"]], 
    delta = oeli::stationary_distribution(parameters[["Gamma"]]), 
    N = controls[["states"]][1], 
    T = length(observations)
  )
  ifelse(negative, -ll, ll)
}



#' Negative log-likelihood function of an HHMM
#'
#' @description
#' This function computes the negative log-likelihood of an HHMM.
#'
#' @param parUncon
#' An object of class \code{parUncon}.
#' @param observations
#' The matrix of the simulated or empirical data used for estimation.
#' @param controls
#' An object of class \code{fHMM_controls}.
#'
#' @return
#' The negative log-likelihood value.
#'
#' @keywords
#' internal
#'
#' @importFrom stats dt dgamma

nLL_hhmm <- function(parUncon, observations, controls) {
  class(parUncon) <- "parUncon"
  M <- controls[["states"]][1]
  N <- controls[["states"]][2]
  observations_cs <- observations[, 1]
  observations_fs <- observations[, -1]
  T <- length(observations_cs)
  par <- parUncon2par(parUncon, controls)
  Gamma <- par[["Gamma"]]
  delta <- stationary_distribution(Gamma)
  mus <- par[["mus"]]
  sigmas <- par[["sigmas"]]
  dfs <- par[["dfs"]]
  allprobs <- matrix(0, M, T)
  log_likelihoods <- matrix(0, M, T)
  controls_split <- list(
    "hierarchy" = FALSE,
    "states" = controls$states[2],
    "sdds" = controls$sdds[2]
  )
  class(controls_split) <- "fHMM_controls"
  for (m in seq_len(M)) {
    if (controls[["sdds"]][[1]]$name == "t") {
      allprobs[m, ] <- 1 / sigmas[m] * stats::dt((observations_cs - mus[m]) /
                                                   sigmas[m], dfs[m])
    }
    if (controls[["sdds"]][[1]]$name == "gamma") {
      allprobs[m, ] <- stats::dgamma(observations_cs,
                                     shape = mus[m]^2 / sigmas[m]^2,
                                     scale = sigmas[m]^2 / mus[m]
      )
    }
    if (controls[["sdds"]][[1]]$name == "lnorm") {
      allprobs[m, ] <- stats::dlnorm(observations_cs,
                                     meanlog = mus[m],
                                     sdlog = sigmas[m]
      )
    }
    par_m <- list(
      "Gamma" = par$Gammas_star[[m]],
      "mus" = par$mus_star[[m]],
      "sigmas" = par$sigmas_star[[m]],
      "dfs" = par$dfs_star[[m]]
    )
    class(par_m) <- "fHMM_parameters"
    parUncon_m <- par2parUncon(par = par_m, controls = controls_split)
    for (t in seq_len(T)) {
      log_likelihoods[m, t] <- -nLL_hmm(
        parUncon_m, observations_fs[t, ][!is.na(observations_fs[t, ])],
        controls_split
      )
    }
  }
  nLL <- -LL_HHMM_Rcpp(
    log_likelihoods = log_likelihoods, allprobs = allprobs,
    Gamma = Gamma, delta = delta, M = M, T = T
  )
  return(nLL)
}