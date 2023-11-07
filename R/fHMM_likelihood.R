#' Negative log-likelihood function of an HMM
#'
#' @description
#' This function computes the negative log-likelihood of an HMM.
#'
#' @param parUncon
#' An object of class \code{parUncon}.
#' @param observations
#' The vector of the simulated or empirical data used for estimation.
#' @param controls
#' An object of class \code{fHMM_controls}.
#'
#' @return
#' The negative log-likelihood value.
#'
#' @export

# TODO: rename nLL_hmm -> ll_hmm, neg as argument

nLL_hmm <- function(parUncon, observations, controls) {
  class(parUncon) <- "parUncon"
  T <- length(observations)
  nstates <- controls[["states"]][1]
  par <- parUncon2par(parUncon, controls)
  sdd <- controls[["sdds"]][[1]]$name
  Gamma <- par[["Gamma"]]
  delta <- stationary_distribution(Gamma)
  mus <- par[["mus"]]
  sigmas <- par[["sigmas"]]
  dfs <- par[["dfs"]]
  allprobs <- matrix(NA_real_, nstates, T)
  for (i in 1:nstates) {
    if (sdd == "t") {
      allprobs[i, ] <- 1 / sigmas[i] * stats::dt(
        x = (observations - mus[i]) / sigmas[i],
        df = dfs[i]
      )
    }
    if (sdd == "gamma") {
      allprobs[i, ] <- stats::dgamma(
        x = observations,
        shape = mus[i]^2 / sigmas[i]^2,
        scale = sigmas[i]^2 / mus[i]
      )
    }
    if (sdd == "lnorm") {
      allprobs[i, ] <- stats::dlnorm(
        x = observations,
        meanlog = mus[i],
        sdlog = sigmas[i]
      )
    }
  }
  return(-LL_HMM_Rcpp(allprobs, Gamma, delta, nstates, T))
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