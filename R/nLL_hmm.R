#' Negative log-likelihood function of an HMM
#' @description
#' This function computes the negative log-likelihood of an HMM.
#' @param parUncon
#' An object of class \code{parUncon}.
#' @param observations
#' The vector of the simulated or empirical data used for estimation.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @return
#' The negative log-likelihood value.
#' @keywords
#' internal

nLL_hmm <- function(parUncon, observations, controls) {
  class(parUncon) <- "parUncon"
  T <- length(observations)
  nstates <- controls[["states"]][1]
  par <- parUncon2par(parUncon, controls)
  sdd <- controls[["sdds"]][[1]]$name
  Gamma <- par[["Gamma"]]
  delta <- Gamma2delta(Gamma)
  mus <- par[["mus"]]
  sigmas <- par[["sigmas"]]
  dfs <- par[["dfs"]]
  allprobs <- matrix(NA, nstates, T)
  for (i in 1:nstates) {
    if (sdd == "t") {
      allprobs[i, ] <- 1 / sigmas[i] * dt(
        x = (observations - mus[i]) / sigmas[i],
        df = dfs[i]
      )
    }
    if (sdd == "gamma") {
      allprobs[i, ] <- dgamma(
        x = observations,
        shape = mus[i]^2 / sigmas[i]^2,
        scale = sigmas[i]^2 / mus[i]
      )
    }
  }
  return(-LL_HMM_Rcpp(allprobs, Gamma, delta, nstates, T))
}
