#' Log-likelihood function of an (H)HMM
#'
#' @description
#' This function computes the log-likelihood value of a (hierarchical) hidden 
#' Markov model for given observations and parameter values.
#'
#' @inheritParams fHMM_parameters
#' @inheritParams parameter_transformations
#' @param observations
#' A \code{numeric} \code{vector} of time-series data.
#' 
#' In the hierarchical case (\code{hierarchy = TRUE}), a \code{matrix} with 
#' coarse-scale data in the first column and corresponding fine-scale data in 
#' the rows.
#' @inheritParams set_controls
#' @param negative
#' Either \code{TRUE} to return the negative log-likelihood value (useful for
#' optimization) or \code{FALSE} (default), else.
#'
#' @return
#' The (negative) log-likelihood value.
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
#' observations <- matrix(dnorm(110), ncol = 11, nrow = 10)
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
  negative = FALSE,
  check_controls = TRUE
) {
  if (isTRUE(check_controls)) {
    controls <- set_controls(
      controls = controls, hierarchy = hierarchy, states = states, sdds = sdds
    )
  } else {
    controls <- structure(
      oeli::merge_lists(
        controls,
        list("hierarchy" = hierarchy, "states" = states, "sdds" = sdds)
      ),
      class = "fHMM_controls"
    )
  }
  nll <- if (isTRUE(controls[["hierarchy"]])) {
    nLL_hhmm(
      parUncon = parUncon, observations = observations, controls = controls
    )
  } else {
    nLL_hmm(
      parUncon = parUncon, observations = observations, controls = controls
    )
  }
  ifelse(isTRUE(negative), nll, -nll)
}

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
#' @keywords internal

nLL_hmm <- function(parUncon, observations, controls) {
  class(parUncon) <- "parUncon"
  T <- length(observations)
  nstates <- controls[["states"]][1]
  par <- parUncon2par(parUncon, controls, FALSE, numerical_safeguard = TRUE)
  sdd <- controls[["sdds"]][[1]]$name
  Gamma <- par[["Gamma"]]
  delta <- try(
    solve(t(diag(nstates) - Gamma + 1), rep(1, nstates)),
    silent = TRUE
  )
  if (inherits(delta, "try-error")) {
    delta <- rep(1, nstates) / nstates
  }
  mu <- par[["mu"]]
  sigma <- par[["sigma"]]
  df <- par[["df"]]
  allprobs <- matrix(NA_real_, nstates, T)
  for (i in 1:nstates) {
    if (sdd == "t") {
      allprobs[i, ] <- 1 / sigma[i] * stats::dt(
        x = (observations - mu[i]) / sigma[i],
        df = df[i]
      )
    } else if (sdd == "gamma") {
      allprobs[i, ] <- stats::dgamma(
        x = observations,
        shape = mu[i]^2 / sigma[i]^2,
        scale = sigma[i]^2 / mu[i]
      )
    } else if (sdd == "normal") {
      allprobs[i, ] <- stats::dnorm(
        x = observations,
        mean = mu[i],
        sd = sigma[i]
      )
    } else if (sdd == "lognormal") {
      allprobs[i, ] <- stats::dlnorm(
        x = observations,
        meanlog = mu[i],
        sdlog = sigma[i]
      )
    } else if (sdd == "poisson") {
      allprobs[i, ] <- stats::dpois(
        x = observations,
        lambda = mu[i]
      )
    } else {
      stop("Unknown state-dependent distribution", call. = FALSE)
    }
  }
  -LL_HMM_Rcpp(allprobs, Gamma, delta, nstates, T)
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
#' @keywords internal

nLL_hhmm <- function(parUncon, observations, controls) {
  class(parUncon) <- "parUncon"
  M <- controls[["states"]][1]
  N <- controls[["states"]][2]
  observations_cs <- observations[, 1]
  observations_fs <- observations[, -1]
  T <- length(observations_cs)
  par <- parUncon2par(parUncon, controls, FALSE, numerical_safeguard = TRUE)
  Gamma <- par[["Gamma"]]
  delta <- try(
    solve(t(diag(M) - Gamma + 1), rep(1, M)),
    silent = TRUE
  )
  if (inherits(delta, "try-error")) {
    delta <- rep(1, M) / M
  }
  mu <- par[["mu"]]
  sigma <- par[["sigma"]]
  df <- par[["df"]]
  allprobs <- matrix(0, M, T)
  log_likelihoods <- matrix(0, M, T)
  controls_split <- structure(
    list(
      "hierarchy" = FALSE,
      "states" = controls$states[2],
      "sdds" = structure(controls$sdds[2], class = "fHMM_sdds")
    ),
    class = "fHMM_controls"
  )
  for (m in seq_len(M)) {
    if (controls[["sdds"]][[1]]$name == "t") {
      allprobs[m, ] <- 1 / sigma[m] * stats::dt((observations_cs - mu[m]) /
                                                  sigma[m], df[m])
    } else if (controls[["sdds"]][[1]]$name == "gamma") {
      allprobs[m, ] <- stats::dgamma(observations_cs,
                                     shape = mu[m]^2 / sigma[m]^2,
                                     scale = sigma[m]^2 / mu[m]
      )
    } else if (controls[["sdds"]][[1]]$name == "normal") {
      allprobs[m, ] <- stats::dnorm(observations_cs,
                                     mean = mu[m],
                                     sd = sigma[m]
      )
    } else if (controls[["sdds"]][[1]]$name == "lognormal") {
      allprobs[m, ] <- stats::dlnorm(observations_cs,
                                     meanlog = mu[m],
                                     sdlog = sigma[m]
      )
    } else if (controls[["sdds"]][[1]]$name == "poisson") {
      allprobs[m, ] <- stats::dpois(observations_cs, lambda = mu[m]
      )
    } else {
      stop("Unknown state-dependent distribution", call. = FALSE)
    }
    par_m <- structure(
      list(
        "Gamma" = par$Gamma_star[[m]],
        "mu" = par$mu_star[[m]],
        "sigma" = par$sigma_star[[m]],
        "df" = par$df_star[[m]]
      ),
      class = "fHMM_parameters"
    )
    parUncon_m <- par2parUncon(par_m, controls_split, FALSE)
    for (t in seq_len(T)) {
      log_likelihoods[m, t] <- -nLL_hmm(
        parUncon_m, observations_fs[t, ][!is.na(observations_fs[t, ])],
        controls_split
      )
    }
  }
  -LL_HHMM_Rcpp(
    log_likelihoods = log_likelihoods, allprobs = allprobs,
    Gamma = Gamma, delta = delta, M = M, T = T
  )
}