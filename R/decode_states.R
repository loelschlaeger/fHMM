#' Decode the underlying hidden state sequence.
#' @description
#' This function decodes the (most likely) underlying hidden state sequence by 
#' applying the Viterbi algorithm 
#' <https://en.wikipedia.org/wiki/Viterbi_algorithm>.
#' @param x
#' An object of class \code{fHMM_model}.
#' @return
#' An object of class \code{fHMM_model}.
#' @export
#' @importFrom stats dt dgamma

decode_states <- function(x) {

  ### definition of the Viterbi algorithm for state decoding
  viterbi <- function(observations, nstates, Gamma, mus, sigmas, dfs, sdd) {
    T <- length(observations)
    delta <- Gamma2delta(Gamma)
    allprobs <- matrix(0, nstates, T)
    for (n in seq_len(nstates)) {
      if (sdd == "t") {
        allprobs[n, ] <- (1 / sigmas[n]) * stats::dt(
          (observations - mus[n]) / sigmas[n],
          dfs[n]
        )
      }
      if (sdd == "gamma") {
        allprobs[n, ] <- stats::dgamma(observations,
          shape = mus[n]^2 / sigmas[n]^2,
          scale = sigmas[n]^2 / mus[n]
        )
      }
    }
    xi <- matrix(0, nstates, T)
    for (n in seq_len(nstates)) {
      xi[n, 1] <- log(delta[n]) + log(allprobs[n, 1])
    }
    for (t in seq_len(T)[-1]) {
      for (n in seq_len(nstates)) {
        xi[n, t] <- max(xi[, t - 1] + log(Gamma[, n])) + log(allprobs[n, t])
      }
    }
    iv <- numeric(T)
    iv[T] <- which.max(xi[, T])
    for (t in rev(seq_len(T - 1))) {
      iv[t] <- which.max(xi[, t] + log(Gamma[, iv[t + 1]]))
    }
    return(iv)
  }

  ### apply Viterbi algorithm
  par <- parUncon2par(x$estimate, x$data$controls)
  if (!x$data$controls$hierarchy) {
    decoding <- viterbi(
      observations = x$data$data,
      nstates = x$data$controls$states[1],
      Gamma = par$Gamma, mus = par$mus, sigmas = par$sigmas,
      dfs = par$dfs, sdd = par$sdd[[1]]$name
    )
  } else {
    decoding <- matrix(NA, ncol = ncol(x$data$data), nrow = nrow(x$data$data))
    decoding[, 1] <- viterbi(
      observations = x$data$data[, 1],
      nstates = x$data$controls$states[1],
      Gamma = par$Gamma, mus = par$mus,
      sigmas = par$sigmas, dfs = par$dfs,
      sdd = par$sdd[[1]]$name
    )
    for (t in seq_len(nrow(decoding))) {
      curr <- decoding[t, 1]
      observations <- x$data$data[t, -1][!is.na(x$data$data[t, -1])]
      decoding[t, -1] <- c(
        viterbi(
          observations = observations,
          nstates = x$data$controls$states[2],
          Gamma = par$Gammas_star[[curr]],
          mus = par$mus_star[[curr]],
          sigmas = par$sigmas_star[[curr]],
          dfs = par$dfs_star[[curr]],
          sdd = par$sdd[[2]]$name
        ),
        rep(NA, max(x$data$T_star) - x$data$T_star[t])
      )
    }
  }

  ### save decoding in 'x' and return 'x'
  message("Decoded states")
  x$decoding <- decoding
  return(x)
}
