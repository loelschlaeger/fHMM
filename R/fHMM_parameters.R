#' Setting and checking parameters
#'
#' @description
#' This function sets and checks parameters for the {fHMM} package.
#'
#' @details
#' See the vignettes for more information on how to specify parameters.
#'
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @param Gamma
#' A tpm (transition probability matrix) of dimension \code{controls$states[1]}.
#' @param mus
#' A vector of expectations of length \code{controls$states[1]}.
#' @param sigmas
#' A vector of standard deviations of length \code{controls$states[1]}.
#' @param dfs
#' A vector of degrees of freedom of length \code{controls$states[1]}.
#' Only relevant if sdd is a t-distribution.
#' @param Gammas_star
#' A list of length \code{controls$states[1]} of tpm's. Each tpm must be of
#' dimension \code{controls$states[2]}.
#' @param mus_star
#' A list of length \code{controls$states[1]} of vectors of expectations.
#' Each vector must be of length \code{controls$states[2]}.
#' @param sigmas_star
#' A list of length \code{controls$states[1]} of vectors of standard deviations.
#' Each vector must be of length \code{controls$states[2]}.
#' @param dfs_star
#' A list of length \code{controls$states[1]} of vectors of degrees of freedom.
#' Each vector must be of length \code{controls$states[2]}.
#' Only relevant if sdd is a t-distribution.
#' @param seed
#' Set a seed for the sampling of parameters.
#' @param scale_par
#' A positive numeric vector of length two, containing scales for sampled
#' expectations and standard deviations. The first entry is the scale for
#' \code{mus} and \code{sigmas}, the second entry is the scale for
#' \code{mus_star} and \code{sigmas_star}. Set an entry to \code{1} for no
#' scaling.
#'
#' @return
#' An object of class \code{fHMM_parameters}.
#'
#' @export
#'
#' @examples
#' controls <- set_controls()
#' fHMM_parameters(controls)
#' @keywords
#' constructor
#'
#' @importFrom stats runif qunif runif

fHMM_parameters <- function(controls,
                            Gamma = NULL, mus = NULL, sigmas = NULL, dfs = NULL,
                            Gammas_star = NULL, mus_star = NULL,
                            sigmas_star = NULL, dfs_star = NULL, seed = NULL,
                            scale_par = c(1, 1)) {

  ### set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### check 'controls' and 'scale_par'
  if (class(controls) != "fHMM_controls") {
    stop("'controls' is not of class 'fHMM_controls'.")
  }
  if (!(length(scale_par) == 2 && all(is_number(scale_par, pos = TRUE)))) {
    stop("'scale_par' must be a positive numeric vector of length 2.")
  }

  ### extract number of states
  M <- controls[["states"]][1]
  N <- controls[["states"]][2]

  ### specify missing parameters
  if (is.null(Gamma)) {
    Gamma <- sample_tpm(M)
  }
  if (is.null(mus)) {
    if (controls[["sdds"]][[1]]$name == "t") {
      mus <- stats::qunif((0:(M - 1) / M + stats::runif(1, 0, 1 / M)), -1, 1) * scale_par[1]
    }
    if (controls[["sdds"]][[1]]$name == "gamma") {
      mus <- stats::qunif((0:(M - 1) / M + stats::runif(1, 0, 1 / M)), 0, 1) * scale_par[1]
    }
  }
  if (is.null(sigmas)) {
    sigmas <- stats::qunif((0:(M - 1) / M + stats::runif(1, 0, 1 / M)), 0, 1) * scale_par[1]
  }
  if (controls[["sdds"]][[1]]$name == "t") {
    if (is.null(dfs)) {
      dfs <- stats::qunif((0:(M - 1) / M + stats::runif(1, 0, 1 / M)), 0, 30)
    }
  } else {
    dfs <- NULL
  }
  if (controls[["hierarchy"]]) {
    if (is.null(Gammas_star)) {
      Gammas_star <- list()
      for (i in 1:M) {
        Gammas_star[[i]] <- sample_tpm(N)
      }
    }
    if (is.null(mus_star)) {
      mus_star <- list()
      for (i in 1:M) {
        if (controls[["sdds"]][[2]]$name == "t") {
          mus_star[[i]] <- stats::qunif((0:(N - 1) / N + stats::runif(1, 0, 1 / N)), -1, 1) * scale_par[2]
        }
        if (controls[["sdds"]][[2]]$name == "gamma") {
          mus_star[[i]] <- stats::qunif((0:(N - 1) / N + stats::runif(1, 0, 1 / N)), 0, 1) * scale_par[2]
        }
      }
    }
    if (is.null(sigmas_star)) {
      sigmas_star <- list()
      for (i in 1:M) {
        sigmas_star[[i]] <- stats::qunif((0:(N - 1) / N + stats::runif(1, 0, 1 / N)), 0, 1) * scale_par[2]
      }
    }
    if (controls[["sdds"]][[2]]$name == "t") {
      if (is.null(dfs_star)) {
        dfs_star <- list()
        for (i in 1:M) {
          dfs_star[[i]] <- stats::qunif((0:(N - 1) / N + stats::runif(1, 0, 1 / N)), 0, 30)
        }
      }
    } else {
      dfs_star <- NULL
    }
  }

  ### set fixed parameters
  if (!is.null(controls[["sdds"]][[1]]$pars$mu)) {
    mus <- rep_len(controls[["sdds"]][[1]]$pars$mu, M)
  }
  if (!is.null(controls[["sdds"]][[1]]$pars$sigma)) {
    sigmas <- rep_len(controls[["sdds"]][[1]]$pars$sigma, M)
  }
  if (controls[["sdds"]][[1]]$name == "t") {
    if (!is.null(controls[["sdds"]][[1]]$pars$df)) {
      dfs <- rep_len(controls[["sdds"]][[1]]$pars$df, M)
    }
  }
  if (controls[["hierarchy"]]) {
    if (!is.null(controls[["sdds"]][[2]]$pars$mu)) {
      mus_star <- rep(list(rep_len(controls[["sdds"]][[2]]$pars$mu, N)), M)
    }
    if (!is.null(controls[["sdds"]][[2]]$pars$sigma)) {
      sigmas_star <- rep(list(rep_len(controls[["sdds"]][[2]]$pars$sigma, N)), M)
    }
    if (controls[["sdds"]][[2]]$name == "t") {
      if (!is.null(controls[["sdds"]][[2]]$pars$df)) {
        dfs_star <- rep(list(rep_len(controls[["sdds"]][[2]]$pars$df, N)), M)
      }
    }
  }

  ### check parameters
  if (!is_tpm(Gamma) || nrow(Gamma) != M) {
    stop("'Gamma' must be a tpm of dimension 'controls$states[1]'.")
  }
  if (controls[["sdds"]][[1]]$name == "t") {
    if (!all(is_number(mus)) || length(mus) != M) {
      stop("'mu' must be a numeric vector of length 'controls$states[1]'.")
    }
  }
  if (controls[["sdds"]][[1]]$name == "gamma") {
    if (!all(is_number(mus, pos = TRUE)) || length(mus) != M) {
      stop("'mu' must be a positive numeric vector of length 'controls$states[1]'.")
    }
  }
  if (!all(is_number(sigmas, pos = TRUE)) || length(sigmas) != M) {
    stop("'sigma' must be a positive numeric vector of length 'controls$states[1]'.")
  }
  if (controls[["sdds"]][[1]]$name == "t") {
    if (!all(is_number(dfs, pos = TRUE)) || length(dfs) != M) {
      stop("'dfs' must be a positive numeric vector of length 'controls$states[1]'.")
    }
  }
  if (controls[["hierarchy"]]) {
    if (!is.list(Gammas_star) || length(Gammas_star) != M) {
      stop("'Gammas_star' must be a list of length 'controls$states[1]'.")
    }
    for (i in 1:M) {
      if (!is_tpm(Gammas_star[[i]]) || nrow(Gammas_star[[i]]) != N) {
        stop("Each element in 'Gammas_star' must be a tpm of dimension 'controls$states[2]'.")
      }
    }
    if (!is.list(mus_star) || length(mus_star) != M) {
      stop("'mus_star' must be a list of length 'controls$states[1]'.")
    }
    for (i in 1:M) {
      if (controls[["sdds"]][[2]]$name == "t") {
        if (!all(is_number(mus_star[[i]])) || length(mus_star[[i]]) != N) {
          stop("Each element in 'mus_star' must be a numeric vector of length 'controls$states[2]'.")
        }
      }
      if (controls[["sdds"]][[2]]$name == "gamma") {
        if (!all(is_number(mus_star[[i]])) || length(mus_star[[i]]) != N) {
          stop("Each element in 'mus_star' must be a numeric vector of length 'controls$states[2]'.")
        }
      }
    }
    if (!is.list(sigmas_star) || length(sigmas_star) != M) {
      stop("'sigmas_star' must be a list of length 'controls$states[1]'.")
    }
    for (i in 1:M) {
      if (!all(is_number(sigmas_star[[i]], pos = TRUE)) || length(sigmas_star[[i]]) != N) {
        stop("Each element in 'sigmas_star' must be a positive numeric vector of length 'controls$states[2]'.")
      }
    }
    if (controls[["sdds"]][[2]]$name == "t") {
      if (!is.list(dfs_star) || length(dfs_star) != M) {
        stop("'dfs_star' must be a list of length 'controls$states[1]'.")
      }
      if (!all(is_number(dfs_star[[i]], pos = TRUE)) || length(dfs_star[[i]]) != N) {
        stop("Each element in 'dfs_star' must be a positive numeric vector of length 'controls$states[2]'.")
      }
    }
  }

  ### build 'fHMM_parameters'
  out <- list(
    "Gamma" = Gamma,
    "mus" = mus,
    "sigmas" = sigmas,
    "dfs" = dfs,
    "sdds" = controls[["sdds"]],
    "Gammas_star" = if (controls[["hierarchy"]]) Gammas_star else NULL,
    "mus_star" = if (controls[["hierarchy"]]) mus_star else NULL,
    "sigmas_star" = if (controls[["hierarchy"]]) sigmas_star else NULL,
    "dfs_star" = if (controls[["hierarchy"]]) dfs_star else NULL
  )
  class(out) <- "fHMM_parameters"
  return(out)
}

#' @noRd
#' @export

print.fHMM_parameters <- function(x, ...) {
  cat("fHMM parameters\n")
}

#' This function transforms an object of class \code{fHMM_parameters} into
#' an object of class \code{parUncon}.
#' @param par
#' An object of class \code{fHMM_parameters}.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @return
#' An object of class \code{parUncon}, i.e. a vector of unconstrained model
#' parameters to be estimated.
#' @keywords
#' internal

par2parUncon <- function(par, controls) {
  stopifnot(class(par) == "fHMM_parameters")
  stopifnot(class(controls) == "fHMM_controls")
  parUncon <- Gamma2gammasUncon(par[["Gamma"]])
  if (is.null(controls$sdds[[1]]$pars$mu)) {
    parUncon <- c(
      parUncon,
      muCon2muUncon(
        muCon = par[["mus"]],
        link = (controls[["sdds"]][[1]]$name == "gamma")
      )
    )
  }
  if (is.null(controls$sdds[[1]]$pars$sigma)) {
    parUncon <- c(
      parUncon,
      sigmaCon2sigmaUncon(par[["sigmas"]])
    )
  }
  if (controls[["sdds"]][[1]]$name == "t") {
    if (is.null(controls$sdds[[1]]$pars$df)) {
      parUncon <- c(
        parUncon,
        dfCon2dfUncon(par[["dfs"]])
      )
    }
  }
  if (controls[["hierarchy"]]) {
    for (s in 1:controls[["states"]][1]) {
      parUncon <- c(
        parUncon,
        Gamma2gammasUncon(par[["Gammas_star"]][[s]])
      )
      if (is.null(controls$sdds[[2]]$pars$mu)) {
        parUncon <- c(
          parUncon,
          muCon2muUncon(par[["mus_star"]][[s]],
            link = (controls[["sdds"]][[2]]$name == "gamma")
          )
        )
      }
      if (is.null(controls$sdds[[2]]$pars$sigma)) {
        parUncon <- c(
          parUncon,
          sigmaCon2sigmaUncon(par[["sigmas_star"]][[s]])
        )
      }
      if (controls[["sdds"]][[2]]$name == "t") {
        if (is.null(controls$sdds[[2]]$pars$df)) {
          parUncon <- c(
            parUncon,
            dfCon2dfUncon(par[["dfs_star"]][[s]])
          )
        }
      }
    }
  }
  class(parUncon) <- "parUncon"
  return(parUncon)
}

#' This function transforms an object of class \code{parUncon} into an object
#' of class \code{parCon}.
#' @param parUncon
#' An object of class \code{parUncon}.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @return
#' An object of class \code{parCon}, i.e. a vector of constrained model
#' parameters to be estimated.
#' @keywords
#' internal

parUncon2parCon <- function(parUncon, controls) {
  stopifnot(class(parUncon) == "parUncon")
  stopifnot(class(controls) == "fHMM_controls")
  M <- controls[["states"]][1]
  parCon <- gammasUncon2gammasCon(parUncon[1:((M - 1) * M)], M)
  parUncon <- parUncon[-(1:((M - 1) * M))]
  if (is.null(controls$sdds[[1]]$pars$mu)) {
    parCon <- c(
      parCon,
      muUncon2muCon(parUncon[1:M],
        link = (controls[["sdds"]][[1]]$name == "gamma")
      )
    )
    parUncon <- parUncon[-(1:M)]
  }
  if (is.null(controls$sdds[[1]]$pars$sigma)) {
    parCon <- c(
      parCon,
      sigmaUncon2sigmaCon(parUncon[1:M])
    )
    parUncon <- parUncon[-(1:M)]
  }
  if (controls[["sdds"]][[1]]$name == "t") {
    if (is.null(controls$sdds[[1]]$pars$df)) {
      parCon <- c(
        parCon,
        dfUncon2dfCon(parUncon[1:M])
      )
      parUncon <- parUncon[-(1:M)]
    }
  }
  if (controls[["hierarchy"]]) {
    N <- controls[["states"]][2]
    for (s in 1:M) {
      parCon <- c(
        parCon,
        gammasUncon2gammasCon(parUncon[1:((N - 1) * N)], N)
      )
      parUncon <- parUncon[-(1:((N - 1) * N))]
      if (is.null(controls$sdds[[2]]$pars$mu)) {
        parCon <- c(
          parCon,
          muUncon2muCon(parUncon[1:N],
            link = (controls[["sdds"]][[2]]$name == "gamma")
          )
        )
        parUncon <- parUncon[-(1:N)]
      }
      if (is.null(controls$sdds[[2]]$pars$sigma)) {
        parCon <- c(
          parCon,
          sigmaUncon2sigmaCon(parUncon[1:N])
        )
        parUncon <- parUncon[-(1:N)]
      }
      if (controls[["sdds"]][[2]]$name == "t") {
        if (is.null(controls$sdds[[2]]$pars$df)) {
          parCon <- c(
            parCon,
            dfUncon2dfCon(parUncon[1:N])
          )
          parUncon <- parUncon[-(1:N)]
        }
      }
    }
  }
  class(parCon) <- "parCon"
  return(parCon)
}

#' This function transforms an object of class \code{parCon} into an object
#' of class \code{fHMM_parameters}.
#' @param parCon
#' An object of class \code{parCon}.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @return
#' An object of class \code{fHMM_parameters}.
#' @keywords
#' internal

parCon2par <- function(parCon, controls) {
  stopifnot(class(parCon) == "parCon")
  stopifnot(class(controls) == "fHMM_controls")
  M <- controls[["states"]][1]
  Gamma <- gammasCon2Gamma(parCon[1:((M - 1) * M)], M)
  parCon <- parCon[-(1:((M - 1) * M))]
  if (is.null(controls$sdds[[1]]$pars$mu)) {
    mus <- parCon[1:M]
    parCon <- parCon[-(1:M)]
  } else {
    mus <- rep(controls$sdds[[1]]$pars$mu, M)
  }
  if (is.null(controls$sdds[[1]]$pars$sigma)) {
    sigmas <- parCon[1:M]
    parCon <- parCon[-(1:M)]
  } else {
    sigmas <- rep(controls$sdds[[1]]$pars$sigma, M)
  }
  if (controls[["sdds"]][[1]]$name == "t") {
    if (is.null(controls$sdds[[1]]$pars$df)) {
      dfs <- parCon[1:M]
      parCon <- parCon[-(1:M)]
    } else {
      dfs <- rep(controls$sdds[[1]]$pars$df, M)
    }
  } else {
    dfs <- NULL
  }
  if (controls[["hierarchy"]]) {
    N <- controls[["states"]][2]
    Gammas_star <- list()
    mus_star <- list()
    sigmas_star <- list()
    if (controls[["sdds"]][[2]]$name == "t") {
      dfs_star <- list()
    } else {
      dfs_star <- NULL
    }
    for (s in 1:M) {
      Gammas_star[[s]] <- gammasCon2Gamma(parCon[1:((N - 1) * N)], N)
      parCon <- parCon[-(1:((N - 1) * N))]
      if (is.null(controls$sdds[[2]]$pars$mu)) {
        mus_star[[s]] <- parCon[1:N]
        parCon <- parCon[-(1:N)]
      } else {
        mus_star[[s]] <- rep(controls$sdds[[2]]$pars$mu, M)
      }
      if (is.null(controls$sdds[[2]]$pars$sigma)) {
        sigmas_star[[s]] <- parCon[1:N]
        parCon <- parCon[-(1:N)]
      } else {
        sigmas_star[[s]] <- rep(controls$sdds[[2]]$pars$sigma, M)
      }
      if (controls[["sdds"]][[2]]$name == "t") {
        if (is.null(controls$sdds[[2]]$pars$df)) {
          dfs_star[[s]] <- parCon[1:N]
          parCon <- parCon[-(1:N)]
        } else {
          dfs_star[[s]] <- rep(controls$sdds[[2]]$pars$df, M)
        }
      }
    }
  } else {
    Gammas_star <- NULL
    mus_star <- NULL
    sigmas_star <- NULL
    dfs_star <- NULL
  }
  par <- fHMM_parameters(
    controls = controls,
    Gamma = Gamma, mus = mus, sigmas = sigmas, dfs = dfs,
    Gammas_star = Gammas_star, mus_star = mus_star,
    sigmas_star = sigmas_star, dfs_star = dfs_star
  )
  return(par)
}

#' This function transforms an object of class \code{fHMM_parameters} into an
#' object of class \code{parCon}.
#' @param par
#' An object of class \code{fHMM_parameters}.
#' @param controls
#' An object of class{fHMM_controls}.
#' @return
#' An object of class \code{parCon}.
#' @keywords
#' internal

par2parCon <- function(par, controls) {
  stopifnot(class(par) == "fHMM_parameters")
  stopifnot(class(controls) == "fHMM_controls")
  return(parUncon2parCon(par2parUncon(par, controls), controls))
}

#' This function transforms an object of class \code{parCon} into an
#' object of class \code{parUncon}.
#' @param parCon
#' An object of class \code{parCon}.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @return
#' An object of class \code{parUncon}.
#' @keywords
#' internal

parCon2parUncon <- function(parCon, controls) {
  stopifnot(class(parCon) == "parCon")
  stopifnot(class(controls) == "fHMM_controls")
  return(par2parUncon(parCon2par(parCon, controls), controls))
}

#' This function transforms an object of class \code{parUncon} into an
#' object of class \code{fHMM_parameters}.
#' @param parUncon
#' An object of class \code{parUncon}.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @return
#' An object of class \code{fHMM_parameters}.
#' @keywords
#' internal

parUncon2par <- function(parUncon, controls) {
  stopifnot(class(parUncon) == "parUncon")
  stopifnot(class(controls) == "fHMM_controls")
  return(parCon2par(parUncon2parCon(parUncon, controls), controls))
}

#' This function un-constrains the constrained expected values \code{muCon}.
#' @param muCon
#' A vector of constrained expected values.
#' @param link
#' A boolean, determining whether to apply the link function.
#' @return
#' A vector of un-constrained expected values.
#' @keywords
#' internal

muCon2muUncon <- function(muCon, link) {
  if (link) {
    muUncon <- log(muCon)
  } else {
    muUncon <- muCon
  }
  return(muUncon)
}

#' This function constrains the un-constrained expected values \code{muUncon}.
#' @param muUncon
#' A vector of un-constrained expected values.
#' @param link
#' A boolean, determining whether to apply the link function.
#' @return
#' A vector of constrained expected values.
#' @keywords
#' internal

muUncon2muCon <- function(muUncon, link) {
  if (link) {
    muCon <- exp(muUncon)
  } else {
    muCon <- muUncon
  }
  return(muCon)
}

#' This function un-constrains the constrained standard deviations
#' \code{sigmaCon}.
#' @param sigmaCon
#' A vector of constrained standard deviations.
#' @return
#' A vector of un-constrained standard deviations.
#' @keywords
#' internal

sigmaCon2sigmaUncon <- function(sigmaCon) {
  return(log(sigmaCon))
}

#' This function constrains the un-constrained standard deviations
#' \code{sigmaUncon}.
#' @param sigmaUncon
#' A vector of un-constrained standard deviations.
#' @return
#' A vector of constrained standard deviations.
#' @keywords
#' internal

sigmaUncon2sigmaCon <- function(sigmaUncon) {
  return(exp(sigmaUncon))
}

#' This function un-constrains the constrained degrees of freedom \code{dfCon}.
#' @param dfCon
#' A vector of constrained degrees of freedom.
#' @return
#' A vector of un-constrained degrees of freedom.
#' @keywords
#' internal

dfCon2dfUncon <- function(dfCon) {
  return(log(dfCon))
}

#' This function constrains the un-constrained degrees of freedom \code{dfUncon}.
#' @param dfUncon
#' A vector of un-constrained degrees of freedom.
#' @return
#' A vector of constrained degrees of freedom.
#' @keywords
#' internal

dfUncon2dfCon <- function(dfUncon) {
  return(exp(dfUncon))
}

#' This function constrains the non-diagonal matrix elements of a transition
#' probability matrix \code{Gamma}.
#' @param Gamma
#' A transition probability matrix.
#' @param shift
#' A numeric value for shifting boundary probabilities.
#' @return
#' A vector of constrained non-diagonal matrix elements (column-wise).
#' @keywords
#' internal

Gamma2gammasCon <- function(Gamma, shift = 1e-3) {
  gammasCon <- Gamma[row(Gamma) != col(Gamma)]
  gammasCon <- replace(gammasCon, gammasCon == 0, shift)
  gammasCon <- replace(gammasCon, gammasCon == 1, 1 - shift)
  return(gammasCon)
}

#' This function un-constrains the non-diagonal matrix elements of a transition
#' probability matrix \code{Gamma}.
#' @inheritParams Gamma2gammasCon
#' @return
#' A vector of un-constrained non-diagonal matrix elements (column-wise).
#' @keywords
#' internal

Gamma2gammasUncon <- function(Gamma) {
  diag(Gamma) <- 0
  Gamma <- log(Gamma / (1 - rowSums(Gamma)))
  diag(Gamma) <- NA
  return(Gamma[!is.na(Gamma)])
}

#' This function builds a transition probability matrix of dimension \code{dim}
#' from constrained non-diagonal elements \code{gammasCon}.
#' @param gammasCon
#' A vector of constrained non-diagonal elements of a transition probability
#' matrix.
#' @param dim
#' The dimension of the transition probability matrix.
#' @return
#' A transition probability matrix.
#' @keywords
#' internal

gammasCon2Gamma <- function(gammasCon, dim) {
  Gamma <- diag(dim)
  Gamma[!Gamma] <- gammasCon
  for (i in 1:dim) {
    Gamma[i, i] <- 1 - (rowSums(Gamma)[i] - 1)
  }
  return(Gamma)
}

#' This function un-constrains the constrained non-diagonal elements
#' \code{gammasCon} of a transition probability matrix of dimension \code{dim}.
#' @inheritParams gammasCon2Gamma
#' @return
#' A vector of un-constrained non-diagonal elements of the transition
#' probability matrix.
#' @keywords
#' internal

gammasCon2gammasUncon <- function(gammasCon, dim) {
  gammasUncon <- Gamma2gammasUncon(gammasCon2Gamma(gammasCon, dim))
  return(gammasUncon)
}

#' This function builds a transition probability matrix from un-constrained
#' non-diagonal elements \code{gammasUncon}.
#' @param gammasUncon
#' A vector of un-constrained non-diagonal elements of a transition probability
#' matrix.
#' @inheritParams gammasCon2Gamma
#' @return
#' A transition probability matrix.
#' @keywords
#' internal

gammasUncon2Gamma <- function(gammasUncon, dim) {
  Gamma <- diag(dim)
  Gamma[!Gamma] <- exp(gammasUncon)
  Gamma <- Gamma / rowSums(Gamma)
  return(Gamma)
}

#' This function constrains non-diagonal elements \code{gammasUncon} of a
#' transition probability matrix.
#' @param gammasUncon
#' A vector of un-constrained non-diagonal elements of a transition probability
#' matrix.
#' @inheritParams gammasUncon2Gamma
#' @return
#' A vector of constrained non-diagonal elements of a transition probability
#' matrix.
#' @keywords
#' internal

gammasUncon2gammasCon <- function(gammasUncon, dim) {
  gammasCon <- Gamma2gammasCon(gammasUncon2Gamma(gammasUncon, dim))
  return(gammasCon)
}

#' This function computes the stationary distribution of a transition
#' probability matrix \code{Gamma}.
#' @param Gamma
#' A transition probability matrix.
#' @return
#' A stationary distribution vector.
#' @details
#' If the stationary distribution vector cannot be computed, it is set to the
#' discrete uniform distribution over the states.
#' @keywords
#' internal

Gamma2delta <- function(Gamma) {
  dim <- dim(Gamma)[1]
  delta_try <- try(solve(t(diag(dim) - Gamma + 1), rep(1, dim)), silent = TRUE)
  if (class(delta_try) == "try-error") {
    delta <- rep(1 / dim, dim)
    warning("F.1")
  } else {
    delta <- delta_try
  }
  return(delta)
}
