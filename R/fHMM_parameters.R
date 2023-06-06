#' Set and check model parameters
#'
#' @description
#' This function sets and checks model parameters.
#' Unspecified parameters are sampled.
#'
#' @details
#' See the [vignette on the model definition](https://loelschlaeger.de/fHMM/articles) 
#' for more details.
#'
#' @inheritParams set_controls
#' 
#' @param Gamma,Gamma_star
#' A transition probability \code{matrix}.
#' 
#' It should have dimension \code{states[1]}.
#' 
#' \code{Gamma_star} is a \code{list} of fine-scale transition probability 
#' matrices. The \code{list} must be of length \code{states[1]}.
#' Each transition probability matrix must be of dimension \code{states[2]}.
#' 
#' @param mu,mu_star
#' A \code{numeric} vector of expected values for the state-dependent 
#' distribution in the different states.
#' 
#' For the gamma- or Poisson-distribution, \code{mu} must be positive.
#' 
#' It should have length \code{states[1]}.
#' 
#' \code{mu_star} is a \code{list} of \code{vectors} with fine-scale 
#' expectations. The \code{list} must be of length \code{states[1]}.
#' Each \code{vector} must be of length \code{states[2]}.
#' 
#' @param sigma,sigma_star
#' A positive \code{numeric} vector of standard deviations for the 
#' state-dependent distribution in the different states. 
#' 
#' It should have length \code{states[1]}.
#' 
#' \code{sigma_star} is a \code{list} of \code{vectors} with fine-scale 
#' standard deviations. The \code{list} must be of length \code{states[1]}.
#' Each vector must be of length \code{states[2]}.
#' 
#' @param df,df_star
#' A positive \code{numeric} vector of degrees of freedom for the 
#' state-dependent distribution in the different states. 
#' 
#' It should have length \code{states[1]}.
#' 
#' Only relevant in case of a state-dependent t-distribution.
#' 
#' \code{df_star} is a \code{list} of \code{vectors} with fine-scale 
#' degrees of freedom. The \code{list} must be of length \code{states[1]}.
#' Each vector must be of length \code{states[2]}.
#' Only relevant in case of a fine-scale state-dependent t-distribution.
#' 
#' @param scale_par
#' A positive \code{numeric} vector of length two, containing scales for sampled
#' expectations and standard deviations. 
#' 
#' The first entry is the scale for
#' \code{mu} and \code{sigma}, the second entry is the scale for
#' \code{mu_star} and \code{sigma_star} (if any). 
#'
#' @return
#' An object of class \code{fHMM_parameters}.
#'
#' @export
#'
#' @examples
#' parameters <- fHMM_parameters(states = 2, sdds = "normal")
#' parameters$Gamma
#'
#' @importFrom stats runif qunif runif

fHMM_parameters <- function(
    controls = list(), hierarchy = FALSE, 
    states = if (!hierarchy) 2 else c(2, 2),
    sdds = if (!hierarchy) "normal" else c("normal", "normal"),
    Gamma = NULL, mu = NULL, sigma = NULL, df = NULL,
    Gamma_star = NULL, mu_star = NULL,
    sigma_star = NULL, df_star = NULL, seed = NULL,
    scale_par = c(1, 1)
  ) {

  ### check 'controls' and 'scale_par'
  controls <- set_controls(
    controls = controls, hierarchy = hierarchy, states = states, sdds = sdds
  )
  if (!(length(scale_par) == 2 && all(is_number(scale_par, non_neg = TRUE)))) {
    stop(
      "'scale_par' must be a positive numeric vector of length 2.",
      call. = FALSE
    )
  }

  ### extract specifications
  hierarchy <- controls[["hierarchy"]]
  M <- controls[["states"]][1] # number of (coarse-scale) states
  if (hierarchy) {
    N <- controls[["states"]][2] # number of fine-scale states
  }
  sdds <- controls[["sdds"]]
  
  ### set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### specify missing parameters
  if (is.null(Gamma)) {
    Gamma <- sample_tpm(M)
  }
  if (is.null(mu)) {
    if (sdds[[1]]$distr_class %in% c("normal", "t", "lognormal")) {
      ### expectation is unrestricted
      mu <- stats::qunif((0:(M - 1) / M + stats::runif(1, 0, 1 / M)), -1, 1) * 
        scale_par[1]
    }
    if (sdds[[1]]$distr_class %in% c("gamma", "poisson")) {
      ### expectation is positive
      mu <- stats::qunif((0:(M - 1) / M + stats::runif(1, 0, 1 / M)), 0, 1) * 
        scale_par[1]
    }
  }
  if (is.null(sigma)) {
    ### standard deviation is positive
    sigma <- stats::qunif((0:(M - 1) / M + stats::runif(1, 0, 1 / M)), 0, 1) * 
      scale_par[1]
  }
  if (sdds[[1]]$distr_class == "t") {
    if (is.null(df)) {
      ### degrees of freedom are positive
      df <- stats::qunif((0:(M - 1) / M + stats::runif(1, 0, 1 / M)), 0, 30)
    }
  } else {
    df <- NULL
  }
  if (hierarchy) {
    if (is.null(Gamma_star)) {
      Gamma_star <- list()
      for (i in 1:M) {
        Gamma_star[[i]] <- sample_tpm(N)
      }
    }
    if (is.null(mu_star)) {
      mu_star <- list()
      for (i in 1:M) {
        if (sdds[[2]]$distr_class %in% c("normal", "t", "lognormal")) {
          ### expectation is unrestricted
          mu_star[[i]] <- stats::qunif((0:(N - 1) / N + stats::runif(1, 0, 1 / N)), -1, 1) * 
            scale_par[2]
        }
        if (sdds[[2]]$distr_class %in% c("poisson", "gamma")) {
          ### expectation is positive
          mu_star[[i]] <- stats::qunif((0:(N - 1) / N + stats::runif(1, 0, 1 / N)), 0, 1) * 
            scale_par[2]
        }
      }
    }
    if (is.null(sigma_star)) {
      sigma_star <- list()
      for (i in 1:M) {
        ### standard deviation is positive
        sigma_star[[i]] <- stats::qunif((0:(N - 1) / N + stats::runif(1, 0, 1 / N)), 0, 1) * 
          scale_par[2]
      }
    }
    if (sdds[[2]]$distr_class == "t") {
      if (is.null(df_star)) {
        df_star <- list()
        for (i in 1:M) {
          ### degrees of freedom are positive
          df_star[[i]] <- stats::qunif((0:(N - 1) / N + stats::runif(1, 0, 1 / N)), 0, 30)
        }
      }
    } else {
      df_star <- NULL
    }
  }

  ### set fixed parameters (if any)
  if ("mu" %in% names(sdds[[1]]$fixed_pars)) {
    mu <- rep_len(sdds[[1]]$fixed_pars$mu, M)
  }
  if ("sigma" %in% names(sdds[[1]]$fixed_pars)) {
    sigma <- rep_len(sdds[[1]]$fixed_pars$sigma, M)
  }
  if (sdds[[1]]$distr_class == "t") {
    if ("df" %in% names(sdds[[1]]$fixed_pars)) {
      df <- rep_len(sdds[[1]]$fixed_pars$df, M)
    }
  }
  if (hierarchy) {
    if ("mu" %in% names(sdds[[2]]$fixed_pars)) {
      mu_star <- rep(list(rep_len(sdds[[2]]$fixed_pars$mu, N)), M)
    }
    if ("sigma" %in% names(sdds[[2]]$fixed_pars)) {
      sigma_star <- rep(list(rep_len(sdds[[2]]$fixed_pars$sigma, N)), M)
    }
    if (sdds[[2]]$distr_class == "t") {
      if ("df" %in% names(sdds[[2]]$fixed_pars)) {
        df_star <- rep(list(rep_len(sdds[[2]]$fixed_pars$df, N)), M)
      }
    }
  }

  ### check parameters
  if (!is_tpm(Gamma) || nrow(Gamma) != M) {
    stop(
      paste("'Gamma' must be a transition probability matrix of dimension", M),
      call. = FALSE
    )
  }
  if (sdds[[1]]$distr_class %in% c("t", "normal", "lognormal")) {
    if (!all(is_number(mu)) || length(mu) != M) {
      stop(
        paste("'mu' must be a numeric vector of length", M),
        call. = FALSE
      )
    }
  }
  if (sdds[[1]]$distr_class %in% c("gamma", "poisson")) {
    if (!all(is_number(mu, non_neg = TRUE)) || length(mu) != M) {
      stop(
        paste("'mu' must be a positive numeric vector of length", M),
        call. = FALSE
      )
    }
  }
  if (!all(is_number(sigma, non_neg = TRUE)) || length(sigma) != M) {
    stop(
      paste("'sigma' must be a positive numeric vector of length", M),
      call. = FALSE
    )
  }
  if (sdds[[1]]$distr_class == "t") {
    if (!all(is_number(df, non_neg = TRUE)) || length(df) != M) {
      stop(
        paste("'df' must be a positive numeric vector of length", M),
        call. = FALSE
      )
    }
  }
  if (hierarchy) {
    if (!is.list(Gamma_star) || length(Gamma_star) != M) {
      stop(
        paste("'Gamma_star' must be a list of length", M),
        call. = FALSE
      )
    }
    for (i in 1:M) {
      if (!is_tpm(Gamma_star[[i]]) || nrow(Gamma_star[[i]]) != N) {
        stop(
          paste("Element", i, "in 'Gamma_star' must be a transition probability matrix of dimension", N),
          call. = FALSE
        )
      }
    }
    if (!is.list(mu_star) || length(mu_star) != M) {
      stop(
        paste("'mu_star' must be a list of length", M),
        call. = FALSE
      )
    }
    for (i in 1:M) {
      if (sdds[[2]]$distr_class %in% c("t", "normal", "lognormal")) {
        if (!all(is_number(mu_star[[i]])) || length(mu_star[[i]]) != N) {
          stop(
            paste("Element", i, "in 'mu_star' must be a numeric vector of length", N),
            call. = FALSE
          )
        }
      }
      if (sdds[[2]]$distr_class %in% c("gamma", "poisson")) {
        if (!all(is_number(mu_star[[i]], non_neg = TRUE)) || length(mu_star[[i]]) != N) {
          stop(
            paste("Element", i, "in 'mu_star' must be a positive numeric vector of length", N),
            call. = FALSE
          )
        }
      }
    }
    if (!is.list(sigma_star) || length(sigma_star) != M) {
      stop(
        paste("'sigma_star' must be a list of length", M),
        call. = FALSE
      )
    }
    for (i in 1:M) {
      if (!all(is_number(sigma_star[[i]], non_neg = TRUE)) || length(sigma_star[[i]]) != N) {
        stop(
          paste("Element", i, "in 'sigma_star' must be a positive numeric vector of length", N),
          call. = FALSE
        )
      }
    }
    if (sdds[[2]]$distr_class == "t") {
      if (!is.list(df_star) || length(df_star) != M) {
        stop(
          paste("'df_star' must be a list of length", M),
          call. = FALSE
        )
      }
      for (i in 1:M) {
        if (!all(is_number(df_star[[i]], non_neg = TRUE)) || length(df_star[[i]]) != N) {
          stop(
            paste("Element", i, "in 'df_star' must be a positive numeric vector of length", N),
            call. = FALSE
          )
        }
      }
    }
  }

  ### build 'fHMM_parameters'
  out <- list(
    "Gamma" = Gamma,
    "mu" = mu,
    "sigma" = sigma,
    "df" = df,
    "sdds" = sdds
  )
  if (hierarchy) {
    out <- c(out, list(
      "Gamma_star" = Gamma_star,
      "mu_star" = mu_star,
      "sigma_star" = sigma_star,
      "df_star" = df_star
    ))
  }
  class(out) <- c("fHMM_parameters", "list")
  return(out)
}

#' @rdname fHMM_parameters
#' @param x
#' An object of class \code{fHMM_parameters}.
#' @param ...
#' Currently not used.
#' @exportS3Method 

print.fHMM_parameters <- function(x, ...) {
  cat("fHMM parameters\n")
  cat(paste0(" $", names(x), collapse = "\n"))
  invisible(x)
}

#' Parameter transformations
#' 
#' @description
#' These helper functions transform model parameters between constrained 
#' (suffix \code{*Con}) and unconstrained spaces 
#' (suffix \code{*Uncon}) for numerical optimization.
#' 
#' @name parameter_transformations
#' 
#' @inheritParams set_controls
#' 
#' @param par
#' An object of class \code{\link{fHMM_parameters}}.
#' 
#' @param link
#' Either \code{TRUE} or \code{FALSE}, determining whether to apply the link
#' function.
#' 
#' @param shift
#' A small, positive \code{numeric} for shifting boundary probabilities.
#' By default, \code{shift = 1e-3}.
#' 
#' @param dim
#' An \code{integer}, the dimension of the transition probability matrix.
#'
#' @param gammasCon,gammasUncon
#' A vector of (un-) constrained non-diagonal transition probabilities.
#' @param muCon,muUncon
#' A vector of (un-) constrained expected values.
#' @param sigmaCon,sigmaUncon
#' A vector of (un-) constrained standard deviations.
#' @param dfCon,dfUncon
#' A vector of (un-) constrained degrees of freedom.
#' 
#' @keywords internal

NULL

#' @rdname parameter_transformations
#' @return
#' For \code{par2parUncon}: a vector of unconstrained model parameters.

par2parUncon <- function(par, controls) {
  stopifnot(inherits(par, "fHMM_parameters"))
  stopifnot(inherits(controls, "fHMM_controls"))
  hierarchy <- controls[["hierarchy"]]
  sdds <- controls[["sdds"]]
  states <- controls[["states"]]
  parUncon <- Gamma2gammasUncon(par[["Gamma"]])
  if (!"mu" %in% names(sdds[[1]]$fixed_pars)) {
    parUncon <- c(
      parUncon,
      muCon2muUncon(
        muCon = par[["mu"]],
        link = (sdds[[1]]$distr_class %in% c("gamma", "poisson"))
      )
    )
  }
  if (!"sigma" %in% names(sdds[[1]]$fixed_pars)) {
    parUncon <- c(
      parUncon, sigmaCon2sigmaUncon(par[["sigma"]])
    )
  }
  if (sdds[[1]]$distr_class == "t") {
    if (!"df" %in% names(sdds[[1]]$fixed_pars)) {
      parUncon <- c(
        parUncon,
        dfCon2dfUncon(par[["df"]])
      )
    }
  }
  if (hierarchy) {
    for (s in 1:states[1]) {
      parUncon <- c(
        parUncon, Gamma2gammasUncon(par[["Gamma_star"]][[s]])
      )
      if (!"mu" %in% names(sdds[[2]]$fixed_pars)) {
        parUncon <- c(
          parUncon,
          muCon2muUncon(
            par[["mu_star"]][[s]],
            link = (sdds[[2]]$distr_class %in% c("gamma", "poisson"))
          )
        )
      }
      if (!"sigma" %in% names(sdds[[2]]$fixed_pars)) {
        parUncon <- c(
          parUncon,
          sigmaCon2sigmaUncon(par[["sigma_star"]][[s]])
        )
      }
      if (sdds[[2]]$distr_class == "t") {
        if (!"df" %in% names(sdds[[2]]$fixed_pars)) {
          parUncon <- c(
            parUncon,
            dfCon2dfUncon(par[["df_star"]][[s]])
          )
        }
      }
    }
  }
  class(parUncon) <- "parUncon"
  return(parUncon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{parUncon2parCon}: a vector of constrained model parameters.

parUncon2parCon <- function(parUncon, controls) {
  stopifnot(inherits(parUncon,"parUncon"))
  stopifnot(inherits(controls,"fHMM_controls"))
  hierarchy <- controls[["hierarchy"]]
  sdds <- controls[["sdds"]]
  M <- controls[["states"]][1]
  parCon <- gammasUncon2gammasCon(parUncon[1:((M - 1) * M)], M)
  parUncon <- parUncon[-(1:((M - 1) * M))]
  if (!"mu" %in% names(sdds[[1]]$fixed_pars)) {
    parCon <- c(
      parCon,
      muUncon2muCon(
        parUncon[1:M],
        link = (sdds[[1]]$distr_class %in% c("gamma", "poisson"))
      )
    )
    parUncon <- parUncon[-(1:M)]
  }
  if (!"sigma" %in% names(sdds[[1]]$fixed_pars)) {
    parCon <- c(
      parCon,
      sigmaUncon2sigmaCon(parUncon[1:M])
    )
    parUncon <- parUncon[-(1:M)]
  }
  if (sdds[[1]]$distr_class == "t") {
    if (!"df" %in% names(sdds[[1]]$fixed_pars)) {
      parCon <- c(
        parCon,
        dfUncon2dfCon(parUncon[1:M])
      )
      parUncon <- parUncon[-(1:M)]
    }
  }
  if (hierarchy) {
    N <- controls[["states"]][2]
    for (s in 1:M) {
      parCon <- c(
        parCon,
        gammasUncon2gammasCon(parUncon[1:((N - 1) * N)], N)
      )
      parUncon <- parUncon[-(1:((N - 1) * N))]
      if (!"mu" %in% names(sdds[[2]]$fixed_pars)) {
        parCon <- c(
          parCon,
          muUncon2muCon(
            parUncon[1:N],
            link = (sdds[[2]]$distr_class %in% c("gamma", "poisson"))
          )
        )
        parUncon <- parUncon[-(1:N)]
      }
      if (!"sigma" %in% names(sdds[[2]]$fixed_pars)) {
        parCon <- c(
          parCon,
          sigmaUncon2sigmaCon(parUncon[1:N])
        )
        parUncon <- parUncon[-(1:N)]
      }
      if (sdds[[2]]$distr_class == "t") {
        if (!"df" %in% names(sdds[[2]]$fixed_pars)) {
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

#' @rdname parameter_transformations
#' @return
#' For \code{parCon2par}: an object of class \code{\link{fHMM_parameters}}.

parCon2par <- function(parCon, controls) {
  stopifnot(inherits(parCon,"parCon"))
  stopifnot(inherits(controls,"fHMM_controls"))
  hierarchy <- controls[["hierarchy"]]
  sdds <- controls[["sdds"]]
  M <- controls[["states"]][1]
  Gamma <- gammasCon2Gamma(parCon[1:((M - 1) * M)], M)
  parCon <- parCon[-(1:((M - 1) * M))]
  if (!"mu" %in% names(sdds[[1]]$fixed_pars)) {
    mu <- parCon[1:M]
    parCon <- parCon[-(1:M)]
  } else {
    mu <- rep(sdds[[1]]$fixed_pars$mu, M)
  }
  if (!"sigma" %in% names(sdds[[1]]$fixed_pars)) {
    sigma <- parCon[1:M]
    parCon <- parCon[-(1:M)]
  } else {
    sigma <- rep(sdds[[1]]$fixed_pars$sigma, M)
  }
  if (sdds[[1]]$distr_class == "t") {
    if (!"df" %in% names(sdds[[1]]$fixed_pars)) {
      df <- parCon[1:M]
      parCon <- parCon[-(1:M)]
    } else {
      df <- rep(sdds[[1]]$fixed_pars$df, M)
    }
  } else {
    df <- NULL
  }
  if (hierarchy) {
    N <- controls[["states"]][2]
    Gamma_star <- list()
    mu_star <- list()
    sigma_star <- list()
    if (sdds[[2]]$distr_class == "t") {
      df_star <- list()
    } else {
      df_star <- NULL
    }
    for (s in 1:M) {
      Gamma_star[[s]] <- gammasCon2Gamma(parCon[1:((N - 1) * N)], N)
      parCon <- parCon[-(1:((N - 1) * N))]
      if (!"mu" %in% names(sdds[[2]]$fixed_pars)) {
        mu_star[[s]] <- parCon[1:N]
        parCon <- parCon[-(1:N)]
      } else {
        mu_star[[s]] <- rep(sdds[[2]]$fixed_pars$mu, M)
      }
      if (!"sigma" %in% names(sdds[[2]]$fixed_pars)) {
        sigma_star[[s]] <- parCon[1:N]
        parCon <- parCon[-(1:N)]
      } else {
        sigma_star[[s]] <- rep(sdds[[2]]$fixed_pars$sigma, M)
      }
      if (sdds[[2]]$distr_class == "t") {
        if (!"df" %in% names(sdds[[2]]$fixed_pars)) {
          df_star[[s]] <- parCon[1:N]
          parCon <- parCon[-(1:N)]
        } else {
          df_star[[s]] <- rep(sdds[[2]]$fixed_pars$df, M)
        }
      }
    }
  } else {
    Gamma_star <- NULL
    mu_star <- NULL
    sigma_star <- NULL
    df_star <- NULL
  }
  par <- fHMM_parameters(
    controls = controls,
    Gamma = Gamma, mu = mu, sigma = sigma, df = df,
    Gamma_star = Gamma_star, mu_star = mu_star,
    sigma_star = sigma_star, df_star = df_star
  )
  return(par)
}

#' @rdname parameter_transformations
#' @return
#' For \code{par2parCon}: a vector of constrained model parameters.

par2parCon <- function(par, controls) {
  stopifnot(inherits(par,"fHMM_parameters"))
  stopifnot(inherits(controls,"fHMM_controls"))
  return(parUncon2parCon(par2parUncon(par, controls), controls))
}

#' @rdname parameter_transformations
#' @return
#' For \code{parCon2parUncon}: a vector of unconstrained model parameters.

parCon2parUncon <- function(parCon, controls) {
  stopifnot(inherits(parCon,"parCon"))
  stopifnot(inherits(controls,"fHMM_controls"))
  return(par2parUncon(parCon2par(parCon, controls), controls))
}

#' @rdname parameter_transformations
#' @return
#' For \code{parUncon2par}: an object of class \code{fHMM_parameters}.

parUncon2par <- function(parUncon, controls) {
  stopifnot(inherits(parUncon,"parUncon"))
  stopifnot(inherits(controls,"fHMM_controls"))
  return(parCon2par(parUncon2parCon(parUncon, controls), controls))
}

#' @rdname parameter_transformations
#' @return
#' For \code{muCon2muUncon}: a vector of unconstrained expected values.

muCon2muUncon <- function(muCon, link) {
  if (link) {
    muUncon <- log(muCon)
  } else {
    muUncon <- muCon
  }
  return(muUncon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{muUncon2muCon}: a vector of constrained expected values.

muUncon2muCon <- function(muUncon, link) {
  if (link) {
    muCon <- exp(muUncon)
  } else {
    muCon <- muUncon
  }
  return(muCon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{sigmaCon2sigmaUncon}: a vector of unconstrained standard 
#' deviations.

sigmaCon2sigmaUncon <- function(sigmaCon) {
  return(log(sigmaCon))
}

#' @rdname parameter_transformations
#' @return
#' For \code{sigmaUncon2sigmaCon}: a vector of constrained standard deviations.

sigmaUncon2sigmaCon <- function(sigmaUncon) {
  return(exp(sigmaUncon))
}

#' @rdname parameter_transformations
#' @return
#' For \code{dfCon2dfUncon}: a vector of unconstrained degrees of freedom.

dfCon2dfUncon <- function(dfCon) {
  return(log(dfCon))
}

#' @rdname parameter_transformations
#' @return
#' For \code{dfUncon2dfCon}: a vector of constrained degrees of freedom.

dfUncon2dfCon <- function(dfUncon) {
  return(exp(dfUncon))
}

#' @rdname parameter_transformations
#' @return
#' For \code{Gamma2gammasCon}: a vector of constrained non-diagonal matrix 
#' elements (column-wise).

Gamma2gammasCon <- function(Gamma, shift = 1e-3) {
  gammasCon <- Gamma[row(Gamma) != col(Gamma)]
  gammasCon <- replace(gammasCon, gammasCon == 0, shift)
  gammasCon <- replace(gammasCon, gammasCon == 1, 1 - shift)
  return(gammasCon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{Gamma2gammasUncon}: a vector of unconstrained non-diagonal matrix 
#' elements (column-wise).

Gamma2gammasUncon <- function(Gamma) {
  diag(Gamma) <- 0
  Gamma <- log(Gamma / (1 - rowSums(Gamma)))
  diag(Gamma) <- NA_real_
  return(Gamma[!is.na(Gamma)])
}

#' @rdname parameter_transformations
#' @return
#' For \code{gammasCon2Gamma}: a transition probability matrix.

gammasCon2Gamma <- function(gammasCon, dim) {
  Gamma <- diag(dim)
  Gamma[!Gamma] <- gammasCon
  for (i in 1:dim) {
    Gamma[i, i] <- 1 - (rowSums(Gamma)[i] - 1)
  }
  return(Gamma)
}

#' @rdname parameter_transformations
#' @return
#' For \code{gammasCon2gammasUncon}: a vector of unconstrained non-diagonal 
#' elements of the transition probability matrix.

gammasCon2gammasUncon <- function(gammasCon, dim) {
  gammasUncon <- Gamma2gammasUncon(gammasCon2Gamma(gammasCon, dim))
  return(gammasUncon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{gammasUncon2Gamma}: a transition probability matrix.

gammasUncon2Gamma <- function(gammasUncon, dim) {
  Gamma <- diag(dim)
  Gamma[!Gamma] <- exp(gammasUncon)
  Gamma <- Gamma / rowSums(Gamma)
  return(Gamma)
}

#' @rdname parameter_transformations
#' @return
#' For \code{gammasUncon2gammasCon}: a vector of constrained non-diagonal 
#' elements of a transition probability matrix.

gammasUncon2gammasCon <- function(gammasUncon, dim) {
  gammasCon <- Gamma2gammasCon(gammasUncon2Gamma(gammasUncon, dim))
  return(gammasCon)
}
