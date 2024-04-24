#' Set and check model parameters
#'
#' @description
#' This function sets and checks model parameters.
#' Unspecified parameters are sampled.
#'
#' @details
#' See the [vignette on the model definition](https://loelschlaeger.de/fHMM/articles/) 
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
#' @param seed
#' Sets a seed for the sampling of parameters.
#' 
#' @param check_controls
#' Either \code{TRUE} to check the defined controls or \code{FALSE} to not check
#' them (which saves computation time), else.
#'
#' @return
#' An object of class \code{fHMM_parameters}.
#'
#' @export
#'
#' @examples
#' parameters <- fHMM_parameters(states = 2, sdds = "normal")
#' parameters$Gamma

fHMM_parameters <- function(
    controls = list(), 
    hierarchy = FALSE, 
    states = if (!hierarchy) 2 else c(2, 2),
    sdds = if (!hierarchy) "normal" else c("normal", "normal"),
    Gamma = NULL, mu = NULL, sigma = NULL, df = NULL,
    Gamma_star = NULL, mu_star = NULL,
    sigma_star = NULL, df_star = NULL,
    scale_par = c(1, 1), seed = NULL,
    check_controls = TRUE
) {
  
  ### check 'controls' and 'scale_par'
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
  if (!checkmate::test_numeric(scale_par, len = 2, lower = 0)) {
    stop(
      "'scale_par' must be a positive numeric vector of length 2.",
      call. = FALSE
    )
  }
  
  ### extract specifications
  M <- controls[["states"]][1] # number of (coarse-scale) states
  N <- controls[["states"]][2] # number of fine-scale states
  sdds <- controls[["sdds"]]
  
  ### set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  ### specify missing parameters
  if (is.null(Gamma)) {
    Gamma <- oeli::sample_transition_probability_matrix(
      dim = M, state_persistent = TRUE
    )
    colnames(Gamma) <- rownames(Gamma) <- paste0("state_", seq_len(M))
  }
  if (is.null(mu)) {
    if (sdds[[1]]$name %in% c("normal", "t", "lognormal")) {
      ### expectation is unrestricted
      mu <- stats::qunif((0:(M - 1) / M + stats::runif(1, 0, 1 / M)), -1, 1) * 
        scale_par[1]
    }
    if (sdds[[1]]$name %in% c("gamma", "poisson")) {
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
  if (sdds[[1]]$name == "poisson") {
    sigma <- NULL 
  }
  if (sdds[[1]]$name == "t") {
    if (is.null(df)) {
      ### degrees of freedom are positive
      df <- stats::qunif((0:(M - 1) / M + stats::runif(1, 0, 1 / M)), 1, 30)
    }
  } else {
    df <- NULL
  }
  if (controls[["hierarchy"]]) {
    if (is.null(Gamma_star)) {
      Gamma_star <- list()
      for (i in 1:M) {
        Gamma_star[[i]] <- oeli::sample_transition_probability_matrix(
          dim = N, state_persistent = TRUE
        )  
      }
    }
    if (is.null(mu_star)) {
      mu_star <- list()
      for (i in 1:M) {
        if (sdds[[2]]$name %in% c("normal", "t", "lognormal")) {
          ### expectation is unrestricted
          mu_star[[i]] <- stats::qunif((0:(N - 1) / N + stats::runif(1, 0, 1 / N)), -1, 1) * 
            scale_par[2]
        }
        if (sdds[[2]]$name %in% c("poisson", "gamma")) {
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
    if (sdds[[2]]$name == "poisson") {
      sigma_star <- NULL 
    }
    if (sdds[[2]]$name == "t") {
      if (is.null(df_star)) {
        df_star <- list()
        for (i in 1:M) {
          ### degrees of freedom are positive
          df_star[[i]] <- stats::qunif((0:(N - 1) / N + stats::runif(1, 0, 1 / N)), 1, 30)
        }
      }
    } else {
      df_star <- NULL
    }
  }
  
  ### set fixed parameters (if any)
  if ("mu" %in% names(sdds[[1]]$pars)) {
    mu <- sdds[[1]]$pars$mu
    if (length(mu) == 1) {
      mu <- rep(mu, M)
    }
  }
  if (sdds[[1]]$name != "poisson") {
    if ("sigma" %in% names(sdds[[1]]$pars)) {
      sigma <- sdds[[1]]$pars$sigma
      if (length(sigma) == 1) {
        sigma <- rep(sigma, M)
      }
    }
  }
  if (sdds[[1]]$name == "t") {
    if ("df" %in% names(sdds[[1]]$pars)) {
      df <- sdds[[1]]$pars$df
      if (length(df) == 1) {
        df <- rep(df, M)
      }
    }
  }
  if (controls[["hierarchy"]]) {
    if ("mu" %in% names(sdds[[2]]$pars)) {
      mu_star <- sdds[[2]]$pars$mu
      if (length(mu_star) == 1) {
        mu_star <- rep(mu_star, N)
      }
      mu_star <- rep(list(mu_star), M)
    }
    if (sdds[[2]]$name != "poisson") {
      if ("sigma" %in% names(sdds[[2]]$pars)) {
        sigma_star <- sdds[[2]]$pars$sigma
        if (length(sigma_star) == 1) {
          sigma_star <- rep(sigma_star, N)
        }
        sigma_star <- rep(list(sigma_star), M)
      }
    }
    if (sdds[[2]]$name == "t") {
      if ("df" %in% names(sdds[[2]]$pars)) {
        df_star <- sdds[[2]]$pars$df
        if (length(df_star) == 1) {
          df_star <- rep(df_star, N)
        }
        df_star <- rep(list(df_star), M)
      }
    }
  }
  
  ### check parameters
  oeli::assert_transition_probability_matrix(Gamma, dim = M)
  if (sdds[[1]]$name %in% c("t", "normal", "lognormal")) {
    if (!checkmate::test_numeric(mu, len = M)) {
      stop(
        paste("'mu' must be a numeric vector of length", M),
        call. = FALSE
      )
    }
  }
  if (sdds[[1]]$name %in% c("gamma", "poisson")) {
    if (!checkmate::test_numeric(mu, len = M) || any(mu <= 0)) {
      stop(
        paste("'mu' must be a positive numeric vector of length", M),
        call. = FALSE
      )
    }
  }
  if (sdds[[1]]$name != "poisson") {
    if (!checkmate::test_numeric(sigma, len = M, lower = 0)) {
      stop(
        paste("'sigma' must be a positive numeric vector of length", M),
        call. = FALSE
      )
    }
  }
  if (sdds[[1]]$name == "t") {
    if (!checkmate::test_numeric(df, len = M, lower = 0)) {
      stop(
        paste("'df' must be a positive numeric vector of length", M),
        call. = FALSE
      )
    }
  }
  if (controls[["hierarchy"]]) {
    if (!is.list(Gamma_star) || length(Gamma_star) != M) {
      stop(
        paste("'Gamma_star' must be a list of length", M),
        call. = FALSE
      )
    }
    for (i in 1:M) {
      oeli::assert_transition_probability_matrix(
        Gamma_star[[i]], dim = N, .var.name = paste0("Gamma_star[[", i, "]]")
      )
    }
    if (!is.list(mu_star) || length(mu_star) != M) {
      stop(
        paste("'mu_star' must be a list of length", M),
        call. = FALSE
      )
    }
    for (i in 1:M) {
      if (sdds[[2]]$name %in% c("t", "normal", "lognormal")) {
        if (!checkmate::test_numeric(mu_star[[i]], len = N)) {
          stop(
            paste("Element", i, "in 'mu_star' must be a numeric vector of length", N),
            call. = FALSE
          )
        }
      }
      if (sdds[[2]]$name %in% c("gamma", "poisson")) {
        if (!checkmate::test_numeric(mu_star[[i]], len = N) || any(mu_star[[i]] <= 0)) {
          stop(
            paste("Element", i, "in 'mu_star' must be a positive numeric vector of length", N),
            call. = FALSE
          )
        }
      }
    }
    if (sdds[[2]]$name != "poisson") {
      if (!is.list(sigma_star) || length(sigma_star) != M) {
        stop(
          paste("'sigma_star' must be a list of length", M),
          call. = FALSE
        )
      }
      for (i in 1:M) {
        if (!checkmate::test_numeric(sigma_star[[i]], len = N, lower = 0)) {
          stop(
            paste("Element", i, "in 'sigma_star' must be a positive numeric vector of length", N),
            call. = FALSE
          )
        }
      }
    }
    if (sdds[[2]]$name == "t") {
      if (!is.list(df_star) || length(df_star) != M) {
        stop(
          paste("'df_star' must be a list of length", M),
          call. = FALSE
        )
      }
      for (i in 1:M) {
        if (!checkmate::test_numeric(df_star[[i]], len = N, lower = 0)) {
          stop(
            paste("Element", i, "in 'df_star' must be a positive numeric vector of length", N),
            call. = FALSE
          )
        }
      }
    }
  }
  
  ### build 'fHMM_parameters' with names
  out <- list("sdds" = sdds)
  if (!is.null(Gamma)) {
    colnames(Gamma) <- rownames(Gamma) <- paste0("state_", seq_len(M))
    out <- c(out, list("Gamma" = Gamma))
  }
  if (!is.null(mu)) {
    names(mu) <- paste0("muCon_", seq_along(mu))
    out <- c(out, list("mu" = mu))
  }
  if (!is.null(sigma)) {
    names(sigma) <- paste0("sigmaCon_", seq_along(sigma))
    out <- c(out, list("sigma" = sigma))
  }
  if (!is.null(df)) {
    names(df) <- paste0("dfCon_", seq_along(df))
    out <- c(out, list("df" = df))
  }
  if (controls[["hierarchy"]]) {
    if (!is.null(Gamma_star)) {
      Gamma_star <- lapply(
        seq_len(M), function(x) {
          Gamma <- Gamma_star[[x]]
          colnames(Gamma) <- rownames(Gamma) <- paste0("fs_state_", seq_len(N))
          return(Gamma)
        }
      )
      names(Gamma_star) <- paste0("cs_state_", seq_len(M))
      out <- c(out, list("Gamma_star" = Gamma_star))
    }
    if (!is.null(mu_star)) {
      mu_star <- lapply(
        seq_len(M), function(x) {
          mu <- mu_star[[x]]
          names(mu) <- paste0("cs_", x, ":muCon_", seq_len(N))
          return(mu)
        }
      )
      names(mu_star) <- paste0("cs_state_", seq_len(M))
      out <- c(out, list("mu_star" = mu_star))
    }
    if (!is.null(sigma_star)) {
      sigma_star <- lapply(
        seq_len(M), function(x) {
          sigma <- sigma_star[[x]]
          names(sigma) <- paste0("cs_", x, ":sigmaCon_", seq_len(N))
          return(sigma)
        }
      )
      names(sigma_star) <- paste0("cs_state_", seq_len(M))
      out <- c(out, list("sigma_star" = sigma_star))
    }
    if (!is.null(df_star)) {
      df_star <- lapply(
        seq_len(M), function(x) {
          df <- df_star[[x]]
          names(df) <- paste0("cs_", x, ":dfCon_", seq_len(N))
          return(df)
        }
      )
      names(df_star) <- paste0("cs_state_", seq_len(M))
      out <- c(out, list("df_star" = df_star))
    }
  }
  structure(out, class = c("fHMM_parameters", "list"))
}

#' @rdname fHMM_parameters
#' @param x
#' An object of class \code{fHMM_parameters}.
#' @param ...
#' Currently not used.
#' @exportS3Method 

print.fHMM_parameters <- function(x, ...) {
  cat("fHMM parameters\n")
  cat(paste0(" $", names(x), collapse = "\n"), "\n")
  invisible(x)
}

#' Parameter transformations
#' 
#' @description
#' These helper functions transform model parameters between 
#' \itemize{
#'   \item constrained spaces (suffix \code{*Con})
#'   \item and unconstrained spaces (suffix \code{*Uncon}).
#' }
#' The former is useful for interpretation, the latter for unconstrained 
#' optimization.
#' 
#' @name parameter_transformations
#' 
#' @inheritParams set_controls
#' 
#' @param par
#' An object of class \code{\link{fHMM_parameters}}, which is a \code{list}
#' of model parameters.
#'
#' @param parCon
#' An object of class \code{parCon}, which is a \code{numeric} \code{vector} 
#' with identified (and constrained) model parameters in the following order:
#' \enumerate{
#'   \item non-diagonal transition probabilities \code{gammasCon}
#'   \item expectations \code{muCon}
#'   \item standard deviations \code{sigmaCon} (if any)
#'   \item degrees of freedom \code{dfCon} (if any)
#'   \item fine-scale parameters for each coarse-scale state, in the same order (if any)
#' }
#'
#' @param parUncon
#' An object of class \code{parUncon}, which is a \code{numeric} \code{vector} 
#' with identified and unconstrained model parameters in the following order:
#' \enumerate{
#'   \item non-diagonal transition probabilities \code{gammasUncon}
#'   \item expectations \code{muUncon}
#'   \item standard deviations \code{sigmaUncon} (if any)
#'   \item degrees of freedom \code{dfUncon} (if any)
#'   \item fine-scale parameters for each coarse-scale state, in the same order (if any)
#' }
#' 
#' @param link
#' Either \code{TRUE} or \code{FALSE}, determining whether to apply the link
#' function.
#' 
#' @param numerical_safeguard
#' Either \code{TRUE} or \code{FALSE}, determining whether to apply the 
#' following small corrections to boundary parameters to improve numerical 
#' performance when calculating and optimizing the likelihood function:
#' - transition probabilities equal to 0 or 1 are shifted towards the center
#'   by \code{1e-3}
#' - standard deviations and degrees of freedom are bounded above by \code{100}
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
#' @param prefix
#' A \code{character} prefix for labeling the parameters.
#' @param use_parameter_labels
#' Either \code{TRUE} to label the parameters or \code{FALSE}, if not (this can
#' save computation time).
#' 
#' @keywords internal

NULL

#' @rdname parameter_transformations
#' @return
#' For \code{par2parUncon}: a vector of unconstrained model parameters.
#' @export

par2parUncon <- function(par, controls, use_parameter_labels = TRUE) {
  stopifnot(inherits(par, "fHMM_parameters"))
  stopifnot(inherits(controls, "fHMM_controls"))
  sdds <- controls[["sdds"]]
  states <- controls[["states"]]
  parUncon <- Gamma2gammasUncon(
    par[["Gamma"]], 
    prefix = "gammasUncon_",
    use_parameter_labels = use_parameter_labels
  )
  if (!"mu" %in% names(sdds[[1]]$pars)) {
    parUncon <- c(
      parUncon,
      muCon2muUncon(
        muCon = par[["mu"]],
        link = (sdds[[1]]$name %in% c("gamma", "poisson")),
        prefix = "muUncon_",
        use_parameter_labels = use_parameter_labels
      )
    )
  }
  if (sdds[[1]]$name != "poisson") {
    if (!"sigma" %in% names(sdds[[1]]$pars)) {
      parUncon <- c(
        parUncon, sigmaCon2sigmaUncon(
          par[["sigma"]], 
          prefix = "sigmaUncon_",
          use_parameter_labels = use_parameter_labels
        )
      )
    }
  }
  if (sdds[[1]]$name == "t") {
    if (!"df" %in% names(sdds[[1]]$pars)) {
      parUncon <- c(
        parUncon,
        dfCon2dfUncon(
          par[["df"]], 
          prefix = "dfUncon_",
          use_parameter_labels = use_parameter_labels
        )
      )
    }
  }
  if (controls[["hierarchy"]]) {
    for (s in 1:states[1]) {
      parUncon <- c(
        parUncon, 
        Gamma2gammasUncon(
          par[["Gamma_star"]][[s]],
          prefix = paste0("cs_", s, ":gammasUncon_"),
          use_parameter_labels = use_parameter_labels
        )
      )
      if (!"mu" %in% names(sdds[[2]]$pars)) {
        parUncon <- c(
          parUncon,
          muCon2muUncon(
            par[["mu_star"]][[s]],
            link = (sdds[[2]]$name %in% c("gamma", "poisson")),
            prefix = paste0("cs_", s, ":muUncon_"),
            use_parameter_labels = use_parameter_labels
          )
        )
      }
      if (sdds[[2]]$name != "poisson") {
        if (!"sigma" %in% names(sdds[[2]]$pars)) {
          parUncon <- c(
            parUncon,
            sigmaCon2sigmaUncon(
              par[["sigma_star"]][[s]],
              prefix = paste0("cs_", s, ":sigmaUncon_"),
              use_parameter_labels = use_parameter_labels
            )
          )
        }
      }
      if (sdds[[2]]$name == "t") {
        if (!"df" %in% names(sdds[[2]]$pars)) {
          parUncon <- c(
            parUncon,
            dfCon2dfUncon(
              par[["df_star"]][[s]],
              prefix = paste0("cs_", s, ":dfUncon_"),
              use_parameter_labels = use_parameter_labels
            )
          )
        }
      }
    }
  }
  structure(parUncon, class = c("parUncon", "numeric"))
}

#' @rdname parameter_transformations
#' @return
#' For \code{parUncon2parCon}: a vector of constrained model parameters.
#' @export

parUncon2parCon <- function(
    parUncon, controls, use_parameter_labels = TRUE, numerical_safeguard = FALSE
  ) {
  stopifnot(inherits(parUncon, "parUncon"))
  stopifnot(inherits(controls, "fHMM_controls"))
  sdds <- controls[["sdds"]]
  M <- controls[["states"]][1]
  parCon <- gammasUncon2gammasCon(
    parUncon[1:((M - 1) * M)], 
    dim = M,
    prefix = "gammasCon_",
    use_parameter_labels = use_parameter_labels,
    numerical_safeguard = numerical_safeguard
  )
  parUncon <- parUncon[-(1:((M - 1) * M))]
  if (!"mu" %in% names(sdds[[1]]$pars)) {
    parCon <- c(
      parCon,
      muUncon2muCon(
        parUncon[1:M],
        link = (sdds[[1]]$name %in% c("gamma", "poisson")),
        prefix = "muCon_",
        use_parameter_labels = use_parameter_labels
      )
    )
    parUncon <- parUncon[-(1:M)]
  }
  if (sdds[[1]]$name != "poisson") {
    if (!"sigma" %in% names(sdds[[1]]$pars)) {
      parCon <- c(
        parCon,
        sigmaUncon2sigmaCon(
          parUncon[1:M],
          prefix = "sigmaCon_",
          use_parameter_labels = use_parameter_labels,
          numerical_safeguard = numerical_safeguard
        )
      )
      parUncon <- parUncon[-(1:M)]
    }
  }
  if (sdds[[1]]$name == "t") {
    if (!"df" %in% names(sdds[[1]]$pars)) {
      parCon <- c(
        parCon,
        dfUncon2dfCon(
          parUncon[1:M],
          prefix = "dfCon_",
          use_parameter_labels = use_parameter_labels,
          numerical_safeguard = numerical_safeguard
        )
      )
      parUncon <- parUncon[-(1:M)]
    }
  }
  if (controls[["hierarchy"]]) {
    N <- controls[["states"]][2]
    for (s in seq_len(M)) {
      parCon <- c(
        parCon,
        gammasUncon2gammasCon(
          parUncon[1:((N - 1) * N)], 
          dim = N,
          prefix = paste0("cs_", s, ":gammasCon_"),
          use_parameter_labels = use_parameter_labels,
          numerical_safeguard = numerical_safeguard
        )
      )
      parUncon <- parUncon[-(1:((N - 1) * N))]
      if (!"mu" %in% names(sdds[[2]]$pars)) {
        parCon <- c(
          parCon,
          muUncon2muCon(
            parUncon[1:N],
            link = (sdds[[2]]$name %in% c("gamma", "poisson")),
            prefix = paste0("cs_", s, ":muCon_"),
            use_parameter_labels = use_parameter_labels
          )
        )
        parUncon <- parUncon[-(1:N)]
      }
      if (sdds[[2]]$name != "poisson") {
        if (!"sigma" %in% names(sdds[[2]]$pars)) {
          parCon <- c(
            parCon,
            sigmaUncon2sigmaCon(
              parUncon[1:N],
              prefix = paste0("cs_", s, ":sigmaCon_"),
              use_parameter_labels = use_parameter_labels,
              numerical_safeguard = numerical_safeguard
            )
          )
          parUncon <- parUncon[-(1:N)]
        }
      }
      if (sdds[[2]]$name == "t") {
        if (!"df" %in% names(sdds[[2]]$pars)) {
          parCon <- c(
            parCon,
            dfUncon2dfCon(
              parUncon[1:N],
              prefix = paste0("cs_", s, ":dfCon_"),
              use_parameter_labels = use_parameter_labels,
              numerical_safeguard = numerical_safeguard
            )
          )
          parUncon <- parUncon[-(1:N)]
        }
      }
    }
  }
  structure(parCon, class = c("parCon", "numeric"))
}

#' @rdname parameter_transformations
#' @return
#' For \code{parCon2par}: an object of class \code{\link{fHMM_parameters}}.
#' @export

parCon2par <- function(parCon, controls, use_parameter_labels = TRUE) {
  
  parCon_tmp <- parCon
  
  stopifnot(inherits(parCon, "parCon"))
  stopifnot(inherits(controls, "fHMM_controls"))
  sdds <- controls[["sdds"]]
  M <- controls[["states"]][1]
  Gamma <- gammasCon2Gamma(
    parCon[1:((M - 1) * M)], 
    M, 
    use_parameter_labels = use_parameter_labels
  )
  parCon <- parCon[-(1:((M - 1) * M))]
  if (!"mu" %in% names(sdds[[1]]$pars)) {
    mu <- parCon[1:M]
    parCon <- parCon[-(1:M)]
  } else {
    mu <- sdds[[1]]$pars$mu
  }
  if (sdds[[1]]$name != "poisson") {
    if (!"sigma" %in% names(sdds[[1]]$pars)) {
      sigma <- parCon[1:M]
      parCon <- parCon[-(1:M)]
    } else {
      sigma <- sdds[[1]]$pars$sigma
    }
  }
  if (sdds[[1]]$name == "t") {
    if (!"df" %in% names(sdds[[1]]$pars)) {
      df <- parCon[1:M]
      parCon <- parCon[-(1:M)]
    } else {
      df <- sdds[[1]]$pars$df
    }
  } else {
    df <- NULL
  }
  if (controls[["hierarchy"]]) {
    N <- controls[["states"]][2]
    Gamma_star <- list()
    mu_star <- list()
    sigma_star <- list()
    if (sdds[[2]]$name == "t") {
      df_star <- list()
    } else {
      df_star <- NULL
    }
    for (s in 1:M) {
      Gamma_star[[s]] <- gammasCon2Gamma(
        parCon[1:((N - 1) * N)], 
        N,
        use_parameter_labels = use_parameter_labels
      )
      parCon <- parCon[-(1:((N - 1) * N))]
      if (!"mu" %in% names(sdds[[2]]$pars)) {
        mu_star[[s]] <- parCon[1:N]
        parCon <- parCon[-(1:N)]
      } else {
        mu_star[[s]] <- sdds[[2]]$pars$mu
      }
      if (sdds[[2]]$name != "poisson") {
        if (!"sigma" %in% names(sdds[[2]]$pars)) {
          sigma_star[[s]] <- parCon[1:N]
          parCon <- parCon[-(1:N)]
        } else {
          sigma_star[[s]] <- sdds[[2]]$pars$sigma
        }
      }
      if (sdds[[2]]$name == "t") {
        if (!"df" %in% names(sdds[[2]]$pars)) {
          df_star[[s]] <- parCon[1:N]
          parCon <- parCon[-(1:N)]
        } else {
          df_star[[s]] <- sdds[[2]]$pars$df
        }
      }
    }
  } else {
    Gamma_star <- NULL
    mu_star <- NULL
    sigma_star <- NULL
    df_star <- NULL
  }
  fHMM_parameters(
    controls = controls,
    Gamma = Gamma, mu = mu, sigma = sigma, df = df,
    Gamma_star = Gamma_star, mu_star = mu_star,
    sigma_star = sigma_star, df_star = df_star,
    check_controls = FALSE
  )
}

#' @rdname parameter_transformations
#' @return
#' For \code{par2parCon}: a vector of constrained model parameters.
#' @export

par2parCon <- function(par, controls, use_parameter_labels = TRUE) {
  stopifnot(inherits(par, "fHMM_parameters"))
  stopifnot(inherits(controls, "fHMM_controls"))
  parUncon2parCon(
    par2parUncon(par, controls, use_parameter_labels = use_parameter_labels), 
    controls,
    use_parameter_labels = use_parameter_labels
  )
}

#' @rdname parameter_transformations
#' @return
#' For \code{parCon2parUncon}: a vector of unconstrained model parameters.
#' @export

parCon2parUncon <- function(parCon, controls, use_parameter_labels = TRUE) { 
  stopifnot(inherits(parCon, "parCon"))
  stopifnot(inherits(controls, "fHMM_controls"))
  par2parUncon(
    parCon2par(parCon, controls, use_parameter_labels = use_parameter_labels), 
    controls,
    use_parameter_labels = use_parameter_labels
  )
}

#' @rdname parameter_transformations
#' @return
#' For \code{parUncon2par}: an object of class \code{fHMM_parameters}.
#' @export

parUncon2par <- function(
    parUncon, controls, use_parameter_labels = TRUE, numerical_safeguard = FALSE
  ) {
  stopifnot(inherits(parUncon, "parUncon"))
  stopifnot(inherits(controls, "fHMM_controls"))
  parCon2par(
    parUncon2parCon(
      parUncon, controls, use_parameter_labels = use_parameter_labels,
      numerical_safeguard = numerical_safeguard
    ), 
    controls,
    use_parameter_labels = use_parameter_labels
  )
}

#' @rdname parameter_transformations
#' @return
#' For \code{muCon2muUncon}: a vector of unconstrained expected values.

muCon2muUncon <- function(
    muCon, link, prefix = "muUncon_", use_parameter_labels = TRUE
  ) {
  muUncon <- if (link) {
    log(muCon)
  } else {
    muCon
  }
  if (isTRUE(use_parameter_labels)) {
    names(muUncon) <- paste0(prefix, seq_along(muUncon))
  }
  return(muUncon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{muUncon2muCon}: a vector of constrained expected values.

muUncon2muCon <- function(
    muUncon, link, prefix = "muCon_", use_parameter_labels = TRUE
  ) {
  muCon <- if (link) {
    exp(muUncon)
  } else {
    muUncon
  }
  if (isTRUE(use_parameter_labels)) {
    names(muCon) <- paste0(prefix, seq_along(muCon))
  }
  return(muCon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{sigmaCon2sigmaUncon}: a vector of unconstrained standard 
#' deviations.

sigmaCon2sigmaUncon <- function(
    sigmaCon, prefix = "sigmaUncon_", use_parameter_labels = TRUE
  ) {
  sigmaUncon <- log(sigmaCon)
  if (isTRUE(use_parameter_labels)) {
    names(sigmaUncon) <- paste0(prefix, seq_along(sigmaUncon))
  }
  return(sigmaUncon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{sigmaUncon2sigmaCon}: a vector of constrained standard deviations.

sigmaUncon2sigmaCon <- function(
    sigmaUncon, prefix = "sigmaCon_", use_parameter_labels = TRUE,
    numerical_safeguard = FALSE
  ) {
  sigmaCon <- exp(sigmaUncon)
  if (isTRUE(numerical_safeguard)) {
    sigmaCon[sigmaCon > 100] <- 100
  }
  if (isTRUE(use_parameter_labels)) {
    names(sigmaCon) <- paste0(prefix, seq_along(sigmaCon))
  }
  return(sigmaCon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{dfCon2dfUncon}: a vector of unconstrained degrees of freedom.

dfCon2dfUncon <- function(
    dfCon, prefix = "dfUncon_", use_parameter_labels = TRUE
  ) {
  dfUncon <- log(dfCon)
  if (isTRUE(use_parameter_labels)) {
    names(dfUncon) <- paste0(prefix, seq_along(dfUncon))
  }
  return(dfUncon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{dfUncon2dfCon}: a vector of constrained degrees of freedom.

dfUncon2dfCon <- function(
    dfUncon, prefix = "dfCon_", use_parameter_labels = TRUE,
    numerical_safeguard = FALSE
  ) {
  dfCon <- exp(dfUncon)
  if (isTRUE(numerical_safeguard)) {
    dfCon[dfCon > 100] <- 100
  }
  if (isTRUE(use_parameter_labels)) {
    names(dfCon) <- paste0(prefix, seq_along(dfCon))
  }
  return(dfCon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{Gamma2gammasCon}: a vector of constrained non-diagonal matrix 
#' elements (column-wise).

Gamma2gammasCon <- function(
    Gamma, prefix = "gammasCon_", use_parameter_labels = TRUE,
    numerical_safeguard = FALSE
  ) {
  gammasCon <- Gamma[row(Gamma) != col(Gamma)]
  if (isTRUE(numerical_safeguard)) {
    gammasCon <- replace(gammasCon, gammasCon == 0, 1e-3)
    gammasCon <- replace(gammasCon, gammasCon == 1, 1 - 1e-3)
  }
  if (isTRUE(use_parameter_labels)) {
    names(gammasCon) <- oeli::matrix_indices(
      Gamma, prefix = prefix, exclude_diagonal = TRUE
    )
  }
  return(gammasCon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{Gamma2gammasUncon}: a vector of unconstrained non-diagonal matrix 
#' elements (column-wise).

Gamma2gammasUncon <- function(
    Gamma, prefix = "gammasUncon_", use_parameter_labels = TRUE
  ) {
  diag(Gamma) <- 0
  Gamma <- log(Gamma / (1 - rowSums(Gamma)))
  diag(Gamma) <- NA_real_
  gammasUncon <- Gamma[!is.na(Gamma)]
  if (isTRUE(use_parameter_labels)) {
    names(gammasUncon) <- oeli::matrix_indices(
      Gamma, prefix = prefix, exclude_diagonal = TRUE
    )
  }
  return(gammasUncon)
}

#' @rdname parameter_transformations
#' @return
#' For \code{gammasCon2Gamma}: a transition probability matrix.

gammasCon2Gamma <- function(
    gammasCon, dim, prefix = "state_", use_parameter_labels = TRUE
  ) {
  Gamma <- diag(dim)
  Gamma[!Gamma] <- gammasCon
  for (i in 1:dim) {
    Gamma[i, i] <- 1 - (rowSums(Gamma)[i] - 1)
  }
  if (isTRUE(use_parameter_labels)) {
    colnames(Gamma) <- rownames(Gamma) <- paste0(prefix, seq_len(dim))
  }
  return(Gamma)
}

#' @rdname parameter_transformations
#' @return
#' For \code{gammasCon2gammasUncon}: a vector of unconstrained non-diagonal 
#' elements of the transition probability matrix.

gammasCon2gammasUncon <- function(
    gammasCon, dim, prefix = "gammasUncon_", use_parameter_labels = TRUE
  ) {
  Gamma2gammasUncon(
    gammasCon2Gamma(gammasCon, dim, use_parameter_labels = use_parameter_labels), 
    prefix = prefix,
    use_parameter_labels = use_parameter_labels
  )
}

#' @rdname parameter_transformations
#' @return
#' For \code{gammasUncon2Gamma}: a transition probability matrix.

gammasUncon2Gamma <- function(
    gammasUncon, dim, prefix = "state_", use_parameter_labels = TRUE,
    numerical_safeguard = FALSE
  ) {
  Gamma <- diag(dim)
  Gamma[!Gamma] <- exp(gammasUncon)
  if (isTRUE(numerical_safeguard)) {
    Gamma[!is.finite(Gamma)] <- 100
  }
  Gamma <- Gamma / rowSums(Gamma)
  if (isTRUE(use_parameter_labels)) {
    colnames(Gamma) <- rownames(Gamma) <- paste0(prefix, seq_len(dim))
  }
  return(Gamma)
}

#' @rdname parameter_transformations
#' @return
#' For \code{gammasUncon2gammasCon}: a vector of constrained non-diagonal 
#' elements of a transition probability matrix.

gammasUncon2gammasCon <- function(
    gammasUncon, dim, prefix = "gammasCon_", use_parameter_labels = TRUE,
    numerical_safeguard = FALSE
  ) {
  Gamma2gammasCon(
    gammasUncon2Gamma(
      gammasUncon, dim, use_parameter_labels = use_parameter_labels,
      numerical_safeguard = numerical_safeguard
    ), 
    prefix = prefix,
    use_parameter_labels = use_parameter_labels,
    numerical_safeguard = numerical_safeguard
  )
}