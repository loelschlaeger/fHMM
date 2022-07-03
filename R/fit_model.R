#' Model fitting
#'
#' @description
#' This function fits a HMM to \code{data} via maximum likelihood estimation.
#'
#' @details
#' The function is parallelized only if \code{ncluster > 1}.
#'
#' @param data
#' An object of class \code{fHMM_data}.
#' @param seed
#' Set a seed for the sampling of initial values.
#' @param ncluster
#' Set the number of clusters for parallelization.
#' @param verbose
#' Set to \code{TRUE} to print progress messages.
#' @param init
#' Optionally an object of class \code{parUncon} for initialization. This can
#' for example be the estimate of a previously fitted model \code{model}, i.e.
#' the element \code{model$estimate}. The initial values are computed via
#' \code{replicate(n, jitter(init, amount = 1), simplify = FALSE)},
#' where \code{n <- data$controls$fit$runs}.
#'
#' @return
#' An object of class \code{fHMM_model}.
#'
#' @export
#'
#' @importFrom stats sd nlm
#' @importFrom foreach %dopar%

fit_model <- function(data, ncluster = 1, seed = NULL, verbose = TRUE,
                      init = NULL) {

  ### check inputs
  if (!inherits(data, "fHMM_data")) {
    stop("'data' is not of class 'fHMM_data'.")
  }
  if (!is_number(ncluster, int = TRUE, pos = TRUE)) {
    stop("'ncluster' must be a positive integer.")
  }
  if (!isTRUE(verbose) && !isFALSE(verbose)) {
    stop("'verbose' must be either TRUE or FALSE.")
  }

  ### set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### generate start values
  if (!is.null(init)) {
    start_values <- replicate(data$controls$fit$runs, jitter(init), simplify = FALSE)
  } else {
    start_values <- list()
    if (data[["controls"]][["fit"]][["origin"]]) {
      start_values[[1]] <- par2parUncon(data[["true_parameters"]], data[["controls"]])
    } else {
      ### compute parameter scales based on the method of moments
      scale_par <- c(1, 1)
      if (!data[["controls"]][["hierarchy"]]) {
        scale_par[1] <- mean(c(mean(data[["data"]], na.rm = "TRUE"), stats::sd(data[["data"]], na.rm = "TRUE")))
      } else {
        scale_par[1] <- mean(c(mean(data[["data"]][, 1], na.rm = "TRUE"), stats::sd(data[["data"]][, 1], na.rm = "TRUE")))
        scale_par[2] <- mean(c(mean(data[["data"]][, -1], na.rm = "TRUE"), stats::sd(data[["data"]][, -1], na.rm = "TRUE")))
      }
      scale_par <- abs(scale_par)
      for (run in 1:data[["controls"]][["fit"]][["runs"]]) {
        start_values[[run]] <- par2parUncon(
          fHMM_parameters(controls = data[["controls"]], scale_par = scale_par),
          data[["controls"]]
        )
      }
    }
  }

  ### define likelihood function
  target <- ifelse(!data[["controls"]][["hierarchy"]], nLL_hmm, nLL_hhmm)

  ### check start values
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent, :eta ETA",
      total = data[["controls"]][["fit"]][["runs"]], width = 45, clear = TRUE,
      complete = "=", incomplete = "-", current = ">"
    )
    pb$message("Checking start values")
  }
  ll_at_start_values <- rep(NA_real_, data[["controls"]][["fit"]][["runs"]])
  if (ncluster == 1) {
    for (run in 1:data[["controls"]][["fit"]][["runs"]]) {
      if (verbose) pb$tick(0)
      ll <- target(
        parUncon = start_values[[run]],
        observations = data[["data"]],
        controls = data[["controls"]]
      )
      if (!(is.na(ll) || is.nan(ll) || abs(ll) > 1e100)) {
        ll_at_start_values[run] <- ll
      }
      if (verbose) pb$tick()
    }
  } else if (ncluster > 1) {
    cluster <- parallel::makeCluster(ncluster)
    doSNOW::registerDoSNOW(cluster)
    opts <- if (verbose) list(progress = function(n) pb$tick()) else list()
    ll_at_start_values <- foreach::foreach(
      run = 1:data[["controls"]][["fit"]][["runs"]],
      .packages = "fHMM", .options.snow = opts
    ) %dopar% {
      ll <- target(
        parUncon = start_values[[run]],
        observations = data[["data"]],
        controls = data[["controls"]]
      )
      if (verbose) pb$tick()
      if (!(is.na(ll) || is.nan(ll) || abs(ll) > 1e100)) {
        ll
      } else {
        NA_real_
      }
    }
    parallel::stopCluster(cluster)
    ll_at_start_values <- unlist(ll_at_start_values)
  }
  if (sum(is.na(ll_at_start_values)) == data[["controls"]][["fit"]][["runs"]]) {
    stop("The likelihood function could not be computed at any of the selected start values, try to increase 'runs' in 'controls'.")
  }
  if (sum(is.na(ll_at_start_values)) > 0.5 * data[["controls"]][["fit"]][["runs"]]) {
    warning("The likelihood function could not be computed at more than half of the selected start values, try to increase 'runs' in 'controls'.", immediate. = TRUE)
  }
  runs_seq <- which(!is.na(ll_at_start_values))

  ### start optimization
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent, :eta ETA",
      total = data[["controls"]][["fit"]][["runs"]], width = 45, clear = TRUE,
      complete = "=", incomplete = "-", current = ">"
    )
    pb$message("Maximizing likelihood")
  }
  start_time <- Sys.time()
  if (ncluster == 1) {
    mods <- list()
    for (run in 1:data[["controls"]][["fit"]][["runs"]]) {
      if (verbose) pb$tick(0)
      if (!is.na(ll_at_start_values[run])) {
        suppressWarnings({
          mod <- try(
            stats::nlm(
              f = target,
              p = start_values[[run]],
              observations = data[["data"]],
              controls = data[["controls"]],
              iterlim = data[["controls"]][["fit"]][["iterlim"]],
              steptol = data[["controls"]][["fit"]][["steptol"]],
              gradtol = data[["controls"]][["fit"]][["gradtol"]],
              print.level = data[["controls"]][["fit"]][["print.level"]],
              typsize = start_values[[run]],
              hessian = FALSE
            ),
            silent = TRUE
          )
        })
      } else {
        mod <- NA
      }
      if (!identical(mod, NA) && !inherits(mod, "try-error") &&
        mod[["code"]] %in% data[["controls"]][["fit"]][["accept"]]) {
        mods[[run]] <- mod
      } else {
        mods[[run]] <- NA
      }
      if (verbose) pb$tick()
    }
  } else if (ncluster > 1) {
    cluster <- parallel::makeCluster(ncluster)
    doSNOW::registerDoSNOW(cluster)
    opts <- if (verbose) list(progress = function(n) pb$tick()) else list()
    mods <- foreach::foreach(
      run = 1:data[["controls"]][["fit"]][["runs"]],
      .packages = "fHMM", .options.snow = opts
    ) %dopar% {
      if (verbose) pb$tick(0)
      if (!is.na(ll_at_start_values[run])) {
        suppressWarnings({
          mod <- try(
            stats::nlm(
              f = target,
              p = start_values[[run]],
              observations = data[["data"]],
              controls = data[["controls"]],
              iterlim = data[["controls"]][["fit"]][["iterlim"]],
              steptol = data[["controls"]][["fit"]][["steptol"]],
              gradtol = data[["controls"]][["fit"]][["gradtol"]],
              print.level = data[["controls"]][["fit"]][["print.level"]],
              typsize = start_values[[run]],
              hessian = FALSE
            ),
            silent = TRUE
          )
        })
        if (verbose) pb$tick()
        if (!inherits(mod, "try-error") &&
          mod[["code"]] %in% data[["controls"]][["fit"]][["accept"]]) {
          mod
        } else {
          NA
        }
      } else {
        NA
      }
    }
    parallel::stopCluster(cluster)
  }
  end_time <- Sys.time()

  ### save and check likelihood values
  lls <- -unlist(sapply(mods, `[`, "minimum"), use.names = FALSE)
  if (all(is.na(lls))) {
    stop("None of the estimation runs ended successfully, adapt 'accept' or increase 'runs' in 'controls'.")
  }

  ### compute Hessian
  if (verbose) message("Computing Hessian")
  hessian <- suppressWarnings(
    stats::nlm(
      f = target,
      p = mods[[which.max(lls)]][["estimate"]],
      observations = data[["data"]],
      controls = data[["controls"]],
      iterlim = 1,
      hessian = TRUE,
      typsize = mods[[which.max(lls)]][["estimate"]]
    )[["hessian"]]
  )

  ### final message
  if (verbose) message("Fitting completed")

  ### extract estimation results
  mod <- mods[[which.max(lls)]]
  ll <- -mod[["minimum"]]
  estimate <- mod[["estimate"]]
  class(estimate) <- "parUncon"
  estimation_time <- ceiling(difftime(end_time, start_time, units = "mins"))

  ### create and return 'fHMM_model' object
  out <- list(
    "data" = data,
    "estimate" = estimate,
    "nlm_output" = mod,
    "estimation_time" = estimation_time,
    "ll" = ll,
    "lls" = lls,
    "gradient" = mod$gradient,
    "hessian" = hessian,
    "decoding" = NULL
  )
  class(out) <- "fHMM_model"
  return(out)
}

#' @noRd
#' @export

print.fHMM_model <- function(x, ...) {
  cat("fHMM fitted model:\n")
  cat("* total estimation time:", x$estimation_time, units(x$estimation_time), "\n")
  cat("* accepted runs:", sum(!is.na(x$lls)), "of", length(x$lls), "\n")
  cat("* log-likelihood:", x$ll, "\n")
  return(invisible(x))
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
#' @keywords
#' internal
#'
#' @importFrom stats dgamma dt

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
  delta <- Gamma2delta(Gamma)
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
      allprobs[m, ] <- 1 / sigmas[m] * stats::dt((observations_cs - mus[m]) / sigmas[m], dfs[m])
    }
    if (controls[["sdds"]][[1]]$name == "gamma") {
      allprobs[m, ] <- stats::dgamma(observations_cs,
        shape = mus[m]^2 / sigmas[m]^2,
        scale = sigmas[m]^2 / mus[m]
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
