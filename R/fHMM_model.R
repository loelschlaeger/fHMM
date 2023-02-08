#' Constructor of a model object
#'
#' @description
#' This function constructs an object of class \code{\link{fHMM_model}}, which 
#' contains details about the fitted (hierarchical) Hidden Markov model.
#' 
#' @param data
#' An object of class \code{\link{fHMM_data}}.
#' @param estimate
#' A \code{numeric} vector of unconstrained model estimates.
#' @param nlm_output
#' The output of \code{\link[stats]{nlm}} for the selected optimization run.
#' @param estimation_time
#' A \code{diff.time} object, the total estimation time.
#' @param ll
#' A \code{numeric}, the model log-likelihood.
#' @param lls
#' A \code{numeric} vector, the model log-likelihoods in all optimization runs.
#' @param gradient
#' A \code{numeric} vector, the gradient at the optimum.
#' @param hessian
#' A \code{matrix}, the Hessian at the optimum.
#' @param decoding
#' A \code{numeric} vector, the decoded time series.
#' 
#' @return 
#' An object of class \code{\link{fHMM_model}}.

fHMM_model <- function(
    data, estimate, nlm_output, estimation_time, ll, lls, gradient, hessian, 
    decoding
) {
  structure(
    list(
      "data" = data,
      "estimate" = estimate,
      "nlm_output" = nlm_output,
      "estimation_time" = estimation_time,
      "ll" = ll,
      "lls" = lls,
      "gradient" = gradient,
      "hessian" = hessian,
      "decoding" = NULL
    ), 
    class = "fHMM_model"
  )
}

#' Model fitting
#'
#' @description
#' This function fits a HMM to \code{\link{fHMM_data}} via numerical likelihood 
#' maximization.
#'
#' @details
#' The function is parallelized if \code{ncluster > 1}.
#'
#' @param data
#' An object of class \code{\link{fHMM_data}}.
#' @param seed
#' Set a seed for the sampling of initial values.
#' No seed by default.
#' @param ncluster
#' Set the number of clusters for parallelization.
#' By default, \code{ncluster = 1}.
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
#' An object of class \code{\link{fHMM_model}}.
#'
#' @export
#'
#' @importFrom stats sd nlm
#' @importFrom foreach %dopar%

fit_model <- function(
    data, ncluster = 1, seed = NULL, verbose = TRUE, init = NULL
  ) {

  ### check inputs
  if (!inherits(data, "fHMM_data")) {
    stop("'data' is not of class 'fHMM_data'.", 
         call. = FALSE)
  }
  if (!is_number(ncluster, int = TRUE, pos = TRUE)) {
    stop("'ncluster' must be a positive integer.", 
         call. = FALSE)
  }
  if (!isTRUE(verbose) && !isFALSE(verbose)) {
    stop("'verbose' must be either TRUE or FALSE.", 
         call. = FALSE)
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
        scale_par[1] <- mean(c(mean(data[["data"]], na.rm = TRUE), stats::sd(data[["data"]], na.rm = TRUE)))
      } else {
        scale_par[1] <- mean(c(mean(data[["data"]][, 1], na.rm = TRUE), stats::sd(data[["data"]][, 1], na.rm = TRUE)))
        scale_par[2] <- mean(c(mean(data[["data"]][, -1], na.rm = TRUE), stats::sd(data[["data"]][, -1], na.rm = TRUE)))
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
    stop("The likelihood function could not be computed at any of the selected start values, try to increase 'runs' in 'controls'.", 
         call. = FALSE)
  }
  if (sum(is.na(ll_at_start_values)) > 0.5 * data[["controls"]][["fit"]][["runs"]]) {
    warning("The likelihood function could not be computed at more than half of the selected start values, try to increase 'runs' in 'controls'.", 
            call. = FALSE, immediate. = TRUE)
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
    stop("None of the estimation runs ended successfully, adapt 'accept' or increase 'runs' in 'controls'.", 
         call. = FALSE)
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
  fHMM_model(
    data = data,
    estimate = estimate,
    nlm_output = mod,
    estimation_time = estimation_time,
    ll = ll,
    lls = lls,
    gradient = mod$gradient,
    hessian = hessian,
    decoding = NULL
  )
}

#' @rdname fit_model
#' @param x
#' An object of class \code{\link{fHMM_model}}.
#' @param ...
#' Currently not used.
#' @exportS3Method 

print.fHMM_model <- function(x, ...) {
  cat("fHMM fitted model:\n")
  cat("* total estimation time:", x$estimation_time, units(x$estimation_time), "\n")
  cat("* accepted runs:", sum(!is.na(x$lls)), "of", length(x$lls), "\n")
  cat("* log-likelihood:", x$ll, "\n")
  invisible(x)
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

#' Residuals
#'
#' @description
#' This function extracts the computed (pseudo-) residuals of
#' an \code{\link{fHMM_model}} object.
#'
#' @param object
#' An object of class \code{\link{fHMM_model}}.
#' @param ...
#' Ignored.
#'
#' @return
#' A vector (or a matrix, in case of an hierarchical HMM) with (pseudo-)
#' residuals for each observation.
#'
#' @examples
#' compute_residuals(dax_model_3t)
#' res <- residuals(dax_model_3t)
#' head(res)
#' 
#' @export

residuals.fHMM_model <- function(object, ...) {
  
  ### check input
  if (!inherits(object,"fHMM_model")) {
    stop("'object' must be of class 'fHMM_model'.", call. = FALSE)
  }
  if (is.null(object[["residuals"]])) {
    stop("No residuals contained in 'object'.",
         "Please call 'compute_residuals()' first. ", call. = FALSE)
  }
  
  ### extract residuals
  return(object[["residuals"]])
}

#' @noRd
#' @export

summary.fHMM_model <- function(object, alpha = 0.05, ...) {
  
  ### model information
  simulated <- object$data$controls$simulated
  hierarchy <- object$data$controls$hierarchy
  no_par <- npar(object)
  data_size <- nobs(object)
  ll <- logLik(object)
  aic <- AIC(object)
  bic <- BIC(object)
  model_info <- data.frame(
    simulated, hierarchy,
    "LL" = ll, "AIC" = aic, "BIC" = bic
  )
  
  ### state-dependent distributions
  sdds <- parUncon2par(object$estimate, object$data$controls)$sdds
  
  ### parameter estimates
  estimates_table <- coef.fHMM_model(object, alpha)
  
  ### states
  if (!is.null(object$decoding)) {
    if (simulated) {
      if (!hierarchy) {
        decoding_table <- table(object$data$markov_chain, object$decoding,
                                dnn = c("true", "decoded")
        )
      } else {
        decoding_table_cs <- table(object$data$markov_chain[, 1],
                                   object$decoding[, 1],
                                   dnn = c("true", "decoded")
        )
        decoding_table_fs <- table(object$data$markov_chain[, -1],
                                   object$decoding[, -1],
                                   dnn = c("true", "decoded")
        )
        decoding_table <- list(
          "coarse-scale" = decoding_table_cs,
          "fine-scale" = decoding_table_fs
        )
      }
    } else {
      if (!hierarchy) {
        decoding_table <- table(object$decoding, dnn = "decoded")
      } else {
        decoding_table_cs <- table(object$decoding[, 1], dnn = "decoded")
        decoding_table_fs <- table(object$decoding[, -1], dnn = "decoded")
        decoding_table <- list(
          "coarse-scale" = decoding_table_cs,
          "fine-scale" = decoding_table_fs
        )
      }
    }
  } else {
    decoding_table <- NULL
  }
  
  ### residuals
  if (!is.null(object$residuals)) {
    if (!hierarchy) {
      res_summary <- summary(object$residuals)
    } else {
      res_summary_cs <- summary(object$residuals[, 1])
      res_summary_fs <- summary(as.vector(object$residuals[, -1]))
      res_summary <- list(
        "coarse-scale" = res_summary_cs,
        "fine-scale" = res_summary_fs
      )
    }
  } else {
    res_summary <- NULL
  }
  
  ### build and return summary
  out <- list(
    "no_par" = no_par,
    "data_size" = data_size,
    "model_info" = model_info,
    "sdds" = sdds,
    "estimates_table" = estimates_table,
    "decoding_table" = decoding_table,
    "res_summary" = res_summary
  )
  class(out) <- "summary.fHMM_model"
  return(out)
}

#' @noRd
#' @export

print.summary.fHMM_model <- function(x, digits = 4, ...) {
  cat("Summary of fHMM model\n\n")
  print(x$model_info)
  cat("\nState-dependent distributions:\n")
  print(x$sdds)
  cat("\n")
  cat("\nEstimates:\n")
  print(x$estimates_table, digits = digits)
  if (!is.null(x$decoding_table)) {
    cat("\nStates:\n")
    print(x$decoding_table, digits = digits)
  }
  if (!is.null(x$res_summary)) {
    cat("\nResiduals:\n")
    print(x$res_summary, digits = digits)
  }
  return(invisible(x))
}

#' Model coefficients
#'
#' @description
#' This function returns the estimated model coefficients and an \code{alpha}
#' confidence interval.
#'
#' @param object
#' An object of class \code{\link{fHMM_model}}.
#' @param digits
#' An \code{integer}, the number of significant digits to be used.
#' By default, \code{digits = 2}.
#' @param ...
#' Ignored.
#' @inheritParams compute_ci
#'
#' @return
#' A \code{data.frame}.
#'
#' @exportS3Method 

coef.fHMM_model <- function(object, alpha = 0.05, digits = 2, ...) {
  ci <- compute_ci(object, alpha)
  estimates_table <- data.frame(lapply(ci, as.vector))
  if (object$data$controls$simulated) {
    true <- par2parCon(object$data$true_parameters, object$data$controls)
    estimates_table <- cbind(estimates_table, true = as.vector(true))
  }
  rownames(estimates_table) <- parameter_labels(
    controls = object$data$controls, expected_length = nrow(estimates_table)
  )
  return(estimates_table)
}

#' @exportS3Method 
#' @importFrom stats AIC

AIC.fHMM_model <- function(object, ..., k = 2) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  ll <- sapply(models, logLik.fHMM_model)
  npar <- sapply(models, npar)
  aic <- mapply(function(ll, npar) -2 * ll + 2 * npar, ll, npar)
  return(aic)
}

#' @exportS3Method 
#' @importFrom stats BIC

BIC.fHMM_model <- function(object, ...) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  ll <- sapply(models, logLik)
  npar <- sapply(models, npar)
  nobs <- sapply(models, nobs)
  bic <- mapply(function(ll, npar, nobs) -2 * ll + npar * log(nobs), ll, npar, 
                nobs)
  return(bic)
}

#' @exportS3Method 
#' @importFrom stats nobs

nobs.fHMM_model <- function(object, ...) {
  return(length(as.vector(object$data$data)))
}

#' @exportS3Method 
#' @importFrom stats logLik

logLik.fHMM_model <- function(object, ...) {
  return(object$ll)
}

#' Number of model parameters
#'
#' @description
#' This function extracts the number of model parameters of an 
#' \code{\link{fHMM_model}} object.
#'
#' @param object
#' An object of class \code{\link{fHMM_model}}.
#'
#' @param ...
#' Optionally more objects of class \code{\link{fHMM_model}}.
#'
#' @return
#' Either a numeric value (if just one object is provided) or a numeric vector.
#'
#' @examples
#' npar(dax_model_3t, dax_model_2n)
#'
#' @export

npar <- function(object, ...) {
  UseMethod("npar")
}

#' @export
#' @rdname npar

npar.fHMM_model <- function(object, ...) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  npar <- sapply(models, function(x) length(x$estimate))
  return(npar)
}

#' Prediction
#'
#' @description
#' This function predicts the next \code{ahead} states and data points based on
#' an \code{\link{fHMM_model}} object.
#'
#' @param object
#' An object of class \code{\link{fHMM_model}}.
#' @param ahead
#' A positive \code{integer}, the forecast horizon.
#' @inheritParams compute_ci
#' @param ...
#' Ignored.
#'
#' @return
#' A \code{data.frame} of state probabilities and data point estimates along 
#' with confidence intervals.
#'
#' @examples
#' predict(dax_model_3t)
#' 
#' @export
#' @importFrom stats qt qgamma

predict.fHMM_model <- function(object, ahead = 5, alpha = 0.05, ...) {
  
  ### check input
  if (!inherits(object,"fHMM_model")) {
    stop("'object' must be of class 'fHMM_model'.", call. = FALSE)
  }
  if (!(length(ahead) == 1 && is_number(ahead, int = TRUE, pos = TRUE))) {
    stop("'ahead' must be a positive integer.", call. = FALSE)
  }
  if (!(length(alpha) == 1 && is_number(alpha, pos = TRUE) && alpha <= 1)) {
    stop("'alpha' must be a numeric between 0 and 1.", call. = FALSE)
  }
  
  ### extract parameters
  par <- parUncon2par(object$estimate, object$data$controls)
  M <- object$data$controls$states[1]
  N <- object$data$controls$states[2]
  sdds <- object$data$controls$sdds
  
  ### predict states
  state_prediction <- matrix(NA_real_, nrow = ahead, ncol = M)
  last_state <- tail(if (object$data$controls$hierarchy) object$decoding[, 1] else object$decoding, n = 1)
  state_prob <- replace(numeric(M), last_state, 1)
  for (i in 1:ahead) {
    state_prob <- state_prob %*% par$Gamma
    state_prediction[i, ] <- state_prob
  }
  rownames(state_prediction) <- 1:ahead
  colnames(state_prediction) <- paste("state", 1:M, sep = "_")
  if (object$data$controls$hierarchy) {
    for (s in 1:M) {
      state_prob <- rep(1 / N, N)
      fs_state_prediction <- matrix(NA_real_, nrow = ahead, ncol = N)
      for (i in 1:ahead) {
        state_prob <- state_prob %*% par$Gammas_star[[s]]
        fs_state_prediction[i, ] <- state_prediction[i, s] * state_prob
      }
      rownames(fs_state_prediction) <- 1:ahead
      colnames(fs_state_prediction) <- paste("state", s, 1:N, sep = "_")
      state_prediction <- cbind(state_prediction, fs_state_prediction)
    }
  }
  
  ### predict data
  data_prediction <- matrix(NA_real_, nrow = ahead, ncol = 3)
  props <- sort(c(alpha, 0.5, 1 - alpha))
  if (!object$data$controls$hierarchy) {
    if (sdds[[1]]$name == "t") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, ] %*%
            (stats::qt(p = x, df = par$dfs) * par$sigmas + par$mus)
        })
      }
    }
    if (sdds[[1]]$name == "lnorm") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, ] %*%
            stats::qlnorm(p = x, meanlog = par$mus, sdlog = par$sigmas)
        })
      }
    }
    if (sdds[[1]]$name == "gamma") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, ] %*%
            stats::qgamma(
              p = x, shape = par$mus^2 / par$sigmas^2,
              scale = par$sigmas^2 / par$mus
            )
        })
      }
    }
  } else {
    if (sdds[[2]]$name == "t") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, -(1:M)] %*%
            (stats::qt(p = x, df = unlist(par$dfs_star)) * 
               unlist(par$sigmas_star) + unlist(par$mus_star))
        })
      }
    }
    if (sdds[[2]]$name == "lnorm") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, -(1:M)] %*%
            stats::qlnorm(p = x, meanlog = par$mus_star, 
                          sdlog = par$sigmas_star)
        })
      }
    }
    if (sdds[[2]]$name == "gamma") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, -(1:M)] %*%
            stats::qgamma(
              p = x, shape = unlist(par$mus_star)^2 / unlist(par$sigmas_star)^2,
              scale = unlist(par$sigmas_star)^2 / unlist(par$mus_star)
            )
        })
      }
    }
  }
  rownames(data_prediction) <- 1:ahead
  colnames(data_prediction) <- c("lb", "estimate", "ub")
  
  ### return 'fHMM_prediction' object
  prediction <- list("states" = state_prediction, "data" = data_prediction)
  class(prediction) <- "fHMM_predict"
  return(prediction)
}

#' @noRd
#' @export

print.fHMM_predict <- function(x, digits = 5, ...) {
  print(round(cbind(x$states, x$data), digits = digits))
  return(invisible(x))
}

