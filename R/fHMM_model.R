#' Constructor of a model object
#'
#' @description
#' This function constructs an object of class \code{\link{fHMM_model}}, which 
#' contains details about the fitted (hierarchical) Hidden Markov model.
#' 
#' @param x,object
#' An object of class \code{\link{fHMM_model}}.
#' @param ...
#' Currently not used.
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
#' @param alpha
#' A \code{numeric} between 0 and 1, the confidence level.
#' @param digits
#' The number of decimal places.
#' @param k
#' Passed on to \code{\link[stats]{AIC}}.
#' @param ahead
#' The number of time points to predict ahead.
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

fit_model <- function(
    data, ncluster = 1, seed = NULL, verbose = TRUE, init = NULL
  ) {

  ### check inputs
  if (!inherits(data, "fHMM_data")) {
    stop("'data' is not of class 'fHMM_data'.", 
         call. = FALSE)
  }
  if (!checkmate::test_count(ncluster, positive = TRUE)) {
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
    stop(
      "The likelihood could not be computed at any of the selected start values.\n",
      "Try to increase 'runs' in 'controls'.", 
      call. = FALSE
    )
  }
  if (sum(is.na(ll_at_start_values)) > 0.5 * data[["controls"]][["fit"]][["runs"]]) {
    warning(
      "The likelihood could not be computed at more than half of the selected start values.\n",
      "Try to increase 'runs' in 'controls'.", 
      call. = FALSE, immediate. = TRUE
    )
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
    stop(
      "None of the estimation runs ended successfully.\n",
      "Adapt 'accept' or increase 'runs' in 'controls'.", 
      call. = FALSE
    )
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

#' @rdname fHMM_model
#' @exportS3Method 

print.fHMM_model <- function(x, ...) {
  cat("fHMM fitted model:\n")
  cat("* total estimation time:", x$estimation_time, units(x$estimation_time), "\n")
  cat("* accepted runs:", sum(!is.na(x$lls)), "of", length(x$lls), "\n")
  cat("* log-likelihood:", x$ll, "\n")
  invisible(x)
}

#' @rdname fHMM_model
#' @exportS3Method

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

#' @rdname fHMM_model
#' @exportS3Method 

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
      res_cs <- stats::na.omit(object$residuals[, 1])
      res_summary_cs <- summary(res_cs)
      res_fs <- stats::na.omit(as.vector(object$residuals[, -1]))
      res_summary_fs <- summary(res_fs)
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

#' @rdname fHMM_model
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

#' @rdname fHMM_model
#' @exportS3Method 

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

#' @rdname fHMM_model
#' @exportS3Method 

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

#' @rdname fHMM_model
#' @exportS3Method 

nobs.fHMM_model <- function(object, ...) {
  return(length(as.vector(object$data$data)))
}

#' @rdname fHMM_model
#' @exportS3Method 

logLik.fHMM_model <- function(object, ...) {
  return(object$ll)
}

#' @rdname fHMM_model
#' @export

npar <- function(object, ...) {
  UseMethod("npar")
}

#' @rdname fHMM_model
#' @exportS3Method 

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

#' @rdname fHMM_model
#' @exportS3Method 

predict.fHMM_model <- function(object, ahead = 5, alpha = 0.05, ...) {
  
  ### check input
  if (!inherits(object,"fHMM_model")) {
    stop("'object' must be of class 'fHMM_model'.", call. = FALSE)
  }
  if (!checkmate::test_count(ahead, positive = TRUE)) {
    stop("'ahead' must be a positive integer.", call. = FALSE)
  }
  if (!checkmate::test_number(alpha, lower = 0, upper = 1)) {
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
        state_prob <- state_prob %*% par$Gamma_star[[s]]
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
            (stats::qt(p = x, df = par$df) * par$sigma + par$mu)
        })
      }
    } else if (sdds[[1]]$name == "normal") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, ] %*%
            stats::qnorm(p = x, mean = par$mu, sd = par$sigma)
        })
      }
    } else if (sdds[[1]]$name == "lognormal") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, ] %*%
            stats::qlnorm(p = x, meanlog = par$mu, sdlog = par$sigma)
        })
      }
    } else if (sdds[[1]]$name == "gamma") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, ] %*%
            stats::qgamma(
              p = x, shape = par$mu^2 / par$sigma^2,
              scale = par$sigma^2 / par$mu
            )
        })
      }
    } else if (sdds[[1]]$name == "poisson") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, ] %*%
            stats::qpois(p = x, lambda = par$mu)
        })
      }
    } else {
      stop("Unknown state-dependent distribution", call. = FALSE)
    }
  } else {
    if (sdds[[2]]$name == "t") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, -(1:M)] %*%
            (stats::qt(p = x, df = unlist(par$df_star)) * 
               unlist(par$sigma_star) + unlist(par$mu_star))
        })
      }
    } else if (sdds[[2]]$name == "normal") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, -(1:M)] %*%
            stats::qnorm(p = x, mean = par$mu_star, sd = par$sigma_star)
        })
      }
    } else if (sdds[[2]]$name == "lognormal") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, -(1:M)] %*%
            stats::qlnorm(p = x, meanlog = par$mu_star, 
                          sdlog = par$sigma_star)
        })
      }
    } else if (sdds[[2]]$name == "gamma") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, -(1:M)] %*%
            stats::qgamma(
              p = x, shape = unlist(par$mu_star)^2 / unlist(par$sigma_star)^2,
              scale = unlist(par$sigma_star)^2 / unlist(par$mu_star)
            )
        })
      }
    } else if (sdds[[2]]$name == "poisson") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, -(1:M)] %*%
            stats::qpois(p = x, mean = par$mu_star)
        })
      }
    } else {
      stop("Unknown state-dependent distribution", call. = FALSE)
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
#' @exportS3Method 

print.fHMM_predict <- function(x, digits = 5, ...) {
  print(round(cbind(x$states, x$data), digits = digits))
  return(invisible(x))
}

