#' Initialization of numerical likelihood optimization
#' 
#' @description
#' This helper function generates a set of initial values for the numerical
#' optimization of the model likelihood function.
#' 
#' @param seed
#' Set a seed for the generation of initial values.
#' No seed by default.
#' 
#' @inheritParams fit_model
#' @inheritParams get_initial_estimate
#' 
#' @return
#' A \code{list}, where each element is an object of class \code{parUncon}.
#'
#' @keywords internal

get_initial_values <- function(
    data, ncluster = 1, seed = NULL, verbose = TRUE, initial_estimate = NULL
  ) {
  
  ### set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  ### generate start values
  initial_estimate <- get_initial_estimate(
    data = data, verbose = verbose, initial_estimate = initial_estimate
  )
  initial_values <- replicate(data$controls$fit$runs, jitter(initial_estimate), simplify = FALSE)
  
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
  ll_at_initial_values <- rep(NA_real_, data[["controls"]][["fit"]][["runs"]])
  if (ncluster == 1) {
    for (run in 1:data[["controls"]][["fit"]][["runs"]]) {
      if (verbose) pb$tick(0)
      ll <- target(
        parUncon = initial_values[[run]],
        observations = data[["data"]],
        controls = data[["controls"]]
      )
      if (!(is.na(ll) || is.nan(ll) || abs(ll) > 1e100)) {
        ll_at_initial_values[run] <- ll
      }
      if (verbose) pb$tick()
    }
  } else if (ncluster > 1) {
    cluster <- parallel::makeCluster(ncluster)
    doSNOW::registerDoSNOW(cluster)
    opts <- if (verbose) list(progress = function(n) pb$tick()) else list()
    ll_at_initial_values <- foreach::foreach(
      run = 1:data[["controls"]][["fit"]][["runs"]],
      .packages = "fHMM", .options.snow = opts
    ) %dopar% {
      ll <- target(
        parUncon = initial_values[[run]],
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
    ll_at_initial_values <- unlist(ll_at_initial_values)
  }
  if (sum(is.na(ll_at_initial_values)) == data[["controls"]][["fit"]][["runs"]]) {
    stop(
      "The likelihood could not be computed at any of the selected start values.\n",
      "Try to increase 'runs' in 'controls'.", 
      call. = FALSE
    )
  }
  if (sum(is.na(ll_at_initial_values)) > 0.5 * data[["controls"]][["fit"]][["runs"]]) {
    warning(
      "The likelihood could not be computed at more than half of the selected start values.\n",
      "Try to increase 'runs' in 'controls'.", 
      call. = FALSE, immediate. = TRUE
    )
  }
  
  return(initial_values)
  
}