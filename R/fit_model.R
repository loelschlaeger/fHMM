#' Model fitting
#'
#' @description
#' This function fits a hidden Markov model via numerical likelihood 
#' maximization.
#'
#' @details
#' Multiple optimization runs starting from different initial values are 
#' computed in parallel if \code{ncluster > 1}.
#'
#' @param data
#' An object of class \code{\link{fHMM_data}}.
#' 
#' @param ncluster
#' Set the number of clusters for parallel optimization runs to reduce 
#' optimization time.
#' By default, \code{ncluster = 1} (no clustering).
#' 
#' @param verbose
#' Set to \code{TRUE} to print progress messages.
#' 
#' @inheritParams get_initial_values
#' @inheritParams set_controls
#'
#' @return
#' An object of class \code{\link{fHMM_model}}.
#' 
#' @examples
#' ### 2-state HMM with normal distributions
#' 
#' # define model
#' controls <- set_controls(states = 2, sdds = "normal", horizon = 80, runs = 5)
#' 
#' # define parameters
#' parameters <- fHMM_parameters(controls, mu = c(-1, 1), seed = 1)
#' 
#' # sample data
#' data <- prepare_data(controls, true_parameter = parameters, seed = 1)
#' 
#' # fit model
#' model <- fit_model(data, seed = 1)
#' 
#' # inspect fit
#' summary(model)
#' plot(model, "sdds")
#' 
#' # decode states
#' model <- decode_states(model)
#' 
#' # predict
#' predict(model, ahead = 5)
#'
#' @export

fit_model <- function(
    data, ncluster = 1, seed = NULL, verbose = TRUE, initial_estimate = NULL
) {
  
  ### check inputs
  if (!inherits(data, "fHMM_data")) {
    stop("'data' is not of class 'fHMM_data'.", call. = FALSE)
  }
  if (!checkmate::test_count(ncluster, positive = TRUE)) {
    stop("'ncluster' must be a positive integer.", call. = FALSE)
  }
  if (!isTRUE(verbose) && !isFALSE(verbose)) {
    stop("'verbose' must be either TRUE or FALSE.", call. = FALSE)
  }
  
  ### generate initial values
  initial_values <- get_initial_values(
    data = data, ncluster = ncluster, seed = seed, verbose = verbose,
    initial_estimate = initial_estimate
  )
  
  ### define likelihood function
  target <- ifelse(!data[["controls"]][["hierarchy"]], nLL_hmm, nLL_hhmm)
  
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
      
      suppressWarnings({
        mod <- try(
          stats::nlm(
            f = target,
            p = initial_values[[run]],
            observations = data[["data"]],
            controls = data[["controls"]],
            iterlim = data[["controls"]][["fit"]][["iterlim"]],
            steptol = data[["controls"]][["fit"]][["steptol"]],
            gradtol = data[["controls"]][["fit"]][["gradtol"]],
            print.level = data[["controls"]][["fit"]][["print.level"]],
            typsize = initial_values[[run]],
            hessian = FALSE
          ),
          silent = TRUE
        )
      })
      
      accept_run <- !inherits(mod, "try-error") && 
        mod[["code"]] %in% data[["controls"]][["fit"]][["accept"]]

      if (accept_run) {
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
      
      suppressWarnings({
        mod <- try(
          stats::nlm(
            f = target,
            p = initial_values[[run]],
            observations = data[["data"]],
            controls = data[["controls"]],
            iterlim = data[["controls"]][["fit"]][["iterlim"]],
            steptol = data[["controls"]][["fit"]][["steptol"]],
            gradtol = data[["controls"]][["fit"]][["gradtol"]],
            print.level = data[["controls"]][["fit"]][["print.level"]],
            typsize = initial_values[[run]],
            hessian = FALSE
          ),
          silent = TRUE
        )
      })
      
      if (verbose) pb$tick()
      
      accept_run <- !inherits(mod, "try-error") &&
        mod[["code"]] %in% data[["controls"]][["fit"]][["accept"]]
      
      if (accept_run) {
        mod
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
    numDeriv::hessian(
      func = target,
      method = "Richardson",
      x = mods[[which.max(lls)]][["estimate"]],
      observations = data[["data"]],
      controls = data[["controls"]]
    )
  )
  
  ### final message
  if (verbose) message("Fitting completed")
  
  ### extract estimation results
  mod <- mods[[which.max(lls)]]
  ll <- -mod[["minimum"]]
  estimate <- mod[["estimate"]]
  class(estimate) <- "parUncon"
  estimation_time <- ceiling(difftime(end_time, start_time, units = "mins"))
  
  ### TODO reorder states
  
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