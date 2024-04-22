#' Initialization of numerical likelihood optimization
#' 
#' @description
#' This helper function generates a set of initial values for the numerical
#' optimization of the model likelihood function.
#' 
#' @param initial_estimate
#' Optionally defines an initial estimate for the numerical likelihood 
#' optimization. Good initial estimates can improve the optimization process.
#' Can be:
#' - \code{NULL} (the default), in this case
#'   - applies a heuristic to calculate a good initial estimate
#'   - or uses the true parameter values (if available and 
#'     \code{data$controls$origin} is \code{TRUE})
#' - or an object of class \code{parUncon}, for example the estimate of a 
#'   previously fitted model, i.e. the element \code{model$estimate}. 
#'   
#' @param seed
#' Set a seed for the generation of initial values.
#' No seed by default.
#' 
#' @inheritParams fit_model
#' 
#' @return
#' A \code{list}, where each element is an object of class \code{parUncon}.
#'
#' @keywords internal

get_initial_values <- function(
    data, ncluster = 1, seed = NULL, verbose = TRUE, initial_estimate = NULL
  ) {
  
  ### input checks
  checkmate::assert_class(data, "fHMM_data")
  checkmate::assert_number(ncluster)
  checkmate::assert_flag(verbose)
  controls <- data[["controls"]]
  checkmate::assert_class(controls, "fHMM_controls")
  runs <- controls[["fit"]][["runs"]]
  
  ### set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  ### define likelihood function
  target <- ifelse(!data[["controls"]][["hierarchy"]], nLL_hmm, nLL_hhmm)
  
  ### define function to compute log-likelihood value at initial estimate
  compute_ll_at_initial_estimate <- function(initial_estimate) {
    target(
      parUncon = initial_estimate,
      observations = data[["data"]],
      controls = controls
    )
  }
  
  ### define check function for initial estimate
  check_initial_estimate <- function(initial_estimate, verbose, return_value) {
    
    ### check correct format
    expected_length <- length(par2parUncon(fHMM_parameters(controls), controls))
    test_initial_estimate <- oeli::test_numeric_vector(
      initial_estimate, finite = TRUE, any.missing = FALSE, len = expected_length
    )
    if (!test_initial_estimate) {
      ll <- NA_real_
      if (verbose) {
        error_msg <- oeli::check_numeric_vector(
          initial_estimate, finite = TRUE, any.missing = FALSE, len = expected_length
        )
        message("'initial_estimate' is bad: ", error_msg)
      }
    } else {
      ### check implied log-likelihood value
      ll <- try(compute_ll_at_initial_estimate(initial_estimate), silent = TRUE) 
    }
    
    ### return value
    if (!(inherits(ll, "try-error") || is.na(ll) || is.nan(ll) || abs(ll) > 1e100)) {
      if (return_value) {
        return(ll)
      } else {
        return(TRUE)
      }
    } else {
      if (verbose) {
        message("Evaluating the log-likelihood at 'initial_estimate' failed")
      }
      if (return_value) {
        NA_real_
      } else {
        return(FALSE)
      }
    }
  }
  
  ### initialize at pre-defined initial estimate
  if (!is.null(initial_estimate)) {
    if (verbose) {
      message("Initializing using given value 'initial_estimate'")
    }
    if (!check_initial_estimate(initial_estimate, verbose = verbose, return_value = FALSE)) {
      if (verbose) {
        message(
          "Initializing at 'initial_estimate' failed, choosing other values instead"
        )
      }
      initial_estimate <- NULL
    }
  }
  
  ### initialize at true value
  if (is.null(initial_estimate) && controls[["fit"]][["origin"]]) {
    if (verbose) {
      message("Initializing at true values")
    }
    initial_estimate <- list(par2parUncon(data[["true_parameters"]], controls))
    if (check_initial_estimate(initial_estimate, verbose = verbose, return_value = FALSE)) {
      
      ### only one initial value in this case
      return(list(initial_estimate))
      
    } else {
      initial_estimate <- NULL
      if (verbose) {
        message(
          "Initializing at true values failed, choosing other values instead"
        )
      }
    }
  }
  
  ### define initialization heuristic
  initial_heuristic <- function(data, states) {
    
    ### cluster data
    cluster <- stats::kmeans(
      data, centers = states, iter.max = 100, nstart = 100
    )$cluster
    
    ### sample tpm at random with state persistence
    Gamma <- oeli::sample_transition_probability_matrix(states, state_persistent = TRUE)
    
    ### compute method of moments estimator for each cluster
    mu <- numeric(states)
    sigma <- numeric(states)
    for (s in seq_len(states)) {
      cluster_s <- data[cluster == s]
      mu[s] <- mean(cluster_s, na.rm = TRUE)
      sigma[s] <- sd(cluster_s, na.rm = TRUE)
    }
    
    ### return
    list("mu" = mu, "sigma" = sigma, "Gamma" = Gamma)
  }
  
  ### applying heuristic
  if (is.null(initial_estimate)) {
    if (controls[["hierarchy"]]) {
      
      # TODO
      
    } else {
      initial_estimate_list <- initial_heuristic(
        data[["data"]], states = controls[["states"]]
      )
      initial_estimate <- par2parUncon(
        do.call(
          what = fHMM_parameters,
          args = c(initial_estimate_list, list(controls))
        ),
        controls
      )
    }
  }
  
  ### jitter 'initial_estimate'
  jitter_initial_estimate <- function(initial_estimate, N) {
    
    
    
    replicate(
      N, jitter(initial_estimate, factor = 100), simplify = FALSE
    )
    
    
  }
  
  
  ### generate and check start values
  if (verbose) {
    message("Checking start values")
  }
  N <- runs * 2
  initial_values <- jitter_initial_estimate(initial_estimate, N)
  initial_values[[1]] <- initial_estimate
  ll_at_initial_values <- rep(NA_real_, N)
  
  while (TRUE) {
    
    ind <- which(is.na(ll_at_initial_values))
    N <- length(initial_values)
    
    ### stopping criterium
    if (length(ind) == 0 && N == runs) {
      break
    }
    
    ### catch cases, where parallelization does not make sense
    if (length(ind) < ncluster) {
      ncluster <- max(1, min(ncluster, length(ind)))
    }
    
    ### compute log-likelihood values
    if (ncluster <= 1) {
      
      for (i in ind) {
        ll_at_initial_values[i] <- check_initial_estimate(
          initial_values[[i]], verbose = FALSE, return_value = TRUE
        )
      }
      
    } else {
      
      cluster <- parallel::makeCluster(ncluster)
      doSNOW::registerDoSNOW(cluster)

      ll_at_initial_values <- foreach::foreach(
        run = seq_along(ind), .packages = "fHMM"
      ) %dopar% {
        check_initial_estimate(
          initial_values[[ind[run]]], verbose = FALSE, return_value = TRUE
        )
      }
      
      parallel::stopCluster(cluster)
      ll_at_initial_values <- unlist(ll_at_initial_values)
    }
    
    ### replace initial values that lead to NA
    ind <- which(is.na(ll_at_initial_values))
    initial_values[ind] <- jitter_initial_estimate(initial_estimate, length(ind))
    
    ### drop largest negative log-likelihood value
    largest <- which.max(ll_at_initial_values)
    if (length(largest) == 1) {
      ll_at_initial_values <- ll_at_initial_values[-largest]
      initial_values <- initial_values[-largest]
    }
  }
  
  ### return list of initial values
  return(initial_values)
}