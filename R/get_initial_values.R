#' Initialization of numerical likelihood optimization
#' 
#' @description
#' This helper function generates a set of initial values for the numerical
#' optimization of the model likelihood function.
#' 
#' @param initial_estimate
#' Optionally defines an initial estimate for the numerical likelihood 
#' optimization. Can be:
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
  
  ### initialize at pre-defined initial estimate
  if (!is.null(initial_estimate)) {
    if (verbose) {
      message("Initializing using given value 'initial_estimate'")
    }
    if (!check_initial_estimate(initial_estimate)) {
      initial_estimate <- NULL
    }
  }
  
  ### initialize at true value
  if (is.null(initial_estimate) && controls[["fit"]][["origin"]]) {
    if (verbose) {
      message("Initializing at true values")
    }
    initial_estimate <- list(par2parUncon(data[["true_parameters"]], controls))
    if (check_initial_estimate(initial_estimate)) {
      return(list(initial_estimate))
    } else {
      if (verbose) {
        message(
          "Initializing at true values failed, choosing other values instead"
        )
      }
    }
  }
  
  ### define heuristic
  initial_heuristic <- function(data, states) {
    
    cluster <- stats::kmeans(
      data, centers = states, iter.max = 100, nstart = 100
    )$cluster
    
    mu <- numeric(states)
    sigma <- numeric(states)
    Gamma <- oeli::sample_transition_probability_matrix(states)
    
    for (s in seq_len(states)) {
      cluster_s <- data[cluster == s]
      mu[s] <- mean(cluster_s, na.rm = TRUE)
      sigma[s] <- sd(cluster_s, na.rm = TRUE)
    }
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
  
  ### define likelihood function
  target <- ifelse(!data[["controls"]][["hierarchy"]], nLL_hmm, nLL_hhmm)
  
  ### generate and check start values
  if (verbose) {
    message("Checking start values")
  }
  N <- runs * 2
  
  while (TRUE) {
    
    if (N < ncluster) {
      ncluster <- 1
    }
    
    initial_values <- replicate(N, jitter(initial_estimate), simplify = FALSE)
  

    ll_at_initial_values <- rep(NA_real_, N)
    if (ncluster == 1) {
      
      for (n in seq_len(N)) {
        
        ll <- target(
          parUncon = initial_values[[run]],
          observations = data[["data"]],
          controls = controls
        )
        
        if (!(is.na(ll) || is.nan(ll) || abs(ll) > 1e100)) {
          ll_at_initial_values[run] <- ll
        }

      }
    } else if (ncluster > 1) {
      
      cluster <- parallel::makeCluster(ncluster)
      doSNOW::registerDoSNOW(cluster)

      ll_at_initial_values <- foreach::foreach(
        run = 1:N, .packages = "fHMM"
      ) %dopar% {
        
        ll <- target(
          parUncon = initial_values[[run]],
          observations = data[["data"]],
          controls = controls
        )

        if (!(is.na(ll) || is.nan(ll) || abs(ll) > 1e100)) {
          ll
        } else {
          NA_real_
        }
      }
      
      parallel::stopCluster(cluster)
      ll_at_initial_values <- unlist(ll_at_initial_values)
    }
    
    
  }
  
  return(initial_values)
  
}