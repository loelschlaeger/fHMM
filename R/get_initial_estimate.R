#' Initial estimate for numerical likelihood optimization
#' 
#' @description
#' This helper function uses a heuristic to find a good initial estimate for the 
#' numerical optimization of the model likelihood function.
#' 
#' @param initial_estimate
#' Optionally defines an initial estimate for the numerical likelihood 
#' optimization. Can be:
#' - \code{NULL} (the default), in this case
#'   - applies a heuristic to calculate an initial estimate,
#'   - or uses the true parameter values if \code{data$controls$origin} is 
#'     \code{TRUE}.
#' - or an object of class \code{parUncon}, for example the estimate of a 
#'   previously fitted model, i.e. the element \code{model$estimate}. 
#' 
#' @inheritParams fit_model
#'
#' @return 
#' An object of class \code{parUncon}.
#' 
#' @keywords internal

get_initial_estimate <- function(
    data, verbose = TRUE, initial_estimate = NULL
  ) {
  
  ### define likelihood function
  target <- ifelse(!data[["controls"]][["hierarchy"]], nLL_hmm, nLL_hhmm)
  
  ### get initial estimate
  if (is.null(initial_estimate)) {
    if (data[["controls"]][["fit"]][["origin"]]) {
      
      return(par2parUncon(data[["true_parameters"]], data[["controls"]]))
      
    } else {
      
      # apply heuristic
      
      ### compute parameter scales based on the method of moments
      scale_par <- c(1, 1)
      if (!data[["controls"]][["hierarchy"]]) {
        scale_par[1] <- mean(c(mean(data[["data"]], na.rm = TRUE), stats::sd(data[["data"]], na.rm = TRUE)))
      } else {
        scale_par[1] <- mean(c(mean(data[["data"]][, 1], na.rm = TRUE), stats::sd(data[["data"]][, 1], na.rm = TRUE)))
        scale_par[2] <- mean(c(mean(data[["data"]][, -1], na.rm = TRUE), stats::sd(data[["data"]][, -1], na.rm = TRUE)))
      }
      scale_par <- abs(scale_par)
      
      

      initial_estimate <- par2parUncon(
        fHMM_parameters(controls = data[["controls"]], scale_par = scale_par),
        data[["controls"]]
      )
      
      return(initial_estimate)
      
    }
  } else {
    
    return(initial_estimate)
    # check "initial_estimate"
    
  }
  
  
}