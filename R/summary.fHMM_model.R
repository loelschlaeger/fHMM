#' Summary method for \code{fHMM_model}.
#' @param object
#' An object of class \code{fHMM_model}.
#' @param ci_level
#' Level of confidence.
#' @param ...
#' Ignored.
#' @return 
#' An object of class \code{summary.fHMM_model}.
#' @export

summary.fHMM_model = function(object, ci_level = 0.05, ...) {
  
  simulated = object$data$controls$simulated
  hierarchy = object$data$controls$hierarchy
  no_par = length(object$estimate)
  data_size = length(as.vector(object$data$data))
  ll = object$ll
  aic = -2*ll + 2*no_par
  bic = -2*ll + no_par*log(data_size)
  
  ### build data frame of parameter estimates
  ci = compute_ci(object, ci_level)
  ci$lb$sdds = NULL
  ci$estimate$sdds = NULL
  ci$ub$sdds = NULL
  estimates = data.frame(lapply(ci,unlist))
  if(simulated){
    true = object$data$true_parameters
    true$sdds = NULL
    estimates = cbind(estimates, true = unlist(true))
  }
  
  ### decoded states
  if(!is.null(object$decoding)){
    decod_freq = table(object$decoding)   
    if(simulated){
      true_decod_freq = table(object$data$markov_chain)
    }
  }
 
  ### build and return summary
  out = list("simulated" = simulated,
             "hierarchy" = hierarchy,
             "no_par" = no_par,
             "data_size" = data_size,
             "ll" = ll,
             "aic" = aic,
             "bic" = bic,
             "estimates" = estimates,
             "decod_freq" = decod_freq)
  class(out) = "summary.fHMM_model"
  return(out)
}