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
  
  ### model information
  simulated = object$data$controls$simulated
  hierarchy = object$data$controls$hierarchy
  no_par = length(object$estimate)
  data_size = length(as.vector(object$data$data))
  ll = object$ll
  aic = -2*ll + 2*no_par
  bic = -2*ll + no_par*log(data_size)
  model_info = data.frame(
    simulated, hierarchy, "LL" = ll, "AIC" = aic, "BIC" = bic,
    row.names = "value"
  )
  
  ### state-dependent distributions
  sdds = parUncon2par(object$estimate, object$data$controls)$sdds
  
  ### parameter estimates
  ci = compute_ci(object, ci_level)
  estimates_table = data.frame(lapply(ci, as.vector))
  if(simulated){
    true = par2parCon(object$data$true_parameters, object$data$controls)
    estimates_table = cbind(estimates_table, true = as.vector(true))
  }
  
  ### states
  if(!is.null(object$decoding)){
    if(simulated){
      decoding_table = table(object$data$markov_chain, object$decoding,
                             dnn = c("true", "decoded"))
    } else {
      decoding_table = table(object$decoding, dnn = "decoded")
    }
  } else {
    decoding_table = NULL
  }
  
  ### residuals
  if(!is.null(object$residuals)){
    res_summary = summary(object$residuals)
  } else {
    res_summary = NULL
  }
 
  ### build and return summary
  out = list("no_par" = no_par,
             "data_size" = data_size,
             "model_info" = model_info,
             "sdds" = sdds,
             "estimates_table" = estimates_table,
             "decoding_table" = decoding_table,
             "res_summary" = res_summary)
  class(out) = "summary.fHMM_model"
  return(out)
}