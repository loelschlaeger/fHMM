#' Constrains standard deviations.
#' @param sigmaUncon Vector of unconstrained standard deviations.
#' @return Vector of constrained standard deviations.

sigmaUncon2sigmaCon = function(sigmaUncon){
  return(exp(sigmaUncon))
}