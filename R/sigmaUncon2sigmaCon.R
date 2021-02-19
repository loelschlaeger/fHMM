#' Constrain standard deviations
#' @param sigmaUncon Unconstrained standard deviations
#' @return Constrained standard deviations
sigmaUncon2sigmaCon = function(sigmaUncon){
  return(exp(sigmaUncon))
}