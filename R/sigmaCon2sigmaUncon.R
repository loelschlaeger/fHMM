#' Constrain standard deviations
#'
#' @param sigmaCon Constrained standard deviations
#' 
#' @return Unconstrained standard deviations

sigmaCon2sigmaUncon = function(sigmaCon){
  return(log(sigmaCon))
}