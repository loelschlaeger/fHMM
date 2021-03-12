#' Unconstrains standard deviations.
#' @param sigmaCon Vector of constrained standard deviations.
#' @return Vector of unconstrained standard deviations.

sigmaCon2sigmaUncon = function(sigmaCon){
  return(log(sigmaCon))
}