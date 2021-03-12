#' Constrains non-diagonal elements of transition probability matrix.
#' @param gammasUncon Vector of unconstrained non-diagonal elements of transition probability matrix.
#' @param dim Numeric, dimension of transition probability matrix.
#' @return Vector of constrained non-diagonal elements of transition probability matrix.

gammasUncon2gammasCon = function(gammasUncon,dim){
  gammasCon = Gamma2gammasCon(gammasUncon2Gamma(gammasUncon,dim))
  return(gammasCon)
}