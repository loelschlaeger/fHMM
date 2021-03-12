#' Unconstrains non-diagonal elements of transition probability matrix.
#' @param gammasCon Vector of constrained non-diagonal elements of transition probability matrix.
#' @param dim Numeric, dimension of transition probability matrix.
#' @return Vector of unconstrained non-diagonal elements of transition probability matrix.

gammasCon2gammasUncon = function(gammasCon,dim){
  gammasUncon = Gamma2gammasUncon(gammasCon2Gamma(gammasCon,dim))
  return(gammasUncon)
}