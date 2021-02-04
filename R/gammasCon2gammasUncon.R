#' Unconstrain non-diagonal elements of transition probability matrix
#'
#' @param gammasCon Constrained non-diagonal elements of transition probability matrix
#' @param dim Dimension of transition probability matrix
#' 
#' @return Unconstrained non-diagonal elements of transition probability matrix

gammasCon2gammasUncon = function(gammasCon,dim){
  gammasUncon = Gamma2gammasUncon(gammasCon2Gamma(gammasCon,dim))
  return(gammasUncon)
}