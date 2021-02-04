#' Constrain non-diagonal elements of transition probability matrix
#'
#' @param gammasUncon Unconstrained non-diagonal elements of transition probability matrix
#' @param dim Dimension of transition probability matrix
#' 
#' @return Constrained non-diagonal elements of transition probability matrix

gammasUncon2gammasCon = function(gammasUncon,dim){
  gammasCon = Gamma2gammasCon(gammasUncon2Gamma(gammasUncon,dim))
  return(gammasCon)
}