#' Constrains non-diagonal matrix elements of transition probability matrix.
#' @details Function may shift 0 and 1 non-diagonal elements by \code{1e-3}.
#' @param Gamma Transition probability matrix.
#' @param shift Boolean, determining wheter to shift boundary probabilities.
#' @return Vector of constrained non-diagonal matrix elements (column-wise).

Gamma2gammasCon = function(Gamma,shift=FALSE){
  gammasCon = Gamma[row(Gamma)!=col(Gamma)] 
  if(shift){
    gammasCon = replace(gammasCon,gammasCon==0,1e-3)
    gammasCon = replace(gammasCon,gammasCon==1,1-1e-3)
  }
  return(gammasCon)
}
