#' Get constrained non-diagonal matrix elements of transition probability matrix
#'
#' @param Gamma Transition probability matrix
#' 
#' @return Constrained non-diagonal matrix elements (column-wise)

Gamma2gammasCon = function(Gamma){
  diag(Gamma) = NA
  return(Gamma[!is.na(Gamma)])
}