#' Get unconstrained non-diagonal matrix elements of transition probability matrix
#'
#' @param Gamma Transition probability matrix
#' 
#' @return Unconstrained non-diagonal matrix elements (column-wise)

Gamma2gammasUncon = function(Gamma){
  diag(Gamma) = 0
  Gamma       = log(Gamma/(1-rowSums(Gamma)))
  diag(Gamma) = NA
  return(Gamma[!is.na(Gamma)])
}