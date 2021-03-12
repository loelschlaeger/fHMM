#' Builds transition probability matrix from unconstrained non-diagonal elements.
#' @param gammasUncon Vector of unconstrained non-diagonal elements of transition probability matrix.
#' @param dim Numeric, dimension of transition probability matrix.
#' @return Transition probability matrix.

gammasUncon2Gamma = function(gammasUncon,dim){
  Gamma         = diag(dim)
  Gamma[!Gamma] = exp(gammasUncon)
  Gamma         = Gamma/rowSums(Gamma)
  return(Gamma)
}