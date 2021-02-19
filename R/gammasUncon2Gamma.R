#' Build transition probability matrix from unconstrained non-diagonal elements
#' @param gammasUncon Unconstrained non-diagonal elements of transition probability matrix
#' @param dim Dimension of transition probability matrix
#' @return Transition probability matrix
gammasUncon2Gamma = function(gammasUncon,dim){
  Gamma         = diag(dim)
  Gamma[!Gamma] = exp(gammasUncon)
  Gamma         = Gamma/rowSums(Gamma)
  return(Gamma)
}