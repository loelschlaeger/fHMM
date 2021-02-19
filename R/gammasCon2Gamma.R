#' Build transition probability matrix from constrained non-diagonal elements
#' @param gammasCon Constrained non-diagonal elements of transition probability matrix
#' @param dim Dimension of transition probability matrix
#' @return Transition probability matrix
gammasCon2Gamma = function(gammasCon,dim){
  Gamma         = diag(dim)
  Gamma[!Gamma] = gammasCon 
  for(i in 1:dim){
    Gamma[i,i] = 1-(rowSums(Gamma)[i]-1)
  }
  return(Gamma)
}