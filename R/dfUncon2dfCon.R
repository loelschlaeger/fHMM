#' Constrain degrees of freedom
#' @param dfUncon Unconstrained degrees of freedom
#' @return Constrained degrees of freedom
dfUncon2dfCon = function(dfUncon){
  return(exp(dfUncon))
}