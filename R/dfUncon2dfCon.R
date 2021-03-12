#' Constrains degrees of freedom.
#' @param dfUncon Numeric, unconstrained degrees of freedom.
#' @return Numeric, constrained degrees of freedom.

dfUncon2dfCon = function(dfUncon){
  return(exp(dfUncon))
}