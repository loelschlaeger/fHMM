#' Unconstrains degrees of freedom.
#' @param dfCon Numeric, constrained degrees of freedom.
#' @return Numeric, unconstrained degrees of freedom.

dfCon2dfUncon = function(dfCon){
  return(log(dfCon))
}