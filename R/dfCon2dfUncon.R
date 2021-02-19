#' Unconstrain degrees of freedom
#' @param dfCon Constrained degrees of freedom
#' @return Unconstrained degrees of freedom
dfCon2dfUncon = function(dfCon){
  return(log(dfCon))
}