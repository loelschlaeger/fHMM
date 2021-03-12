#' Constrains expected values.
#' @param muUncon Vector of unconstrained expected values.
#' @param link Boolean, determining whether to apply the link function.
#' @return Vector of constrained expected values.

muUncon2muCon = function(muUncon,link){
  if(link) muCon = exp(muUncon)
  if(!link) muCon = muUncon
  return(muCon)
}