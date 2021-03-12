#' Unconstrains expected values.
#' @param muCon Vector of constrained expected values.
#' @param link Boolean, determining whether to apply the link function.
#' @return Vector of unconstrained expected values.

muCon2muUncon = function(muCon,link){
  if(link) muUncon = log(muCon)
  if(!link) muUncon = muCon
  return(muUncon)
}