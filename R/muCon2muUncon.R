#' Unconstrain expected values
#' @param muCon Constrained expected values
#' @param link Boolean determining whether to apply the link function
#' @return Unconstrained expected values
muCon2muUncon = function(muCon,link){
  if(link) muUncon = log(muCon)
  if(!link) muUncon = muCon
  return(muUncon)
}