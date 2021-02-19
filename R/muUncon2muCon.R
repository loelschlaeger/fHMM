#' Constrain expected values
#' @param muUncon Unconstrained expected values
#' @param link Boolean determining whether to apply the link function
#' @return Constrained expected values
muUncon2muCon = function(muUncon,link){
  if(link) muCon = exp(muUncon)
  if(!link) muCon = muUncon
  return(muCon)
}