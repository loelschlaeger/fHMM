#' Brings uncontrained model parameters \code{thetaUncon} in list form \code{thetaList}.
#' @param thetaUncon Unconstrained model parameters in vector form.
#' @param controls A list of controls.
#' @return Constrained model parameters in list form.

thetaUncon2thetaList = function(thetaUncon,controls){
  return(thetaCon2thetaList(thetaUncon2thetaCon(thetaUncon,controls),controls))
}