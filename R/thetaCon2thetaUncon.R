#' Unconstrain model parameters
#' @param thetaCon Constrained model parameters in vector form
#' @param controls A list of controls
#' @return Unconstrained model parameters in vector form
thetaCon2thetaUncon = function(thetaCon,controls){
  return(thetaList2thetaUncon(thetaCon2thetaList(thetaCon,controls),controls))
}