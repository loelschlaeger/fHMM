#' Bring constrained model parameters from list form to vector form
#' @param thetaList Constrained model parameters in list form
#' @param controls A list of controls
#' @return Constrained model parameters in vector form
thetaList2thetaCon = function(thetaList,controls){
  return(thetaUncon2thetaCon(thetaList2thetaUncon(thetaList,controls),controls))
}