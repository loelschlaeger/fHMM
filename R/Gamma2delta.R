#' Compute stationary distribution of transition probability matrix
#' @param Gamma Transition probability matrix
#' @return Stationary distribution
Gamma2delta = function(Gamma){
  dim   = dim(Gamma)[1]
  if(class(try(solve(t(diag(dim)-Gamma+1),rep(1,dim)),silent=TRUE))=="try-error"){ 
    delta = rep(1/dim,dim)
    warning(sprintf("%s (%s)",exception("F.1")[2],exception("F.1")[1]),call.=FALSE)
  } else { 
    delta = solve(t(diag(dim)-Gamma+1),rep(1,dim))
  }
  return(delta)
}