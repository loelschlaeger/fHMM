# Initializes randomly the estimation routine

initializeEstimation = function(controls){
  
  M          = controls[["M"]]
  N          = controls[["N"]]
  est_df     = controls[["est_df"]]
  
  gammasUncon     = runif((M-1)*M+M*(N-1)*N,-2,-1) #non-diagonal entries of tpm
  musUncon        = rnorm(M+M*N)*10^(-3)
  sigmasUncon     = log(runif(M+M*N,0.1,1)*10^(-2))
  
  if(est_df=="all") {dfsUncon     = runif(M+M*N,0,0.5)}
  if(est_df=="fscs"){dfsUncon     = runif(2,0,0.5)}
  if(est_df=="no")  {dfsUncon     = c()}
  
  thetaUncon = c(gammasUncon,musUncon,sigmasUncon,dfsUncon)
  
  return(thetaUncon)
}