# Initializes randomly the estimation routine

init_est = function(controls){
  M          = controls[["M"]]
  N          = controls[["N"]]
  est_df     = controls[["est_df"]]
  gammasUncon = runif((M-1)*M+M*(N-1)*N,-2,-1) #non-diagonal entries of tpm
  musUncon    = rnorm(M+M*N)*10^(-3)
  sigmasUncon = log(runif(M+M*N,0.1,1)*10^(-2))
  if(est_df=="yes") {dfs = sample(1:30,M+M*N,replace=TRUE)}
  if(est_df=="no")  {dfs = c()}
  thetaUncon = c(gammasUncon,musUncon,sigmasUncon,dfs)
  return(thetaUncon)
}