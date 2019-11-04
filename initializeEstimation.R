initializeEstimation <- function(controls,atTrueValues=FALSE){
  
  M          = controls[["M"]]
  N          = controls[["N"]]
  est_df     = controls[["est_df"]]
  set_df_cs  = controls[["set_df_cs"]] 
  set_df_fs  = controls[["set_df_fs"]] 
  
  gammas = runif((M-1)*M+M*(N-1)*N,-2,-1) #non-diagonal entries of tpm
  mus    = rnorm(M+M*N)*10^(-3)
  sigmas = log(runif(M+M*N,0.1,1)*10^(-2))
  if(est_df=="all") {dfs     = runif(M+M*N,0,0.5)}
  if(est_df=="fscs"){dfs     = runif(2,0,0.5)}
  if(est_df=="no")  {dfs     = c()}
  
  start <- c(gammas,mus,sigmas,dfs)
  
  return(start)
}