### initializes the estimation routine randomly
source("trans.R")

init_est = function(controls){
  set.seed(controls[["seed"]])
  M  = controls[["states"]][1] #coarse-scale states
  N  = controls[["states"]][2] #fine-scale states
  df_cs = controls[["fix_df"]][1]
  df_fs = controls[["fix_df"]][2]
  
  gammasUncon = gammasCon2gammasUncon(runif((M-1)*M,0,1/M),M)
  mus         = sort(rnorm(M)*10^(-3),decreasing=TRUE)
  sigmasUncon = log(sort(runif(M,0.1,1)*10^(-2),decreasing=FALSE))
  dfs         = if(is.na(df_cs)) sample(1:30,M,replace=TRUE) else integer(0)
  for(m in seq_len(M)){
    gammasUncon = c(gammasUncon,gammasCon2gammasUncon(runif((N-1)*N,0,1/N),N))
    mus         = c(mus,rnorm(N)*10^(-3))
    sigmasUncon = c(sigmasUncon,log(runif(N,0.1,1)*10^(-2)))
    dfs         = c(dfs,if(is.na(df_fs)) sample(1:30,N,replace=TRUE) else integer(0))
  }
  
  thetaUncon = c(gammasUncon,mus,sigmasUncon,dfs)
  return(thetaUncon)
}
