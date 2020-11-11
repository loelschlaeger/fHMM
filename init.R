### initializes the estimation routine randomly

init_est = function(controls){
  M  = controls[["states"]][1] #coarse-scale states
  N  = controls[["states"]][2] #fine-scale states
  df_cs = controls[["fix_df"]][1]
  df_fs = controls[["fix_df"]][2]
  
  gammasUncon = gammasCon2gammasUncon(runif((M-1)*M,0,1/M),M)
  mus         = rnorm(M)*10^(-3)
  sigmasUncon = log(runif(M,0.1,1)*10^(-2))
  dfs         = if(is.na(df_cs)) sample(1:30,M,replace=TRUE) else integer(0)
  for(n in seq_len(N)){
    gammasUncon = c(gammasUncon,gammasCon2gammasUncon(runif((N-1)*N,0,1/N),N),M)
    mus         = c(mus,rnorm(N)*10^(-3))
    sigmasUncon = c(sigmasUncon,log(runif(N,0.1,1)*10^(-2)))
    dfs         = c(dfs,if(is.na(df_fs)) sample(1:30,N,replace=TRUE) else integer(0))
  }
  
  thetaUncon = c(gammasUncon,mus,sigmasUncon,dfs)
  return(thetaUncon)
}
