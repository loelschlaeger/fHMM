### initialize the code
init = function(){
  cat("\f")
  writeLines("Fitting (hierarchical) hidden Markov models to financial data.")
  if(!dir.exists("models")){
    dir.create("models")
  }
  writeLines(paste0("Estimation results will be saved in '",getwd(),"/models'."))
}

### initialize the estimation routine randomly
source("trans.R")

init_est = function(controls){
  M  = controls[["states"]][1] #coarse-scale states
  N  = controls[["states"]][2] #fine-scale states
  df_cs = controls[["fix_df"]][1]
  df_fs = controls[["fix_df"]][2]
  
  gammasUncon = gammasCon2gammasUncon(runif((M-1)*M,0,1/M),M)
  mus         = sort(rnorm(M)*10^(-1),decreasing=TRUE)
  sigmasUncon = log(sort(runif(M,0.1,1)*10^(-2),decreasing=FALSE))
  dfs         = if(is.na(df_cs)) runif(M,0,30) else integer(0)
  if(controls[["model"]]=="HHMM") for(m in seq_len(M)){
    gammasUncon = c(gammasUncon,gammasCon2gammasUncon(runif((N-1)*N,0,1/N),N))
    mus         = c(mus,rnorm(N)*10^(-1))
    sigmasUncon = c(sigmasUncon,log(runif(N,0.1,1)*10^(-2)))
    dfs         = c(dfs,if(is.na(df_fs)) runif(N,0,30) else integer(0))
  }
  
  thetaUncon = c(gammasUncon,mus,sigmasUncon,dfs)
  return(thetaUncon)
}

#TODO: check if something gets overwritten
### load parameters and results of the old model 'name'
loadModel = function(name){
  loadable = c("controls","data","est","states")
  path = paste0("models/",name)
  if(!dir.exists(path)){
    stop(paste0("Path '",path,"' does not exist."),call.=FALSE)
  }
  loaded = list()
  for(object in loadable){
    if(file.exists(paste0(path,"/",object))){
      loaded[[object]] = readRDS(paste0(path,"/",object))
    }
  }
  if(length(loaded)>=1) writeLines(paste0("Loaded ",paste0("'",names(loaded),"'",collapse=", ")," from model '",name,"'."))
  if(length(loaded)==0) writeLines(paste0("Unable to load any object from path '",path,"'."))
  invisible(list2env(loaded, envir = .GlobalEnv))
}
