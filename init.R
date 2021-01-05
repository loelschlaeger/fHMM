### initialize the code
init = function(){
  message("Loading code...\r")
  if(!dir.exists("models")) dir.create("models")
  if(!dir.exists("data")) dir.create("data")
  installed_packages = installed.packages()[,"Package"]
  exe = alist(if(!"Rcpp" %in% installed_packages){ writeLines("\nInstalling package 'Rcpp'.\n"); install.packages("Rcpp",quite=TRUE)},
              require("Rcpp"),
              if(!"RcppArmadillo" %in% installed_packages){ writeLines("\nInstalling package 'RcppArmadillo'.\n"); install.packages("RcppArmadillo",quitely=TRUE)},
              require("RcppArmadillo"),
              if(!"progress" %in% installed_packages){ writeLines("\nInstalling package 'progress'.\n"); install.packages("progress",quite=TRUE)},
              require("progress"),
              source("checks.R"),
              source("data.R"),
              sourceCpp("loglike.cpp"),
              source("optim.R"),
              source("trans.R"),
              source("visual.R"),
              source("viterbi.R"))
  for(e in seq_len(length(exe))) suppressPackageStartupMessages(eval(exe[[e]]))
  cat("\f")
  writeLines("Fit (H)HMMs to financial data.")
  writeLines(paste0("Data source:   ",getwd(),"/data"))
  writeLines(paste0("Model results: ",getwd(),"/models"))
}

### initialize the estimation routine randomly
init_est = function(controls){
  M  = controls[["states"]][1] #coarse-scale states
  N  = controls[["states"]][2] #fine-scale states
  df_cs = controls[["fix_dfs"]][1]
  df_fs = controls[["fix_dfs"]][2]
  
  gammasUncon = gammasCon2gammasUncon(runif((M-1)*M,0,1/M),M)
  mus         = sort(rnorm(M)*10^(-2),decreasing=TRUE) 
  sigmasUncon = log(sort(runif(M,0.1,1)*10^(-2)/2,decreasing=FALSE))
  dfs         = if(is.na(df_cs)) runif(M,0,30) else integer(0)
  if(controls[["model"]]=="HHMM") for(m in seq_len(M)){
    gammasUncon = c(gammasUncon,gammasCon2gammasUncon(runif((N-1)*N,0,1/N),N))
    mus         = c(mus,sort(rnorm(N)*10^(-2),decreasing=TRUE))
    sigmasUncon = c(sigmasUncon,log(sort(runif(N,0.1,1)*10^(-2)/2,decreasing=FALSE)))
    dfs         = c(dfs,if(is.na(df_fs)) runif(N,0,30) else integer(0))
  }
  
  thetaUncon = c(gammasUncon,mus,sigmasUncon,dfs)
  return(thetaUncon)
}

### load parameters and results of the old model 'id'
reinit = function(id){
  loadable = c("controls","data","fit","decoding","labels")
  path = paste0("models/",id)
  if(!dir.exists(path)) stop(paste0("Reinitialization failed, path '",path,"' does not exist."),call.=FALSE)
  loaded = list()
  for(object in loadable){
    if(file.exists(paste0(path,"/",object))) loaded[[object]] = readRDS(paste0(path,"/",object))
  }
  if(length(loaded)>=1){
    writeLines("Reinitialization successful.")
    writeLines(paste0("ID:      ",id))
    writeLines(paste0("Objects: ",paste0(names(loaded),collapse=", ")))
  }
  if(length(loaded)==0) stop(paste0("Reinitialization failed, unable to load any object from path '",path,"'."),call.=FALSE)
  invisible(list2env(loaded, envir = .GlobalEnv))
}
