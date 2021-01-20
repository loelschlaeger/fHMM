### initialize the code
load_code = function(){
  if(!dir.exists("models")) dir.create("models")
  if(!dir.exists("data")) dir.create("data")
  installed_packages = installed.packages()[,"Package"]
  exe = alist(if(!"Rcpp" %in% installed_packages){ writeLines("\nInstalling package 'Rcpp'.\n"); install.packages("Rcpp",quite=TRUE)},
              require("Rcpp"),
              if(!"RcppArmadillo" %in% installed_packages){ writeLines("\nInstalling package 'RcppArmadillo'.\n"); install.packages("RcppArmadillo",quitely=TRUE)},
              require("RcppArmadillo"),
              if(!"progress" %in% installed_packages){ writeLines("\nInstalling package 'progress'.\n"); install.packages("progress",quite=TRUE)},
              require("progress"),
              if(!"MASS" %in% installed_packages){ writeLines("\nInstalling package 'MASS'.\n"); install.packages("MASS",quite=TRUE)},
              require("MASS"),
              if(!"tseries" %in% installed_packages){ writeLines("\nInstalling package 'tseries'.\n"); install.packages("tseries",quite=TRUE)},
              require("tseries"),
              sourceCpp("loglike.cpp"),
              source("checks.R"),
              source("data.R"),
              source("optim.R"),
              source("trans.R"),
              source("visual.R"),
              source("viterbi.R"))
  for(e in seq_len(length(exe))){
    suppressPackageStartupMessages(eval(exe[[e]]))
    cat(sprintf("Loading HHMM_Finance code: %.0f%%",(e/length(exe)*100)),"\r")
  }
  cat("\n")
  writeLines(paste0("Data source:   ",getwd(),"/data"))
  writeLines(paste0("Model results: ",getwd(),"/models"))
}

### initialize the estimation routine randomly
init_est = function(controls){
  M  = controls[["states"]][1] 
  N  = controls[["states"]][2] 
  df_cs = controls[["fixed_dfs"]][1]
  df_fs = controls[["fixed_dfs"]][2]
  
  gammasUncon = gammasCon2gammasUncon(runif((M-1)*M,0,1/M),M)
  mus         = sort(rnorm(M)*10^(-1),decreasing=TRUE) 
  sigmasUncon = log(sort(runif(M,0.1,1)*10^(-2),decreasing=FALSE))
  dfs         = if(is.na(df_cs)) runif(M,0,30) else integer(0)
  if(controls[["model"]]=="HHMM") for(m in seq_len(M)){
    gammasUncon = c(gammasUncon,gammasCon2gammasUncon(runif((N-1)*N,0,1/N),N))
    mus         = c(mus,sort(rnorm(N)*10^(-1),decreasing=TRUE))
    sigmasUncon = c(sigmasUncon,log(sort(runif(N,0.1,1)*10^(-2),decreasing=FALSE)))
    dfs         = c(dfs,if(is.na(df_fs)) runif(N,0,30) else integer(0))
  }
  
  thetaUncon = c(gammasUncon,mus,sigmasUncon,dfs)
  return(thetaUncon)
}

### load .rds-files of the old model 'id'
load_model = function(id){
  path = paste0("models/",id)
  if(!dir.exists(path)) stop(paste0("Path '",path,"' does not exist."),call.=FALSE)
  loadable = list.files(path=path,pattern="*.rds")
  loaded = list()
  for(object in loadable) loaded[[object]] = readRDS(paste0(path,"/",object))
  if(length(loaded)>=1){
    writeLines("Reinitialization successful.")
    writeLines(paste0("ID:      ",id))
    writeLines(paste0("Objects: ",paste0(names(loaded),collapse=", ")))
  }
  if(length(loaded)==0) stop(paste0("Unable to load any object from path '",path,"'."),call.=FALSE)
  invisible(list2env(loaded, envir = .GlobalEnv))
}
