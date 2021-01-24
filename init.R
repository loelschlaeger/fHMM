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
    message(sprintf("Loading HHMM_Finance code: %.0f%%",(e/length(exe)*100)),"\r",appendLF=FALSE)
  }
  message(paste0("Data source: ",getwd(),"/data"))
  message(paste0("Model results: ",getwd(),"/models"))
}

### define main function
hhmmf = function(id="test",controls,events=NULL,warn=0,simpar=NULL){
  ### save 'id' in 'controls'
  controls[["id"]] = id
  
  ### set handling of warnings
  options(warn=warn); on.exit(options(warn=0))
  
  ### create output folder
  if(dir.exists(paste0("models/",id)) & id!="test"){
    stop(paste0("Model '",id,"' already exists."),call.=FALSE)
  } else {
    if(!dir.exists(paste0("models/",id))){
      dir.create(paste0("models/",id))
    }
  }
  
  ### execute model fitting 
  seperator = paste0(rep("-",45),collapse="")
  tryCatch(
    {sink(file = paste0("models/",id,"/protocol.txt"),split = TRUE)
       controls = check_controls(controls); writeLines(seperator)
       data     = get_data(controls,simpar); writeLines(seperator)
       fit      = max_likelihood(data,controls)
       decoding = apply_viterbi(data,fit,controls)
     sink()
     create_visuals(data,fit,decoding,controls,events)
    },
    error = function(cond) message(cond),
    finally = {
      ### close all sink connections
      for(i in seq_len(sink.number())){
        sink()
      }
    }
  )
}