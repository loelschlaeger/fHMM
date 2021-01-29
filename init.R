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
              Rcpp::sourceCpp("loglike.cpp"),
              source("checks.R"),
              source("data.R"),
              source("exception.R"),
              source("hhmmf.R"),
              source("optim.R"),
              source("trans.R"),
              source("visual.R"),
              source("viterbi.R"),
              source("yahoo.R"))
  for(e in seq_len(length(exe))){
    suppressPackageStartupMessages(eval(exe[[e]]))
    message(sprintf("Loading HHMM_Finance code: %.0f%%",(e/length(exe)*100)),"\r",appendLF=FALSE)
  }
  message(paste0("Data source: ",getwd(),"/data"))
  message(paste0("Model results: ",getwd(),"/models"))
}