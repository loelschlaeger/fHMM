### initialize the code
load_code = function(){

  installed_packages = installed.packages()[,"Package"]
  exe = alist(
    ### installing and loading packages
    if(!"Rcpp" %in% installed_packages){ writeLines("\nInstalling package 'Rcpp'.\n"); install.packages("Rcpp",quite=TRUE)},
    require("Rcpp"),
    if(!"RcppArmadillo" %in% installed_packages){ writeLines("\nInstalling package 'RcppArmadillo'.\n"); install.packages("RcppArmadillo",quitely=TRUE)},
    require("RcppArmadillo"),
    if(!"progress" %in% installed_packages){ writeLines("\nInstalling package 'progress'.\n"); install.packages("progress",quite=TRUE)},
    require("progress"),
    if(!"MASS" %in% installed_packages){ writeLines("\nInstalling package 'MASS'.\n"); install.packages("MASS",quite=TRUE)},
    require("MASS"),
    if(!"tseries" %in% installed_packages){ writeLines("\nInstalling package 'tseries'.\n"); install.packages("tseries",quite=TRUE)},
    require("tseries"),
    
    ### loading code
    Rcpp::sourceCpp("scr/LL_HMM_Rcpp.cpp"),
    Rcpp::sourceCpp("scr/LL_HHMM_Rcpp.cpp"),
    source('R/apply_viterbi.R'),
    source('R/check_controls.R'),
    source('R/check_decoding.R'),
    source('R/check_estimation.R'),
    source('R/check_saving.R'),
    source('R/compute_ci.R'),
    source('R/compute_fs.R'),
    source('R/create_visuals.R'),
    source('R/dfCon2dfUncon.R'),
    source('R/dfUncon2dfCon.R'),
    source('R/download_data.R'),
    source('R/exception.R'),
    source('R/fit_hmm.R'),
    source('R/Gamma2delta.R'),
    source('R/Gamma2gammasCon.R'),
    source('R/Gamma2gammasUncon.R'),
    source('R/gammasCon2Gamma.R'),
    source('R/gammasCon2gammasUncon.R'),
    source('R/gammasUncon2Gamma.R'),
    source('R/gammasUncon2gammasCon.R'),
    source('R/init_est.R'),
    source('R/max_likelihood.R'),
    source('R/muCon2muUncon.R'),
    source('R/muUncon2muCon.R'),
    source('R/nLL_hhmm.R'),
    source('R/nLL_hmm.R'),
    source('R/parameter_names.R'),
    source('R/plot_ll.R'),
    source('R/plot_sdd.R'),
    source('R/plot_ts.R'),
    source('R/process_data.R'),
    source('R/pseudo_residuals.R'),
    source('R/read_data.R'),
    source('R/sigmaCon2sigmaUncon.R'),
    source('R/sigmaUncon2sigmaCon.R'),
    source('R/simulate_data.R'),
    source('R/thetaList2thetaCon.R'),
    source('R/thetaList2thetaListOrdered.R'),
    source('R/thetaCon2thetaList.R'),
    source('R/thetaUncon2thetaCon.R'),
    source('R/thetaUncon2thetaList.R'),
    source('R/thetaUncon2thetaUnconSplit.R'),
    source('R/thetaUnconSplit2thetaList.R')
  )
  for(e in seq_len(length(exe))){
    suppressPackageStartupMessages(eval(exe[[e]]))
    message(sprintf("loading fhmm code: %.0f%%",(e/length(exe)*100)),"\r",appendLF=FALSE)
  }
  message(sprintf("fhmm code loaded %10s"," "))
}