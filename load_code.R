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
  for(file in grep(list.files(path="src",pattern=".cpp$"),invert=TRUE,pattern="RcppExports.cpp",value=TRUE)) Rcpp::sourceCpp(paste0("src/",file)),
  for(file in grep(list.files(path="R",pattern=".R$"),invert=TRUE,pattern="RcppExports.R",value=TRUE)) source(paste0("R/",file))
)
for(e in seq_len(length(exe))){
  suppressPackageStartupMessages(eval(exe[[e]]))
  message(sprintf("loading fHMM code: %.0f%%",(e/length(exe)*100)),"\r",appendLF=FALSE)
}
message(sprintf("fHMM code loaded %10s"," "))
rm(e,exe,file,installed_packages)
