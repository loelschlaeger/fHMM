### maximization of the likelihood function

library(Rcpp)
library(RcppArmadillo)
library(progress)
sourceCpp("loglike.cpp")
source("loglike.R")
source("trans.R")
source("init.R")
source("checks.R")

maxLikelihood = function(data,controls){
  observations = data[["observations"]]
  runs = controls[["runs"]]
  llks = rep(NA,runs) 
  mods = list() 
  if(controls[["model"]]=="HMM") target = nLL_hmm
  if(controls[["model"]]=="HHMM") target = nLL_hhmm
  
  writeLines("Started estimation.")
  pb = progress_bar$new(format = "[:bar] :percent eta: :eta", total = runs, clear = TRUE)
  start = Sys.time()
	for (k in 1:runs){
	  suppressWarnings({ tryCatch({ mods[[k]] = nlm(f = target,
    			                                        p = init_est(controls),
                            			                observations = observations,
                            			                controls = controls,
                            			                iterlim = controls[["iterlim"]],
                            			                steptol = controls[["steptol"]],
                            			                print.level = controls[["print.level"]],
                            			                hessian = controls[["hessian"]]
    			                                        )
    		                          if(mods[[k]]$code %in% controls[["accept_codes"]]) llks[k] = mods[[k]]$minimum
  			                        },error = function(e){})
  		              })
	  pb$tick()
	}
  end = Sys.time()
  
	est = check_estimation(round(difftime(end,start,units='mins')),mods,llks,data,controls)
	
	return(est)
}
