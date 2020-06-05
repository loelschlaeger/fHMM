### Maximization of the likelihood function using nlm

library(Rcpp)
library(RcppArmadillo)
sourceCpp("loglike_cpp.cpp")
source("loglike.R")
source("transformations.R")
source("initialize.R")

maxLikelihood = function(observations,controls){
  
  M          = controls[["M"]]
  N          = controls[["N"]]
  runs       = controls[["runs"]]
  iterlim    = controls[["iterlim"]]
  
  cat("Start with likelihood maximization\n")
  cat("run (estimated time remaining in min)\n")

	llks = rep(NA,runs) 
	mods = list() 
	start_time = Sys.time()
	for (k in 1:runs){
	  start = initializeEstimation(controls) 
		tryCatch({
			  mods[[k]] = nlm(f=logL_hhmm,p=start,observations=observations,controls=controls,iterlim=iterlim,steptol=1e-08,print.level=2)
			  llks[k] = mods[[k]]$minimum
			},error=function(e){cat(paste("(Error:",conditionMessage(e), ")"),"\n")}
		)
    cat(paste0(k," of ",runs," (",round(difftime(Sys.time(),start_time,units="mins")*(runs-k)/k),")","\n"))
	}
	end_time = Sys.time()
	cat(paste("Done with estimation.", round(difftime(end_time,start_time,units="mins")),"mins total estimation time. \n"))
	
	mod       = mods[[which.min(llks)]]
	thetaCon  = thetaUncon2thetaCon(mod$estimate,controls)
	thetaFull = thetaCon2thetaFull(thetaCon,controls)
	
	cat(paste("Needed",mod$iterations,"for convergence.\n "))

	return(list(
	  "all_likelihoods" = -llks,
	  "likelihood"      = -mod$minimum,
	  "thetaCon"        = thetaCon,
	  "thetaFull"       = thetaFull,
	  "gradient"        = mod$gradient
	  )  
	)
}
