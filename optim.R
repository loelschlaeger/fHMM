# maximization of the likelihood function

## required packages
library(Rcpp)
library(RcppArmadillo)

sourceCpp("loglike_cpp.cpp")
source("loglike.R")
source("trans.R")
source("init.R")

maxLikelihood = function(observations,controls){
  M          = controls[["M"]]
  N          = controls[["N"]]
  runs       = controls[["runs"]]
  iterlim    = controls[["iterlim"]]
  cat("Start likelihood maximization\n")
  cat("run (estimated time remaining in min)\n")
	llks = rep(NA,runs) 
	mods = list() 
	
	start_time = Sys.time()
	for (k in 1:runs){
	  start = init_est(controls) 
		tryCatch({
			  mods[[k]] = nlm(f=logL_hhmm,p=start,observations=observations,controls=controls,iterlim=iterlim,steptol=1e-08,print.level=0)
			  llks[k] = mods[[k]]$minimum
			},error=function(e){cat(paste("(Error:",conditionMessage(e),")"),"\n")}
		)
    if(k%%10==0) cat(paste0(k," of ",runs," (",round(difftime(Sys.time(),start_time,units="mins")*(runs-k)/k),")","\n"))
	}
	end_time = Sys.time()
	cat(paste("Done with estimation.", round(difftime(end_time,start_time,units="mins")),"mins total estimation time. \n"))
	
	mod       = mods[[which.min(llks)]]
	thetaCon  = thetaUncon2thetaCon(mod$estimate,controls)
	thetaFull = thetaCon2thetaFull(thetaCon,controls)
	
	return(list(
	  "likelihood"      = -mod$minimum,
	  "all_likelihoods" = -llks,
	  "thetaFull"       = thetaFull,
	  "gradient"        = mod$gradient
	  )  
	)
}
