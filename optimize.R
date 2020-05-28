### MAXIMIZATION OF THE LIKELIHOOD (using nlm)

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
  outputFile = controls[["outputFile"]]
  
  cat("Start with likelihood maximization\n")
  cat("run (estimated time remaining in min)\n")

	llks = rep(NA,runs) 
	mods = list() 
	start_time = Sys.time()
	for (k in 1:runs){ 
		tryCatch({
			start = initializeEstimation(controls) 
			mods[[k]] = nlm(f=logL_hhmm,p=start,observations=observations,controls=controls,iterlim=iterlim,steptol = 1e-08,print.level=0)
			llks[k] = mods[[k]]$minimum
			},
			error=function(e){cat(paste("(Error:",conditionMessage(e), ")"),"\n")}
			)
      mid_time = Sys.time()
      if(k%%5==0) cat(paste0(k," (",round(difftime(mid_time,start_time,units="secs")/60*(runs-k)/k),")","\n"))
	}
	end_time = Sys.time()
	cat(paste("Done with estimation.", difftime(end_time,start_time),"\n"))
	
	mod       = mods[[which.min(llks)]]
	thetaCon  = thetaUncon2thetaCon(mod$estimate,controls)
	thetaFull = thetaCon2thetaFull(thetaCon,controls)
	
	cat(paste("Iterations:",mod$iterations,"(if close to iteration limit, consider to increase)\n "))
	
	if(outputFile){
	  source("createOutputFile.R")
	}

	return(list(
	  "likelihood"= -mod$minimum,
	  "thetaCon"  = thetaCon,
	  "thetaFull" = thetaFull,
	  "gradient"  = mod$gradient
	  )  
	)
}
