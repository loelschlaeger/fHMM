### MAXIMIZATION OF THE LIKELIHOOD (using nlm)

source("computeLikelihood.R")

library(Rcpp)
library(RcppArmadillo)
sourceCpp("P:/HHMMFinance/RHHMMFinance/LogLike.cpp") # C++ script required for the likelihood evaluation 

source("transformations.R")
source("initializeEstimation.R")

maxLikelihood = function(observations,controls){
  
  M          = controls[["M"]]
  N          = controls[["N"]]
  runs       = controls[["runs"]]
  iterlim    = controls[["iterlim"]]
  outputFile = controls[["outputFile"]]
  
  cat("Start with estimation. This will take some time.\n")
  cat("0%...")

	llks = rep(NA,runs) 
	mods = list() 
	t1 = Sys.time()
	for (k in 1:runs){ 
		tryCatch({
			start = initializeEstimation(controls) 
			mods[[k]] = nlm(f=logL_hhmm,p=start,observations=observations,controls=controls,iterlim=iterlim,steptol = 1e-08,print.level=0)
			llks[k] = mods[[k]]$minimum
			},
			error=function(e){cat("(Fehler:",conditionMessage(e), ")")}
			)
		if(k<runs){cat(paste0(round(k/runs*100,1),"%..."))}
	  if(k==runs){cat("100%\n")}
	}
	t2 = Sys.time()
	cat(paste("Done with estimation.", difftime(t2,t1),"\n"))
	
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
