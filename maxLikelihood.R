### MAXIMIZATION OF THE LIKELIHOOD (using nlm)

source("computeLikelihood.R")

#install.packages("Rcpp")
#install.packages("RcppArmadillo")
library(Rcpp)
library(RcppArmadillo)
sourceCpp("P:/HHMMFinance/RHHMMFinance/LogLike.cpp") # C++ script required for the likelihood evaluation 

source("transformParameters.R")
source("initializeEstimation.R")
source("createOutputFile.R")

maxLikelihood <- function(observations,controls){
  
  M          = controls[["M"]]
  N          = controls[["N"]]
  runs       = controls[["runs"]]
  output     = controls[["output"]]
  iterlim    = controls[["iterlim"]]
  
  cat("Start with estimation. This will take some time.\n")

	llks = rep(NA,runs) 
	mods = list() 
	t1 = Sys.time()
	for (k in 1:runs){ 
		tryCatch({
			start = initializeEstimation(controls) 
			mods[[k]] = nlm(f=logL_hhmm,p=start,observations=observations,controls=controls,iterlim=iterlim,steptol = 1e-10)
			llks[k] = mods[[k]]$minimum
			},
			error=function(e){cat("Fehler in diesem Durchgang:",conditionMessage(e), "\n")}
			)
		cat(paste(round(k/runs*100,1),"%\n"))
	}
	cat(paste("Done with estimation. Computation time:", Sys.time()-t1,"\n"))
	
	mod = mods[[which.min(llks)]]
	est = mod$estimate
	est = backTransform(est,controls)
	
	cat(paste("Iterations:",mod$iterations,"(if close to iteration limit, consider to increase)\n "))
	cat("Gradient:\n")
	print(mod$gradient)
	cat("Estimates:\n")
	print(est)

	if(output){createOutputFile(est,controls)}

	return(est)
}
