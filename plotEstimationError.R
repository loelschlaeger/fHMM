plotEstimationError <- function(theta,est,controls) {
  
  M = controls[["M"]]
  N = controls[["N"]]
  est_df     = controls[["est_df"]]
	
	diff_gamma = abs(theta[["gammas"]]-est[["gammas"]]) 
	diff_mu    = abs(theta[["mus"]]-est[["mus"]])       
	diff_sigma = abs(theta[["sigmas"]]-est[["sigmas"]]) 
	diff_df    = abs(theta[["dfs"]]-est[["dfs"]])       
	
	par(mfrow=c(2,2))
	xlab = "Parameter Index"
	ylab = "Absolute Error"
	
	plot(diff_gamma,xlab=xlab,ylab=ylab,main="Gamma")
	plot(diff_mu,xlab=xlab,ylab=ylab,main="Mu")
	plot(diff_sigma,xlab=xlab,ylab=ylab,main="Sigma")
	if(est_df=="all" || est_df=="fscs") { plot(diff_df,xlab=xlab,ylab=ylab,main="DF") }
}
