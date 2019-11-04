plotEstimationError <- function(theta,est,controls) {
  
  M = controls[["M"]]
  N = controls[["N"]]
  est_df     = controls[["est_df"]]
	
	diff_gamma = (theta[["gammas"]]-est[["gammas"]]) / theta[["gammas"]]
	diff_mu    = (theta[["mus"]]-est[["mus"]])       / theta[["mus"]]
	diff_sigma = (theta[["sigmas"]]-est[["sigmas"]]) / theta[["sigmas"]]
	diff_df    = (theta[["dfs"]]-est[["dfs"]])       / theta[["dfs"]]
	
	par(mfrow=c(2,2))
	xlab = "Parameter Index"
	ylab = "Relative Error"
	
	plot(diff_gamma,xlab=xlab,ylab=ylab,main="Gamma")
	plot(diff_mu,xlab=xlab,ylab=ylab,main="Mu")
	plot(diff_sigma,xlab=xlab,ylab=ylab,main="Sigma")
	if(est_df=="all" || est_df=="fscs") { plot(diff_df,xlab=xlab,ylab=ylab,main="DF") }
}
