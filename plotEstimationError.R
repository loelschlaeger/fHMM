plotEstimationError = function(theta,est,controls) {
  
  M = controls[["M"]]
  N = controls[["N"]]
  est_df     = controls[["est_df"]]
	
	diff_gamma = theta[1:(M*(M-1)+M*N*(N-1))]-est[1:(M*(M-1)+M*N*(N-1))]; theta = theta[-(1:(M*(M-1)+M*N*(N-1)))]; est = est[-(1:(M*(M-1)+M*N*(N-1)))]
	diff_mu    = theta[1:(M+M*N)]-est[1:(M+M*N)]; theta = theta[-(1:(M+M*N))]; est = est[-(1:(M+M*N))]    
	diff_sigma = theta[1:(M+M*N)]-est[1:(M+M*N)]; theta = theta[-(1:(M+M*N))]; est = est[-(1:(M+M*N))]  
	if(est_df=="all" || est_df=="fscs") {diff_df    = theta-est}     
	
	par(mfrow=c(2,2))
	xlab = "parameter index"
	ylab = "true minus estimate"
	
	plot(diff_gamma,xlab=xlab,ylab=ylab,main="Gamma")
	plot(diff_mu,xlab=xlab,ylab=ylab,main="Mu")
	plot(diff_sigma,xlab=xlab,ylab=ylab,main="Sigma")
	if(est_df=="all" || est_df=="fscs") { plot(diff_df,xlab=xlab,ylab=ylab,main="DF") }
}
