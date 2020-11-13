### compute log-likelihood of HMM and HHMM
source("trans.R")

### INPUT:  unconstrained parameter vector, observations, states 
### OUTPUT: negative log-likelihood of HMM
nLL_hmm = function(thetaUncon,observations,N){
  T = length(observations)
  
  gammasUncon = thetaUncon[1:((N-1)*N)]; thetaUncon = thetaUncon[-(1:((N-1)*N))]
  Gamma = gammasUncon2Gamma(gammasUncon,N)
  delta = Gamma2delta(Gamma)
  mu = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
  sigmasUncon = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
  sigma = sigmaUncon2sigmaCon(sigmasUncon)
  df = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
  
  # allprobs stores the state-dependent densities, rows are the states, columns are the time points
  # might produce warnings (if the probabilities are rounded to zero and hence the logarithm is NA in the following)
  allprobs = matrix(0,N,T)
  for (i in 1:N){
  	allprobs[i,] = 1/sigma[i]*dt((observations-mu[i])/sigma[i],df[i])
  }
  		
  LL = LogLikeHMM_Rcpp(allprobs,Gamma,delta,N,T) 
  return(-LL)
}

### INPUT:  unconstrained parameter vector, observations, control parameters
### OUTPUT: negative log-likelihood of HHMM
nLL_hhmm = function(thetaUncon,observations,controls){
  M      = controls[["M"]]
  N      = controls[["N"]]
  est_df = controls[["est_df"]]
	x      = observations
	x_cs   = x[,1]
	T      = length(x_cs)
	T_star = length(x[1,-1])
	gammasUncon = thetaUncon[1:((M-1)*M)]; thetaUncon = thetaUncon[-(1:((M-1)*M))]
	Gamma       = gammasUncon2Gamma(gammasUncon,M)
	delta       = Gamma2delta(Gamma)
	gammasCon_star = list()
	for(i in 1:M){
		gammasCon_star[[i]] = gammasUncon2gammasCon(thetaUncon[1:((N-1)*N)],N); thetaUncon = thetaUncon[-(1:((N-1)*N))]
	}
	mu = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
	musCon_star = list()
	for(i in 1:M){
		musCon_star[[i]] = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
	}
	sigma = sigmaUncon2sigmaCon(thetaUncon[1:M]); thetaUncon <- thetaUncon[-(1:M)]
	sigmasCon_star = list()
	for(i in 1:M){
		sigmasCon_star[[i]] = sigmaUncon2sigmaCon(thetaUncon[1:N]); thetaUncon = thetaUncon[-(1:N)]
	}
	if(est_df=="yes") { 
	  df = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)] 
	}
	if(est_df=="no")  { 
	  df = rep(controls[["set_df_cs"]],M)
	}
	dfsCon_star = list()
	for(i in 1:M){
	  if(est_df=="yes")  {dfsCon_star[[i]] = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]}
	  if(est_df=="no")   {dfsCon_star[[i]] = rep(controls[["set_df_fs"]],N)}
	}
	allprobs = log_likelihoods = matrix(0,M,T)
	thetaCon_step = list()
	for (i in 1:M){
		allprobs[i,] = 1/sigma[i]*dt((x_cs-mu[i])/sigma[i],df[i])
		thetaCon_step[[i]] = c(gammasCon_star[[i]],musCon_star[[i]],sigmasCon_star[[i]],dfsCon_star[[i]])
	}
	for (i in 1:M){
	  log_likelihoods[i,] = - apply(x[,-1], 1, logL_hmm, thetaCon_step[[i]], N) #minus added since now nLL_hmm gives neg. LL
	}
	nloglike = -LogLikeHHMM_Rcpp(log_likelihoods=log_likelihoods,allprobs=allprobs,Gamma=Gamma,delta=delta,M=M,T=T) 
	
	return(nloglike)
}
