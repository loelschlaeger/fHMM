### LOG-LIKELIHOOD OF THE HMMS


# INPUT:  constrained parameter vector thetaCon, observations (x), fs states (N)
# OUTPUT: log-likelihood of HMM
logL_hmm = function(x,thetaCon,N,cpp){
      T = length(x)
			
			gammas = thetaCon[1:((N-1)*N)]; thetaCon = thetaCon[-(1:((N-1)*N))]
			Gamma  = gammasCon2Gamma(gammas,N)
			
			#catch cases in which there is no stationary distribution
			if(class(try(Gamma2delta(Gamma,N),silent=TRUE))=="try-error"){ 
			  delta = rep(1/N,N)
			} else { 
			  delta = Gamma2delta(Gamma,N)
			}
			
			mu    = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
			sigma = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
			df    = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]

			allprobs = matrix(0,N,T)
			for (i in 1:N){
				allprobs[i,] = 1/sigma[i]*dt((x-mu[i])/sigma[i],df[i])
			}
			# allprobs stores the state-dependent densities, rows are the states, columns are the time points
			# might produce warnings (if the probabilities are rounded to zero and hence the logarithm is NA in the following)		

			if(cpp==FALSE){
				phi = matrix(0,N,T)
				# phi stores the log-forward probabilities, rows are the states, columns are the time points 
				for(i in 1:N){
					phi[i,1] = log(delta[i]*allprobs[i,1])
				}
				for (t in 2:T){
					c = max(phi[,t-1])
					for (i in 1:N){
						phi[i,t] = log(sum(exp(phi[,t-1]+log(Gamma[,i])-c)))+c+log(allprobs[i,t])
					}
				}
			
				c = max(phi[,T])
				loglike = log(sum(exp(phi[,T]-c)))+c
			}

			if(cpp==TRUE){
				loglike = LogLikeHMM_Rcpp(allprobs=allprobs,Gamma=Gamma,delta=delta,N=N,T=T) # computing the log-likelihood
			}
			return(loglike)
}

### LOG-LIKELIHOOD OF THE HHMM

# INPUT:  unconstrained parameter vector thetaUncon, observations, cs states (M), fs states (N)
# OUTPUT: NEGATIVE log-likelihood of HHMM
logL_hhmm = function(thetaUncon,observations,controls){
  
      M      = controls[["M"]]
      N      = controls[["N"]]
      cpp    = controls[["cpp"]]
      est_df = controls[["est_df"]]
      
			x      = observations
			x_cs   = x[,1]
			T      = length(x_cs)
			T_star = length(x[1,-1])

			gammasUncon = thetaUncon[1:((M-1)*M)]; thetaUncon = thetaUncon[-(1:((M-1)*M))]
			Gamma       = gammasUncon2Gamma(gammasUncon,M)
			
			#catch cases in which there is no stationary distribution
			if(class(try(Gamma2delta(Gamma,M),silent=TRUE))=="try-error"){ 
			  delta = rep(1/M,M)
			} else { 
			  delta = Gamma2delta(Gamma,M)
			}
			
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
			
			if(est_df=="all") { 
			  df = dfUncon2dfCon(thetaUncon[1:M]); thetaUncon = thetaUncon[-(1:M)] 
			}
			if(est_df=="fscs"){ 
			  df = rep(dfUncon2dfCon(thetaUncon[1]),M);   thetaUncon = thetaUncon[-1] 
			}
			if(est_df=="no")  { 
			  df = rep(controls[["set_df_cs"]],M)
			}
			dfsCon_star = list()
			for(i in 1:M){
			  if(est_df=="all")  {dfsCon_star[[i]] = dfUncon2dfCon(thetaUncon[1:N]);        thetaUncon = thetaUncon[-(1:N)]}
			  if(est_df=="fscs") {dfsCon_star[[i]] = rep(dfUncon2dfCon(thetaUncon[1]),N);   thetaUncon = thetaUncon[-1]    }
			  if(est_df=="no")   {dfsCon_star[[i]] = rep(controls[["set_df_fs"]],N)}
			}
			
			allprobs = log_likelihoods = matrix(0,M,T)
			thetaCon_step = list()
			for (i in 1:M){
				allprobs[i,] = 1/sigma[i]*dt((x_cs-mu[i])/sigma[i],df[i])
				thetaCon_step[[i]] = c(gammasCon_star[[i]],musCon_star[[i]],sigmasCon_star[[i]],dfsCon_star[[i]])
			}

			for (i in 1:M){
			  log_likelihoods[i,] = apply(x[,-1], 1, logL_hmm, thetaCon_step[[i]], N, cpp)
			}
			
			if(cpp==FALSE){
			  phi = matrix(0,M,T)
			  # phi stores the log-forward probabilities, rows are the states, columns are the time points
			  for(i in 1:M){
			  	phi[i,1] = log(delta[i]) + log_likelihoods[i,1] + log(allprobs[i,1])
			  }
			  for (t in 2:T){
			  	c = max(phi[,t-1])
			  	for(i in 1:M){
				  	phi[i,t] = log(sum(exp(phi[,t-1]+log(Gamma[,i])-c)))+c+log_likelihoods[i,t]+log(allprobs[i,t])
			  	}
		  	}

		  	c = max(phi[,T])
		  	nloglike = -(log(sum(exp(phi[,T]-c)))+c)
		  	
			}
			
			if(cpp==TRUE){
			  nloglike = -LogLikeHHMM_Rcpp(log_likelihoods=log_likelihoods,allprobs=allprobs,Gamma=Gamma,delta=delta,M=M,T=T) 
			}
			
			# return the negative log-likelihood since nlm minimizes
			return(nloglike)
}