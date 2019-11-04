### LOG-LIKELIHOOD OF THE HMMS



# INPUT: parameter vector (theta, of the form gammas,mus,sigmas,dfs), observations (x), fs states (N)
# OUTPUT: log-likelihood of HMM
logL_hmm <- function(x,theta,N,cpp){
			T <- length(x)
			
			gammas <- theta[1:((N-1)*N)]; theta <- theta[-(1:((N-1)*N))]
			Gamma <- gammasUncon2Gamma(gammas,N)
			
			#catch cases in which there is no stationary distribution
			if(class(try(solve(t(diag(N)-Gamma+1),rep(1,N)),silent=TRUE))=="try-error"){
				delta <- rep(1/N,N)
			} else {
				delta <- solve(t(diag(N)-Gamma+1),rep(1,N))
			}
			
			mu <- theta[1:N]; theta <- theta[-(1:N)]
			sigma <- exp(theta[1:N]); theta <- theta[-(1:N)]
			df <- theta[1:N]; theta <- theta[-(1:N)]; df <- round(df*30)+1

			allprobs <- matrix(0,N,T)
			for (i in 1:N){
				allprobs[i,] <- 1/sigma[i]*dt((x-mu[i])/sigma[i],df[i])
			}
			# allprobs stores the state-dependent densities, rows are the states, columns are the time points
			# might produce warnings (if the probabilities are rounded to zero and hence the logarithm is NA in the following)		

			if(cpp==FALSE){
				phi <- matrix(0,N,T)
				# phi stores the log-forward probabilities, rows are the states, columns are the time points 
				for(i in 1:N){
					phi[i,1] <- log(delta[i]*allprobs[i,1])
				}
				for (t in 2:T){
					c <- max(phi[,t-1])
					for (i in 1:N){
						phi[i,t] <- log(sum(exp(phi[,t-1]+log(Gamma[,i])-c)))+c+log(allprobs[i,t])
					}
				}
			
				c <- max(phi[,T])
				loglike = log(sum(exp(phi[,T]-c)))+c
			}

			if(cpp==TRUE){
				loglike = LogLikeHMM_Rcpp(allprobs=allprobs,Gamma=Gamma,delta=delta,N=N,T=T) # computing the log-likelihood
			}
			return(loglike)
}

### LOG-LIKELIHOOD OF THE HHMM

# INPUT: parameter vector (theta, of the form gammas,mus,sigmas,dfs), observations, cs states (M), fs states (N)
# OUTPUT: NEGATIVE log-likelihood of HHMM
logL_hhmm <- function(theta,observations,controls){
  
      start <- Sys.time()
  
      M      = controls[["M"]]
      N      = controls[["N"]]
      cpp    = controls[["cpp"]]
      est_df = controls[["est_df"]]
      
			x      = observations
			x_cs   = x[,1]
			T      = length(x_cs)
			T_star = length(x[1,-1])

			gammas = theta[1:((M-1)*M)]; theta = theta[-(1:((M-1)*M))]
			Gamma  = gammasUncon2Gamma(gammas,M)
			

			#catch cases in which there is no stationary distribution
			if(class(try(solve(t(diag(M)-Gamma+1),rep(1,M)),silent=TRUE))=="try-error"){ 
			  delta = rep(1/M,M) 
			} else { 
			  delta = solve(t(diag(M)-Gamma+1),rep(1,M)) 
			}
			
			gammas_star = list()
			for(i in 1:M){
				gammas_star[[i]] = theta[1:((N-1)*N)];theta = theta[-(1:((N-1)*N))]
			}

			mu = theta[1:M]; theta = theta[-(1:M)]
			mus_star <- list()
			for(i in 1:M){
				mus_star[[i]] <- theta[1:N]; theta <- theta[-(1:N)]
			}

			sigma <- exp(theta[1:M]); theta <- theta[-(1:M)]
			sigmas_star = list()
			for(i in 1:M){
				sigmas_star[[i]] = theta[1:N]; theta = theta[-(1:N)]
			}
			
			if(est_df=="all") { df = round(theta[1:M]*30)+1; theta = theta[-(1:M)] }
			if(est_df=="fscs"){ df = rep(round(theta[1]*30)+1,M);   theta = theta[-1] }
			if(est_df=="no")  { df = rep(controls[["set_df_cs"]],M)}
			dfs_star <- list()
			for(i in 1:M){
			  if(est_df=="all")  {dfs_star[[i]] = theta[1:N]; theta = theta[-(1:N)]}
			  if(est_df=="fscs") {dfs_star[[i]] = rep(round(theta[1]*30)+1,N);   }
			  if(est_df=="no")   {dfs_star[[i]] = rep(controls[["set_df_fs"]],N)}
			}
			
			
			allprobs = log_likelihoods = matrix(0,M,T)
			theta_step = list()
			for (i in 1:M){
				allprobs[i,] = 1/sigma[i]*dt((x_cs-mu[i])/sigma[i],df[i])
				theta_step[[i]] = c(gammas_star[[i]],mus_star[[i]],sigmas_star[[i]],dfs_star[[i]])
			}

			#registerDoSEQ()
			#tmp = foreach(i = 1:M) %:% foreach(t = 1:T) %dopar% {
			#  log_likelihoods[i,t] = logL_hmm(theta_step[[i]],x[t,-1],N,cpp)
			#}
			
			#for(i in 1:M){
			#  for(t in 1:T){
			#    log_likelihoods[i,t] = logL_hmm(x[t,-1],theta_step[[i]],N,cpp)
			#  }
			#}
			
			for (i in 1:M){
			  log_likelihoods[i,] = apply(x[,-1], 1, logL_hmm, theta_step[[i]], N, cpp)
			}
			
			
			if(cpp==FALSE){
			  phi <- matrix(0,M,T)
			  # phi stores the log-forward probabilities, rows are the states, columns are the time points
			  for(i in 1:M){
			  	phi[i,1] <- log(delta[i]) + log_likelihoods[i,1] + log(allprobs[i,1])
			  }
			  for (t in 2:T){
			  	c <- max(phi[,t-1])
			  	for(i in 1:M){
				  	phi[i,t] <- log(sum(exp(phi[,t-1]+log(Gamma[,i])-c)))+c+log_likelihoods[i,t]+log(allprobs[i,t])
			  	}
		  	}

		  	c <- max(phi[,T])
		  	nloglike = -(log(sum(exp(phi[,T]-c)))+c)
		  	
			}
			
			if(cpp==TRUE){
			  nloglike = -LogLikeHHMM_Rcpp(log_likelihoods=log_likelihoods,allprobs=allprobs,Gamma=Gamma,delta=delta,M=M,T=T) 
			}
			
			# return the negative log-likelihood since nlm minimizes
			return(nloglike)
}