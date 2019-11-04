### VITERBI ALGORITHM

viterbi <- function(N,theta,x){
		T <- length(x)
		Gamma <- diag(N)
		Gamma[!Gamma] <- theta[1:(N*(N-1))]
		for(i in 1:N){
			Gamma[i,i] <- 1-(rowSums(Gamma)[i]-1)
		}
		theta <- theta[-(1:(N*(N-1)))]
		delta <- solve(t(diag(N)-Gamma+1),rep(1,N))

		mu <- theta[1:N]
		sigma <- theta[(N+1):(2*N)]
		df <- theta[(2*N+1):(3*N)]

		allprobs <- matrix(1,N,T)
		for(i in 1:N){
			allprobs[i,] <- (1/sigma[i])*dt((x-mu[i])/sigma[i],df[i])
		}
		# allprobs stores the state-dependent densities, rows are the states, columns are the time points

		xi <- matrix(0,N,T)
		# xi stores the recursive highest possible probability of sequences, rows are the states, columns are the time points

		for(i in 1:N){
			xi[i,1] <- log(delta[i])+log(allprobs[i,1])
		}
		for (t in 2:T){
			for(i in 1:N){
				xi[i,t] <- max(xi[,t-1]+log(Gamma[,i]))+log(allprobs[i,t])
			}
		}

		iv <- numeric(T)
		# iv stores the most-likely state-sequence

		iv[T] <- which.max(xi[,T])
		for (t in (T-1):1){
			iv[t] <- which.max(xi[,t]+log(Gamma[,iv[t+1]]))
		}
		return(iv)
}

applyViterbi <- function(observations,est,controls){
  
  M      = controls[["M"]]
  N      = controls[["N"]]
  T      = controls[["T"]]
  T_star = controls[["T_star"]]

  #initialize theta

  obs        = observations
  states     = matrix(0,ncol=T_star+1,nrow=T)
  states[,1] = viterbi(M,theta,obs[,1])

  for(i in 1:T){
	  states[i,-1] <- viterbi(N,theta_star[[cs_s[i,1]]],obs[i,-1])
  }

  return(states)

}
