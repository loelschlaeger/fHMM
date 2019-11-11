### VITERBI ALGORITHM

viterbi = function(N,thetaFull,x){
		T = length(x)
		Gamma = thetaFull[["Gamma"]]
		delta = Gamma2delta(Gamma,N)

		mu    = thetaFull[["mus"]]
		sigma = thetaFull[["sigmas"]]
		df    = thetaFull[["dfs"]]

		allprobs = matrix(0,N,T)
		for(i in 1:N){
			allprobs[i,] = (1/sigma[i])*dt((x-mu[i])/sigma[i],df[i])
		}
		# allprobs stores the state-dependent densities, rows are the states, columns are the time points

		xi = matrix(0,N,T)
		# xi stores the recursive highest possible probability of sequences, rows are the states, columns are the time points

		for(i in 1:N){
			xi[i,1] = log(delta[i])+log(allprobs[i,1])
		}
		for (t in 2:T){
			for(i in 1:N){
				xi[i,t] = max(xi[,t-1]+log(Gamma[,i]))+log(allprobs[i,t])
			}
		}

		iv = numeric(T)
		# iv stores the most-likely state-sequence

		iv[T] = which.max(xi[,T])
		for (t in (T-1):1){
			iv[t] = which.max(xi[,t]+log(Gamma[,iv[t+1]]))
		}
		return(iv)
}

applyViterbi = function(observations,estFull,controls){
  M      = controls[["M"]]
  N      = controls[["N"]]
  T      = controls[["T"]]
  T_star = controls[["T_star"]]
  
  thetaFullcs = list(
    "Gamma"  = estFull[["Gamma"]],
    "mus"    = estFull[["mus"]],
    "sigmas" = estFull[["sigmas"]],
    "dfs"    = estFull[["dfs"]]
  )

  states     = matrix(0,ncol=T_star+1,nrow=T)
  states[,1] = viterbi(M,thetaFullcs,observations[,1])

  for(i in 1:T){
    s = states[i,1]
    thetaFullcs = list(
      "Gamma"  = estFull[["Gamma"]][[s]],
      "mus"    = estFull[["mus"]][[s]],
      "sigmas" = estFull[["sigmas"]][[s]],
      "dfs"    = estFull[["dfs"]][[s]]
    ) 
	  states[i,-1] = viterbi(N,thetaFullcs,observations[i,-1])
  }

  return(list(
    "states" = states,
    "cs_states" = states[,1],
    "fs_states" = as.vector(t(states[,-1]))
   )
  )

}
