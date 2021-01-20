### Viterbi algorithm for state decoding
viterbi = function(observations,nstates,Gamma,mus,sigmas,dfs){
		T = length(observations)
		delta = Gamma2delta(Gamma)
		allprobs = matrix(0,nstates,T)
		
		for(n in seq_len(nstates)){
			allprobs[n,] = (1/sigmas[n])*dt((observations-mus[n])/sigmas[n],dfs[n])
		}
		
		xi = matrix(0,nstates,T)
		for(n in seq_len(nstates)){
			xi[n,1] = log(delta[n])+log(allprobs[n,1])
		}
		for (t in seq_len(T)[-1]){
			for(n in seq_len(nstates)){
				xi[n,t] = max(xi[,t-1]+log(Gamma[,n]))+log(allprobs[n,t])
			}
		}
		
		iv = numeric(T)
		iv[T] = which.max(xi[,T])
		for (t in rev(seq_len(T-1))){
			iv[t] = which.max(xi[,t]+log(Gamma[,iv[t+1]]))
		}
		
		return(iv)
}

apply_viterbi = function(data,fit,controls){
  if(is.null(controls[["controls_checked"]])) stop("'controls' invalid",call.=FALSE)
  
  observations = data[["logReturns"]]
  thetaList = fit[["thetaList"]]
  states = controls[["states"]]
  model = controls[["model"]]
  
  if(model=="HMM"){
    decoding = viterbi(observations,states[1],thetaList[["Gamma"]],thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]])
  }
  
  if(model=="HHMM"){
    T = dim(observations)[1]
    T_star = dim(observations)[2]-1
    decoding = matrix(0,ncol=T_star+1,nrow=T)
    decoding[,1] = viterbi(observations[,1],states[1],thetaList[["Gamma"]],thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]])
    for(t in seq_len(T)){
      curr = decoding[t,1]
      decoding[t,-1] = viterbi(observations[t,-1],states[2],thetaList[["Gammas_star"]][[curr]],thetaList[["mus_star"]][[curr]],thetaList[["sigmas_star"]][[curr]],thetaList[["dfs_star"]][[curr]])
    }
  }
  
  check_decoding(decoding,controls)
  
  return(decoding)
}