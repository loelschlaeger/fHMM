### definition of Viterbi algorithm for state decoding
viterbi = function(observations,nstates,Gamma,mus,sigmas,dfs,sdd){
		T = length(observations)
		delta = Gamma2delta(Gamma)
		allprobs = matrix(0,nstates,T)
		
		for(n in seq_len(nstates)){
			if(sdd=="t"){
			  allprobs[n,] = (1/sigmas[n])*dt((observations-mus[n])/sigmas[n],dfs[n])
			}
		  if(sdd=="gamma"){
		    allprobs[n,] = dgamma(observations,shape=mus[n]^2/sigmas[n]^2,scale=sigmas[n]^2/mus[n])
		  }
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

### execution of Viterbi algorithm
apply_viterbi = function(data,fit,controls){
  if(is.null(controls[["controls_checked"]])){
    stop(sprintf("%s (%s)",exception("C.1")[2],exception("C.1")[1]),call.=FALSE)
  }
  
  observations = data[["logReturns"]]
  thetaList = fit[["thetaList"]]
  states = controls[["states"]]
  
  if(controls[["model"]]=="HMM"){
    decoding = viterbi(observations,
                       states[1],
                       thetaList[["Gamma"]],
                       thetaList[["mus"]],
                       thetaList[["sigmas"]],
                       thetaList[["dfs"]],
                       controls[["sdds"]][1])
  }
  
  if(controls[["model"]]=="HHMM"){
    T = dim(observations)[1]
    T_star = data[["T_star"]]
    decoding = matrix(NA,ncol=max(T_star)+1,nrow=T)
    decoding[,1] = viterbi(observations[,1],
                           states[1],
                           thetaList[["Gamma"]],
                           thetaList[["mus"]],
                           thetaList[["sigmas"]],
                           thetaList[["dfs"]],
                           controls[["sdds"]][1])
    for(t in seq_len(T)){
      curr = decoding[t,1]
      decoding[t,-1] = c(viterbi(observations[t,-1][!is.na(observations[t,-1])],
                                 states[2],
                                 thetaList[["Gammas_star"]][[curr]],
                                 thetaList[["mus_star"]][[curr]],
                                 thetaList[["sigmas_star"]][[curr]],
                                 thetaList[["dfs_star"]][[curr]],
                                 controls[["sdds"]][2]),rep(NA,max(T_star)-T_star[t]))
    }
  }
  
  check_decoding(decoding,data,controls)
  
  return(decoding)
}