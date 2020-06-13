# Simulate data from HHMM

source("trans.R")

simulateHHMM = function(controls){
  M      = controls[["M"]]
  N      = controls[["N"]]
  T      = controls[["T"]]
  T_star = controls[["T_star"]]
  est_df = controls[["est_df"]]
	states       = matrix(0,T,T_star+1) 
	observations = matrix(0,T,T_star+1)

	## hidden coarse scale process
	gammasUncon = log(runif((M-1)*M,0,0.3))
	Gamma       = gammasUncon2Gamma(gammasUncon,M)
	delta       = Gamma2delta(Gamma,M)

	## observed coarse scale process
	mus     = seq(-1,1,length.out=M)*10^(-3)
	sigmas  = rev(seq(0.1,1,length.out=M)*10^(-2))
	if(est_df=="yes") {
	  dfs      = sample(1:30,M,replace=TRUE)
	}
	if(est_df=="no")  {
	  dfs      = rep(controls[["set_df_cs"]],M)
	}

	## hidden fine scale processes
	gammasUncon_star = list()
	Gammas_star = list()
	deltas_star = list()
	for(i in 1:M){
		gammasUncon_star[[i]]  = log(runif((N-1)*N,0,0.3))
		Gammas_star[[i]]       = gammasUncon2Gamma(gammasUncon_star[[i]],N)
		deltas_star[[i]]       = Gamma2delta(Gammas_star[[i]],N)
	}

	## observed fine scale processes
	mus_star = list()
	sigmas_star = list()
	for(i in 1:M){
	  mus_star[[i]] = (seq(-1,1,length.out=N)+i-round(M/2))*10^(-3)
	  sigmas_star[[i]] = (rev(seq(0.1,1,length.out=N)+M-i)*10^(-2))
	}
	dfs_star = list() 
	if(est_df=="yes") {
	  dfsUncon_star = list()
	  for(i in 1:M) {
	    dfs_star[[i]] = sample(1:30,N,replace=TRUE)
	  } 
	}
	if(est_df=="no")  {
	  dfsUncon_star = c()
	  for(i in 1:M) {
	    dfs_star[[i]] = rep(controls[["set_df_fs"]],N)
	  }
	}
	
	## parameter vector thetaUncon
	thetaUncon = c(gammasUncon,unlist(gammasUncon_star),mus,unlist(mus_star),sigmaCon2sigmaUncon(sigmas),sigmaCon2sigmaUncon(unlist(sigmas_star)),dfs,unlist(dfs_star))
	
	## parameter vector thetaFull
	thetaFull = list(
	  "Gamma"       = Gamma,
	  "mus"         = mus,
	  "sigmas"      = sigmas,
	  "dfs"         = dfs,
	  "Gammas_star" = Gammas_star,
	  "mus_star"    = mus_star,
	  "sigmas_star" = sigmas_star,
	  "dfs_star"    = dfs_star
	) 

	## simulate HMMs
	simulateStates = function(delta,Gamma,T){
		N      = length(delta)
		states = 1:N
		seq    = numeric(T)
		seq[1] = sample(states,1,prob=delta)
		for(t in 2:T){
			seq[t] = sample(states,1,prob=Gamma[seq[t-1],])
		}
		return(seq)
	}
	simulateObservations = function(states,mus,sigmas,dfs,T,N){
		obs = numeric(T)
		for(t in 1:T){
			obs[t] = rt(1,dfs[states[t]])*sigmas[states[t]]+mus[states[t]]
		}
		return(obs)
	}
	## simulate HHMM
	states[,1]        = simulateStates(Gamma2delta(thetaFull[["Gamma"]],M),thetaFull[["Gamma"]],T)
	observations[,1]  = simulateObservations(states[,1],thetaFull[["mus"]],thetaFull[["sigmas"]],thetaFull[["dfs"]],T,M)
	for(t in 1:T){
		cs                 = states[t,1]
		states[t,-1]       = simulateStates(Gamma2delta(thetaFull[["Gammas_star"]][[cs]],N),thetaFull[["Gammas_star"]][[cs]],T_star)
		observations[t,-1] = simulateObservations(states[t,-1],thetaFull[["mus_star"]][[cs]],thetaFull[["sigmas_star"]][[cs]],thetaFull[["dfs_star"]][[cs]],T_star,N)
	}
	
	output = list(
	  "observations" = observations,
	  "states"       = states,
	  "thetaUncon"   = thetaUncon,
	  "thetaCon"     = thetaUncon2thetaCon(thetaUncon,controls),
	  "thetaFull"    = thetaFull
	)

	return(output)
}


