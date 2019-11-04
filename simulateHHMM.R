source("transformParameters.R")

simulateHHMM <- function(controls){
  
  M      = controls[["M"]]
  N      = controls[["N"]]
  T      = controls[["T"]]
  T_star = controls[["T_star"]]
  est_df = controls[["est_df"]]
  
	states       = matrix(0,T,T_star+1) 
	observations = matrix(0,T,T_star+1)

	## hidden crude scale process
	Gamma         = diag(M)
	Gamma[!Gamma] = runif((M-1)*M,0,0.5)
	Gamma         = Gamma/rowSums(Gamma)
	delta         = solve(t(diag(M)-Gamma+1),rep(1,M))

	## observed crude scale process
	mu     = seq(-1,1,length.out=M)*10^(-3)
	sigma  = rev(seq(0.1,1,length.out=M)*10^(-2))
	if(est_df=="all") {df     = round(seq.int(1,20,length.out=M)) }
	if(est_df=="fscs"){df     = 5 }
	if(est_df=="no")  {df     = controls[["set_df_cs"]]}
	par    = c(mu,sigma,df)

	## hidden fine scale processes
	Gammas_star <- list()
	deltas_star <- list()
	for(i in 1:M){
		G <- diag(N)
		G[!G] <- runif((N-1)*N,0,0.5)
		G <- G/rowSums(G)
		d <- solve(t(diag(N)-G+1),rep(1,N))
		Gammas_star[[i]] <- G
		deltas_star[[i]] <- d
	}

	## observed fine scale processes
	mus_star <- list()
	sigmas_star <- list()
	if(est_df=="all") {dfs_star = list(); for(i in 1:M) {dfs_star[[i]] = round(seq.int(1,20,length.out=N)) } }
	if(est_df=="fscs"){dfs_star = 1 }
	if(est_df=="no")  {dfs_star = controls[["set_df_fs"]]}
	pars_star <- list()
	for(i in 1:M){
		mus_star[[i]]    = seq(-1,1,length.out=N)*10^(-3)
		sigmas_star[[i]] = rev(seq(0.1,1,length.out=N)*10^(-2))
		if(est_df=="all")  {pars_star[[i]] = c(mus_star[[i]],sigmas_star[[i]],dfs_star[[i]])}
		if(est_df=="fscs") {pars_star[[i]] = c(mus_star[[i]],sigmas_star[[i]],dfs_star)}
		if(est_df=="no")   {pars_star[[i]] = c(mus_star[[i]],sigmas_star[[i]],dfs_star)}
	}

	## create parameter vector theta for comparison
	gammas4theta <- c(Gamma2gammas(Gamma))
	for(i in 1:M){
		gammas4theta <- c(gammas4theta,Gamma2gammas(Gammas_star[[i]]))
	}
	mus4theta <- c(mu,unlist(mus_star))
	sigmas4theta <- c(sigma,unlist(sigmas_star))
	dfs4theta <- c(df,unlist(dfs_star))
	theta <- list(
	  "gammas" = gammas4theta,
	  "mus"    = mus4theta,
	  "sigmas" = sigmas4theta,
	  "dfs"    = dfs4theta
	)

	## simulate HMMs
	simulateStates <- function(delta,Gamma,T){
		N <- length(delta)
		states <- 1:N
		seq <- numeric(T)
		seq[1] <- sample(states,1,prob=delta)
		for(t in 2:T){
			seq[t] <- sample(states,1,prob=Gamma[seq[t-1],])
		}
		return(seq)
	}

	simulateObservations <- function(states,par,T,N){
		mus <- par[1:N]; par <- par[-(1:N)]
		sigmas <- par[1:N]; par <- par[-(1:N)]
		if(est_df=="all"){dfs <- par[1:N]}
		if(est_df=="fscs" || est_df=="no"){dfs <- rep(par[1],N)}
		obs <- numeric(T)
		for(t in 1:T){
			obs[t] <- rt(1,dfs[states[t]])*sigmas[states[t]]+mus[states[t]]
		}
		return(obs)
	}

	## simulate HHMM
	states[,1] <- simulateStates(delta,Gamma,T)
	observations[,1] <- simulateObservations(states[,1],par,T,N=M)

	for(t in 1:T){
		cs_state <- states[t,1]
		states[t,-1] <- simulateStates(deltas_star[[cs_state]],Gammas_star[[cs_state]],T_star)
		observations[t,-1] <- simulateObservations(states[t,-1],pars_star[[cs_state]],T_star,N)
	}
	
	output = list(
	  "observations" = observations,
	  "states" = states,
	  "theta" = theta
	)

	return(output)
}


