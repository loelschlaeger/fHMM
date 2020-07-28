# helper functions for parameter transformations

# INPUT:  matrix
# OUTPUT: non-diagonal matrix elements (column-wise)
Gamma2gammas = function(Gamma){
	diag(Gamma) = NA
	return(Gamma[!is.na(Gamma)])
}

# INPUT:  unconstrained non-diagonal elements of transition probability matrix
# OUTPUT: transition probability matrix
gammasUncon2Gamma = function(gammas,N){
	Gamma         = diag(N)
	Gamma[!Gamma] = exp(gammas) # filled column-wise
	Gamma         = Gamma/rowSums(Gamma)
	return(Gamma)
}

# INPUT:  constrained non-diagonal elements of transition probability matrix
# OUTPUT: transition probability matrix
gammasCon2Gamma = function(gammas,N){
	Gamma         = diag(N)
	Gamma[!Gamma] = gammas # filled column-wise
	for(i in 1:N){
		Gamma[i,i] = 1-(rowSums(Gamma)[i]-1)
	}
	return(Gamma)
}

# INPUT:  unconstrained non-diagonal elements of transition probability matrix
# OUTPUT: constrained non-diagonal elements of transition probability matrix
gammasUncon2gammasCon = function(gammasUncon,N){
  gammasCon = Gamma2gammas(gammasUncon2Gamma(gammasUncon,N))
  return(gammasCon)
}

# INPUT:  transition probability matrix
# OUTPUT: stationary distribution
Gamma2delta = function(Gamma,N){
  delta = solve(t(diag(N)-Gamma+1),rep(1,N))
  return(delta)
}

# INPUT:  unconstrained parameter sigma
# OUTPUT: constrained parameter sigma
sigmaUncon2sigmaCon = function(sigmaUncon){
  sigmaCon = exp(sigmaUncon)
  return(sigmaCon)
}

# INPUT:  constrained parameter sigma
# OUTPUT: unconstrained parameter sigma
sigmaCon2sigmaUncon = function(sigmaCon){
  sigmaUncon = log(sigmaCon)
  return(sigmaUncon)
}

# INPUT:  unconstrained estimates, list of control parameter 
# OUTPUT: constrained estimates in vector form
thetaUncon2thetaCon = function(thetaUncon,controls){
	M      = controls[["M"]]
	N      = controls[["N"]]
	est_df = controls[["est_df"]]
	gammasUncon = thetaUncon[1:((M-1)*M)];	thetaUncon  = thetaUncon[-(1:((M-1)*M))]
	Gamma       = gammasUncon2Gamma(gammasUncon,M)
	gammas      = Gamma2gammas(Gamma)
	for(i in 1:M){
	  gammasUncon_star      = thetaUncon[1:((N-1)*N)]; thetaUncon = thetaUncon[-(1:((N-1)*N))]
	  Gamma_star            = gammasUncon2Gamma(gammasUncon_star,N)
	  gammas_star           = Gamma2gammas(Gamma_star)
	  gammas = c(gammas,gammas_star)
	}
	mus     = thetaUncon[1:(M+M*N)]; thetaUncon = thetaUncon[-(1:(M+M*N))]
	sigmasUncon  = thetaUncon[1:(M+M*N)]; thetaUncon = thetaUncon[-(1:(M+M*N))]
	sigmas       = sigmaUncon2sigmaCon(sigmasUncon)
	if(est_df=="yes") {dfs = thetaUncon[1:(M+M*N)]; thetaUncon = thetaUncon[-(1:(M+M*N))]} 
	if(est_df=="no")  {dfs = c() }
	thetaCon = c(gammas,mus,sigmas,dfs)
	return(thetaCon)
}

# INPUT:  constrained estimates (thetaCon), list of control parameter (controls)
# OUTPUT: full constrained estimates in list form
thetaCon2thetaFull = function(thetaCon,controls){
  M      = controls[["M"]]
  N      = controls[["N"]]
  est_df = controls[["est_df"]]
  set_df_cs  = controls[["set_df_cs"]] 
  set_df_fs  = controls[["set_df_fs"]]
  gammas      = thetaCon[1:((M-1)*M)]; thetaCon = thetaCon[-(1:((M-1)*M))]
  Gamma       = gammasCon2Gamma(gammas,M)
  Gammas_star = list()
  for(i in 1:M){
    gammas_star      = thetaCon[1:((N-1)*N)]; thetaCon = thetaCon[-(1:((N-1)*N))]
    Gammas_star[[i]] = gammasCon2Gamma(gammas_star,N)
  }
  mus      = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
  mus_star = list()
  for(i in 1:M){
    mus_star[[i]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
  }
  sigmas      = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
  sigmas_star = list()
  for(i in 1:M){
    sigmas_star[[i]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
  }
  dfs_star = list()
  if(est_df=="yes") {
    dfs      = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
    for(i in 1:M){
      dfs_star[[i]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
    }
  } 
  if(est_df=="no")  {
    dfs      = rep(set_df_cs,M);
    for(i in 1:M){
      dfs_star[[i]] = rep(set_df_fs,N)
    }
  }
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
  return(thetaFull)
}