# INPUT:  matrix
# OUTPUT: non-diagonal matrix elements, column-wise
Gamma2gammas = function(Gamma){
	diag(Gamma) = NA
	return(Gamma[!is.na(Gamma)])
}

# INPUT:  unconstrained non-diagonal elements of tpm
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

# INPUT:  constrained non-diagonal elements of transition probability matrix
# OUTPUT: transition probability matrix
Gamma2delta = function(Gamma,N){
  delta = solve(t(diag(N)-Gamma+1),rep(1,N))
  return(delta)
}

# INPUT:  unconstrained parameter sigma
# OUTPUT: constrained parameter sigma
sigmaUncon2sigmaCon = function(sigmaUncon){
  sigmaCon = exp(SigmaUncon)
  return(sigmaCon)
}

# INPUT:  unconstrained parameter df
# OUTPUT: constrained parameter df
dfUncon2dfCon = function(dfUncon){
  dfCon = round(dfUncon*30)+1
  return(dfCon)
}

# INPUT:  unconstrained estimates (theta), list of control parameter (controls)
# OUTPUT: constrained estimates in vector form
thetaUncon2thetaCon = function(thetaUncon,controls){
	
	M      = controls[["M"]]
	N      = controls[["N"]]
	est_df = controls[["est_df"]]
	
	gammasUncon = thetaUncon[1:((M-1)*M)]; thetaUncon = thetaUncon[-(1:((M-1)*M))]
	Gamma       = gammasUncon2Gamma(gammasUncon,M)
	gammas      = Gamma2gammas(Gamma)
	for(i in 1:M){
	  gammas_starUncon      = thetaUncon[1:((N-1)*N)]; thetaUncon = thetaUncon[-(1:((N-1)*N))]
	  Gamma_star            = gammasUncon2Gamma(gammas_starUncon,N)
	  gammas_star           = Gamma2gammas(gammas_star)
	  gammas = c(gammas,gammas_star)
	}
	
	mus     = thetaUncon[1:(M+M*N)]; thetaUncon = thetaUncon[-(1:(M+M*N))]
	
	sigmasUncon  = thetaUncon[1:(M+M*N)]; thetaUncon = thetaUncon[-(1:(M+M*N))]
	sigmas       = sigmaUncon2sigmaCon(sigmasUncon)
	
	if(est_df=="all") {dfsUncon     = thetaUncon[1:(M+M*N)]; thetaUncon = thetaUncon[-(1:(M+M*N))]} 
	if(est_df=="fscs"){dfsUncon     = thetaUncon[1:2];       thetaUncon = thetaUncon[-(1:2)]      }
	if(est_df=="no")  {dfsUncon     = c() }
	
	dfs = dfUncon2dfCon(dfsUncon)
	
	thetaCon = c(gammas,mus,sigmas,dfs)
	return(thetaCon)
}

# INPUT:  constrained estimates (thetaCon), list of control parameter (controls)
# OUTPUT: full constrained estimates in list form
thetaCon2thetaFull = function(thetaCon,controls){
  M      = controls[["M"]]
  N      = controls[["N"]]
  est_df = controls[["est_df"]]
  
  gammas      = thetaCon[1:((M-1)*M)]; thetaCon = thetaCon[-(1:((M-1)*M))]
  Gamma       = gammas2Gamma(gammas,M)
  Gammas_star = list()
  for(i in 1:M){
    gammas_star      = thetaCon[1:((N-1)*N)]; thetaCon = thetaCon[-(1:((N-1)*N))]
    Gammas_star[[i]] = gammas2Gamma(gammas_star,N)
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
  
  if(est_df=="all") {
    dfs      = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
    dfs_star = list()
    for(i in 1:M){
      dfs_star[[i]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
    }
  } 
  if(est_df=="fscs"){
    dfs      = thetaCon[1];
    dfs_star = thetaCon[2]; thetaCon = thetaCon[-(1:2)]
  }
  if(est_df=="no")  {
    dfs = dfs_star = "not estimated"
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
