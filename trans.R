### helper functions for parameter transformations

### INPUT:  transition probability matrix
### OUTPUT: non-diagonal matrix elements (column-wise)
Gamma2gammasCon = function(Gamma){
	diag(Gamma) = NA
	return(Gamma[!is.na(Gamma)])
}

### INPUT:  constrained non-diagonal elements of (dim x dim)-transition probability matrix
### OUTPUT: transition probability matrix
gammasCon2Gamma = function(gammasCon,dim){
  Gamma         = diag(dim)
  Gamma[!Gamma] = gammasCon #filled column-wise
  for(i in 1:dim){
    Gamma[i,i] = 1-(rowSums(Gamma)[i]-1)
  }
  return(Gamma)
}

### INPUT:  unconstrained non-diagonal elements of (dim x dim)-transition probability matrix
### OUTPUT: transition probability matrix
gammasUncon2Gamma = function(gammasUncon,dim){
	Gamma         = diag(dim)
	Gamma[!Gamma] = exp(gammasUncon) #filled column-wise
	Gamma         = Gamma/rowSums(Gamma)
	return(Gamma)
}

### INPUT:  transition probability matrix
### OUTPUT: unconstrained non-diagonal elements of transition probability matrix
Gamma2gammasUncon = function(Gamma){
  diag(Gamma) = 0
  Gamma       = log(Gamma/(1-rowSums(Gamma)))
  diag(Gamma) = NA
  return(Gamma[!is.na(Gamma)])
}

# INPUT:  unconstrained non-diagonal elements of (dim x dim)--transition probability matrix
# OUTPUT: constrained non-diagonal elements of transition probability matrix
gammasUncon2gammasCon = function(gammasUncon,dim){
  gammasCon = Gamma2gammasCon(gammasUncon2Gamma(gammasUncon,dim))
  return(gammasCon)
}

# INPUT:  constrained non-diagonal elements of (dim x dim)-transition probability matrix
# OUTPUT: unconstrained non-diagonal elements of transition probability matrix
gammasCon2gammasUncon = function(gammasCon,dim){
  gammasUncon = Gamma2gammasUncon(gammasCon2Gamma(gammasCon,dim))
  return(gammasUncon)
}

# INPUT:  transition probability matrix
# OUTPUT: stationary distribution
Gamma2delta = function(Gamma){
  dim   = dim(Gamma)[1]
  if(class(try(solve(t(diag(dim)-Gamma+1),rep(1,dim)),silent=TRUE))=="try-error"){ 
    delta = rep(1/dim,dim)
    warning(call.=FALSE,"Computation of stationary distribution failed, I will continue with uniform distribution.")
  } else { 
    delta = solve(t(diag(dim)-Gamma+1),rep(1,dim))
  }
  return(delta)
}

# INPUT:  unconstrained parameter sigma
# OUTPUT: constrained parameter sigma
sigmaUncon2sigmaCon = function(sigmaUncon){
  return(exp(sigmaUncon))
}

# INPUT:  constrained parameter sigma
# OUTPUT: unconstrained parameter sigma
sigmaCon2sigmaUncon = function(sigmaCon){
  return(log(sigmaCon))
}

# INPUT:  unconstrained model parameters and control parameters 
# OUTPUT: constrained model parameters in vector form
thetaUncon2thetaCon = function(thetaUncon,controls){
  M  = controls[["states"]][1] #coarse-scale states
  N  = controls[["states"]][2] #fine-scale states
  df_cs = controls[["fix_df"]][1]
  df_fs = controls[["fix_df"]][2]
  
	gammasUncon = thetaUncon[1:((M-1)*M)]; thetaUncon = thetaUncon[-(1:((M-1)*M))]
	Gamma       = gammasUncon2Gamma(gammasUncon,M)
	gammasCon   = Gamma2gammasCon(Gamma)
	for(m in seq_len(M)){
	  gammasUncon_star = thetaUncon[1:((N-1)*N)]; thetaUncon = thetaUncon[-(1:((N-1)*N))]
	  gammasCon_star   = gammasUncon2gammasCon(gammasUncon_star,N)
	  gammasCon        = c(gammasCon,gammasCon_star)
	}
	
	mus = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
	for(m in seq_len(M)){
	  mus_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
	  mus      = c(mus,mus_star)
	}
	
	sigmasUncon  = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
	sigmasCon    = sigmaUncon2sigmaCon(sigmasUncon)
	for(m in seq_len(M)){
	  sigmasUncon_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
	  sigmasCon_star   = sigmaUncon2sigmaCon(sigmasUncon_star)
	  sigmasCon        = c(sigmasCon,sigmasCon_star)
	}
	
	dfs = if(is.na(df_cs)){thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]} else integer(0)
	for(m in seq_len(M)){
	  dfs_star = if(is.na(df_fs)) {thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]} else integer(0)
	  dfs      = c(dfs,dfs_star)
	}
	
	thetaCon = c(gammasCon,mus,sigmasCon,dfs)
	return(thetaCon)
}

# INPUT:  constrained model parameters and control parameters
# OUTPUT: constrained model parameters in list form
thetaCon2thetaList = function(thetaCon,controls){
  M  = controls[["states"]][1] #coarse-scale states
  N  = controls[["states"]][2] #fine-scale states
  df_cs = controls[["fix_df"]][1]
  df_fs = controls[["fix_df"]][2]
  
  gammasCon   = thetaCon[1:((M-1)*M)]; thetaCon = thetaCon[-(1:((M-1)*M))]
  Gamma       = gammasCon2Gamma(gammasCon,M)
  Gammas_star = list()
  for(m in seq_len(M)){
    gammasCon_star   = thetaCon[1:((N-1)*N)]; thetaCon = thetaCon[-(1:((N-1)*N))]
    Gammas_star[[m]] = gammasCon2Gamma(gammasCon_star,N)
  }
  
  mus      = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
  mus_star = list()
  for(m in seq_len(M)){
    mus_star[[m]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
  }
  
  sigmasCon      = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
  sigmasCon_star = list()
  for(m in seq_len(M)){
    sigmasCon_star[[m]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
  }
  
  dfs = if(is.na(df_cs)) {thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]} else rep(df_cs,M)
  dfs_star = list()
  for(m in seq_len(M)){
    dfs_star[[m]] = if(is.na(df_fs)) {thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]} else rep(df_fs,N)
  }
  
  thetaList = list(
    "Gamma"       = Gamma,
    "mus"         = mus,
    "sigmas"      = sigmasCon,
    "dfs"         = dfs,
    "Gammas_star" = Gammas_star,
    "mus_star"    = mus_star,
    "sigmas_star" = sigmasCon_star,
    "dfs_star"    = dfs_star
  ) 
  return(thetaList)
}
