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
  Gamma[!Gamma] = gammasCon ###filled column-wise
  for(i in 1:dim){
    Gamma[i,i] = 1-(rowSums(Gamma)[i]-1)
  }
  return(Gamma)
}

### INPUT:  unconstrained non-diagonal elements of (dim x dim)-transition probability matrix
### OUTPUT: transition probability matrix
gammasUncon2Gamma = function(gammasUncon,dim){
	Gamma         = diag(dim)
	Gamma[!Gamma] = exp(gammasUncon) ###filled column-wise
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

### INPUT:  unconstrained non-diagonal elements of (dim x dim)-transition probability matrix
### OUTPUT: constrained non-diagonal elements of transition probability matrix
gammasUncon2gammasCon = function(gammasUncon,dim){
  gammasCon = Gamma2gammasCon(gammasUncon2Gamma(gammasUncon,dim))
  return(gammasCon)
}

### INPUT:  constrained non-diagonal elements of (dim x dim)-transition probability matrix
### OUTPUT: unconstrained non-diagonal elements of transition probability matrix
gammasCon2gammasUncon = function(gammasCon,dim){
  gammasUncon = Gamma2gammasUncon(gammasCon2Gamma(gammasCon,dim))
  return(gammasUncon)
}

### INPUT:  transition probability matrix
### OUTPUT: stationary distribution
Gamma2delta = function(Gamma){
  dim   = dim(Gamma)[1]
  if(class(try(solve(t(diag(dim)-Gamma+1),rep(1,dim)),silent=TRUE))=="try-error"){ 
    delta = rep(1/dim,dim)
    warning(sprintf("%s (%s)",exception("F.1")[2],exception("F.1")[1]),call.=FALSE)
  } else { 
    delta = solve(t(diag(dim)-Gamma+1),rep(1,dim))
  }
  return(delta)
}

### INPUT:  unconstrained parameter mu, boolean for link
### OUTPUT: constrained parameter mu
muUncon2muCon = function(muUncon,link){
  if(link) muCon = exp(muUncon)
  if(!link) muCon = muUncon
  return(muCon)
}

### INPUT:  constrained parameter mu, boolean for link
### OUTPUT: unconstrained parameter mu
muCon2muUncon = function(muCon,link){
  if(link) muUncon = log(muCon)
  if(!link) muUncon = muCon
  return(muUncon)
}

### INPUT:  unconstrained parameter sigma
### OUTPUT: constrained parameter sigma
sigmaUncon2sigmaCon = function(sigmaUncon){
  return(exp(sigmaUncon))
}

### INPUT:  constrained parameter sigma
### OUTPUT: unconstrained parameter sigma
sigmaCon2sigmaUncon = function(sigmaCon){
  return(log(sigmaCon))
}

### INPUT:  unconstrained model parameters and control parameters
### OUTPUT: constrained model parameters in vector form
thetaUncon2thetaCon = function(thetaUncon,controls){
  M  = controls[["states"]][1] 
  N  = controls[["states"]][2]
  
  if(controls[["model"]]=="HMM"){
    gammasUncon = thetaUncon[1:((M-1)*M)]; thetaUncon = thetaUncon[-(1:((M-1)*M))]
    gammasCon   = gammasUncon2gammasCon(gammasUncon,M)
    musUncon    = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
    musCon      = muUncon2muCon(musUncon,link=(controls[["sdds"]][1] == "gamma"))
    sigmasUncon = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
    sigmasCon   = sigmaUncon2sigmaCon(sigmasUncon)
    if(controls[["sdds"]][1] == "t"){
      if(is.na(controls[["fixed_dfs"]][1])){
        dfs = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
      } else {
        dfs = integer(0)
      }
      thetaCon = c(gammasCon,musCon,sigmasCon,dfs)
    }
    if(controls[["sdds"]][1] == "gamma"){
      thetaCon = c(gammasCon,musCon,sigmasCon)
    }
  }
  
  if(controls[["model"]]=="HHMM"){
    gammasUncon = thetaUncon[1:((M-1)*M)]; thetaUncon = thetaUncon[-(1:((M-1)*M))]
    gammasCon   = gammasUncon2gammasCon(gammasUncon,M)
    for(m in seq_len(M)){
      gammasUncon_star = thetaUncon[1:((N-1)*N)]; thetaUncon = thetaUncon[-(1:((N-1)*N))]
      gammasCon_star   = gammasUncon2gammasCon(gammasUncon_star,N)
      gammasCon        = c(gammasCon,gammasCon_star)
    }
    
    musUncon = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
    musCon   = muUncon2muCon(musUncon,link=(controls[["sdds"]][1] == "gamma"))
    for(m in seq_len(M)){
      musUncon_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
      musCon_star   = muUncon2muCon(musUncon_star,link=(controls[["sdds"]][2] == "gamma"))
      musCon        = c(musCon,musCon_star)
    }
    
    sigmasUncon = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
    sigmasCon   = sigmaUncon2sigmaCon(sigmasUncon)
    for(m in seq_len(M)){
      sigmasUncon_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
      sigmasCon_star   = sigmaUncon2sigmaCon(sigmasUncon_star)
      sigmasCon        = c(sigmasCon,sigmasCon_star)
    }
    
    if(controls[["sdds"]][1] == "t"){
      if(is.na(controls[["fixed_dfs"]][1])){
        dfs = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
      } else {
        dfs = integer(0)
      }
      if(controls[["sdds"]][2] == "t"){
        if(is.na(controls[["fixed_dfs"]][2])){
          for(m in seq_len(M)){
            dfs_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
            dfs      = c(dfs,dfs_star)
          }
        }
        thetaCon = c(gammasCon,musCon,sigmasCon,dfs)
      }
      if(controls[["sdds"]][2] == "gamma"){
        thetaCon = c(gammasCon,musCon,sigmasCon,dfs) 
      }
    }
    if(controls[["sdds"]][1] == "gamma"){
      dfs = integer(0)
      if(controls[["sdds"]][2] == "t"){
        if(is.na(controls[["fixed_dfs"]][2])){
          for(m in seq_len(M)){
            dfs_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
            dfs      = c(dfs,dfs_star)
          }
        }
        thetaCon = c(gammasCon,musCon,sigmasCon,dfs)
      }
      if(controls[["sdds"]][2] == "gamma"){
        thetaCon = c(gammasCon,musCon,sigmasCon) 
      }
    }
  }

	return(thetaCon)
}

### INPUT:  constrained model parameters and control parameters 
### OUTPUT: constrained model parameters in list form
thetaCon2thetaList = function(thetaCon,controls){
  M = controls[["states"]][1] 
  N = controls[["states"]][2]
  
  gammasCon = thetaCon[1:((M-1)*M)]; thetaCon = thetaCon[-(1:((M-1)*M))]
  Gamma = gammasCon2Gamma(gammasCon,M)
  if(controls[["model"]]=="HHMM"){
    Gammas_star = list()
    for(m in seq_len(M)){
      gammasCon_star   = thetaCon[1:((N-1)*N)]; thetaCon = thetaCon[-(1:((N-1)*N))]
      Gammas_star[[m]] = gammasCon2Gamma(gammasCon_star,N)
    }
  }
  
  musCon = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
  if(controls[["model"]]=="HHMM"){ 
    musCon_star = list()
    for(m in seq_len(M)){
      musCon_star[[m]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
    }
  }
  
  sigmasCon = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
  if(controls[["model"]]=="HHMM"){
    sigmasCon_star = list()
    for(m in seq_len(M)){
      sigmasCon_star[[m]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
    }
  }
  
  if(controls[["sdds"]][1] == "t"){
    if(is.na(controls[["fixed_dfs"]][1])){
      dfs = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
    } else {
      dfs = rep(controls[["fixed_dfs"]][1],M)
    }
  }
  if(controls[["sdds"]][1] == "gamma"){
    dfs = NULL
  }
    
  if(controls[["model"]]=="HHMM"){ 
    if(controls[["sdds"]][2] == "t"){
      dfs_star = list()
      if(is.na(controls[["fixed_dfs"]][2])){
        for(m in seq_len(M)){
          dfs_star[[m]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
        }
      } else {
        for(m in seq_len(M)){
          dfs_star[[m]] =  rep(df_fs,N)
        }
      }
    }
    if(controls[["sdds"]][2] == "gamma"){
      dfs_star = NULL
    }
  }
  
  if(controls[["model"]]=="HMM"){
    thetaList = list(
      "Gamma"       = Gamma,
      "mus"         = musCon,
      "sigmas"      = sigmasCon,
      "dfs"         = dfs
      ) 
  }
  if(controls[["model"]]=="HHMM"){
    thetaList = list(
      "Gamma"       = Gamma,
      "mus"         = musCon,
      "sigmas"      = sigmasCon,
      "dfs"         = dfs,
      "Gammas_star" = Gammas_star,
      "mus_star"    = musCon_star,
      "sigmas_star" = sigmasCon_star,
      "dfs_star"    = dfs_star
      ) 
  }
  return(thetaList)
}

### INPUT:  unconstrained model parameters and control parameters 
### OUTPUT: constrained model parameters in list form
thetaUncon2thetaList = function(thetaUncon,controls){
  return(thetaCon2thetaList(thetaUncon2thetaCon(thetaUncon,controls),controls))
}

### INPUT:  unconstrained model parameters of a HHMM and control parameters 
### OUTPUT: unconstrained model parameters of fine scale in list form
thetaUncon2thetaUnconSplit = function(thetaUncon,controls){
  M = controls[["states"]][1] 
  N = controls[["states"]][2] 
  
  thetaUnconSplit = list()
  thetaUncon = thetaUncon[-(1:((M-1)*M))] 
  for(m in seq_len(M)){
    thetaUnconSplit[[m]] = thetaUncon[1:((N-1)*N)]
    thetaUncon = thetaUncon[-(1:((N-1)*N))] 
  }
  thetaUncon = thetaUncon[-(1:M)]
  for(m in seq_len(M)){
    thetaUnconSplit[[m]] = c(thetaUnconSplit[[m]],thetaUncon[1:N])
    thetaUncon = thetaUncon[-(1:N)] 
  }
  thetaUncon = thetaUncon[-(1:M)] 
  for(m in seq_len(M)){
    thetaUnconSplit[[m]] = c(thetaUnconSplit[[m]],thetaUncon[1:N])
    thetaUncon = thetaUncon[-(1:N)] 
  }
  if(controls[["sdds"]][1]=="t"){
    if(is.na(controls[["fixed_dfs"]][1])){
      thetaUncon = thetaUncon[-(1:M)]
    }
  }
  if(controls[["sdds"]][2]=="t"){
    if(is.na(controls[["fixed_dfs"]][2])){
      for(m in seq_len(M)){
        thetaUnconSplit[[m]] = c(thetaUnconSplit[[m]],thetaUncon[1:N])
        thetaUncon = thetaUncon[-(1:N)] 
      }
    }
  }
  
  return(thetaUnconSplit)
}

### INPUT:  unconstrained model parameters for one fine-scale HMM
### OUTPUT: constrained model parameters in list form for one fine-scale HMM
thetaUnconSplit2thetaList = function(thetaUncon,controls){
   N = controls[["states"]][2]
   gammasUncon = thetaUncon[1:((N-1)*N)]; thetaUncon = thetaUncon[-(1:((N-1)*N))]
   Gamma       = gammasUncon2Gamma(gammasUncon,N)
   musCon      = muUncon2muCon(thetaUncon[1:N],link=(controls[["sdds"]][2]=="gamma")); thetaUncon = thetaUncon[-(1:N)]
   sigmasCon   = sigmaUncon2sigmaCon(thetaUncon[1:N]); thetaUncon = thetaUncon[-(1:N)]
   if(controls[["sdds"]][2]=="t"){
     if(is.na(controls[["fixed_dfs"]][2])){
       dfs = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
     } else {
       dfs = rep(controls[["fixed_dfs"]][2],N)
     }
   }
   if(controls[["sdds"]][2]=="gamma"){
     dfs = NULL
   }
   thetaList = list(
     "Gamma"  = Gamma,
     "mus"    = musCon,
     "sigmas" = sigmasCon,
     "dfs"    = dfs
     ) 
   return(thetaList)
}

### INPUT:  constrained (unordered) model parameters in list form, control parameters
### OUTPUT: constrained ordered model parameters (states decreasing wrt value of mu) in list form
states_decreasing = function(thetaList,controls){
  M = controls[["states"]][1] 
  N = controls[["states"]][2] 

  mu_order = order(thetaList[["mus"]],decreasing=(controls[["sdds"]][1]=="t"))
  permut = diag(M)[mu_order,]
  thetaList[["Gamma"]] = permut %*% thetaList[["Gamma"]] %*% t(permut)
  thetaList[["mus"]] = as.vector(permut %*% thetaList[["mus"]])
  thetaList[["sigmas"]] = as.vector(permut %*% thetaList[["sigmas"]])
  if(controls[["sdds"]][1]=="t"){
    thetaList[["dfs"]] = as.vector(permut %*% thetaList[["dfs"]]); thetaList[["dfs"]][which(is.nan(thetaList[["dfs"]]))] = Inf
  }
    
  if(controls$model=="HHMM"){
    thetaList[["Gammas_star"]] = thetaList[["Gammas_star"]][mu_order]
    thetaList[["mus_star"]] = thetaList[["mus_star"]][mu_order]
    thetaList[["sigmas_star"]] = thetaList[["sigmas_star"]][mu_order]
    if(controls[["sdds"]][2]=="t"){
      thetaList[["dfs_star"]] = thetaList[["dfs_star"]][mu_order]
    }
    for(m in seq_len(M)){
      permut = diag(N)[order(thetaList[["mus_star"]][[m]],decreasing=TRUE),]
      thetaList[["Gammas_star"]][[m]] = permut %*% thetaList[["Gammas_star"]][[m]] %*% t(permut)
      thetaList[["mus_star"]][[m]] = as.vector(permut %*% thetaList[["mus_star"]][[m]])
      thetaList[["sigmas_star"]][[m]] = as.vector(permut %*% thetaList[["sigmas_star"]][[m]])
      if(controls[["sdds"]][2]=="t"){
        thetaList[["dfs_star"]][[m]] = as.vector(permut %*% thetaList[["dfs_star"]][[m]]); thetaList[["dfs_star"]][[m]][which(is.nan(thetaList[["dfs_star"]][[m]]))] = Inf
      }
    }
  }
  return(thetaList)
}

