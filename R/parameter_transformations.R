#' Unconstrains expected values.
#' @param muCon Vector of constrained expected values.
#' @param link Boolean, determining whether to apply the link function.
#' @return Vector of unconstrained expected values.
#' @keywords internal

muCon2muUncon = function(muCon,link){
  if(link) muUncon = log(muCon)
  if(!link) muUncon = muCon
  return(muUncon)
}

#' Constrains expected values.
#' @param muUncon Vector of unconstrained expected values.
#' @param link Boolean, determining whether to apply the link function.
#' @return Vector of constrained expected values.
#' @keywords internal

muUncon2muCon = function(muUncon,link){
  if(link) muCon = exp(muUncon)
  if(!link) muCon = muUncon
  return(muCon)
}

#' Unconstrains standard deviations.
#' @param sigmaCon Vector of constrained standard deviations.
#' @return Vector of unconstrained standard deviations.
#' @keywords internal

sigmaCon2sigmaUncon = function(sigmaCon){
  return(log(sigmaCon))
}

#' Constrains standard deviations.
#' @param sigmaUncon Vector of unconstrained standard deviations.
#' @return Vector of constrained standard deviations.
#' @keywords internal

sigmaUncon2sigmaCon = function(sigmaUncon){
  return(exp(sigmaUncon))
}

#' Unconstrains degrees of freedom.
#' @param dfCon Numeric, constrained degrees of freedom.
#' @return Numeric, unconstrained degrees of freedom.
#' @keywords internal

dfCon2dfUncon = function(dfCon){
  return(log(dfCon))
}

#' Constrains degrees of freedom.
#' @param dfUncon Numeric, unconstrained degrees of freedom.
#' @return Numeric, constrained degrees of freedom.
#' @keywords internal

dfUncon2dfCon = function(dfUncon){
  return(exp(dfUncon))
}

#' Computes stationary distribution of transition probability matrix.
#' @param Gamma Transition probability matrix.
#' @return Stationary distribution vector.
#' @keywords internal

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

#' Constrains non-diagonal matrix elements of transition probability matrix.
#' @details Function may shift 0 and 1 non-diagonal elements by \code{1e-3}.
#' @param Gamma Transition probability matrix.
#' @param shift Boolean, determining wheter to shift boundary probabilities.
#' @return Vector of constrained non-diagonal matrix elements (column-wise).
#' @keywords internal

Gamma2gammasCon = function(Gamma,shift=FALSE){
  gammasCon = Gamma[row(Gamma)!=col(Gamma)] 
  if(shift){
    gammasCon = replace(gammasCon,gammasCon==0,1e-3)
    gammasCon = replace(gammasCon,gammasCon==1,1-1e-3)
  }
  return(gammasCon)
}

#' Unconstraines non-diagonal matrix elements of transition probability matrix.
#' @param Gamma Transition probability matrix.
#' @return Vector of unconstrained non-diagonal matrix elements (column-wise).
#' @keywords internal

Gamma2gammasUncon = function(Gamma){
  diag(Gamma) = 0
  Gamma       = log(Gamma/(1-rowSums(Gamma)))
  diag(Gamma) = NA
  return(Gamma[!is.na(Gamma)])
}

#' Builds transition probability matrix from constrained non-diagonal elements.
#' @param gammasCon Vector of constrained non-diagonal elements of transition probability matrix.
#' @param dim Numeric, dimension of transition probability matrix.
#' @return Transition probability matrix.
#' @keywords internal

gammasCon2Gamma = function(gammasCon,dim){
  Gamma         = diag(dim)
  Gamma[!Gamma] = gammasCon 
  for(i in 1:dim){
    Gamma[i,i] = 1-(rowSums(Gamma)[i]-1)
  }
  return(Gamma)
}

#' Unconstrains non-diagonal elements of transition probability matrix.
#' @param gammasCon Vector of constrained non-diagonal elements of transition probability matrix.
#' @param dim Numeric, dimension of transition probability matrix.
#' @return Vector of unconstrained non-diagonal elements of transition probability matrix.
#' @keywords internal

gammasCon2gammasUncon = function(gammasCon,dim){
  gammasUncon = Gamma2gammasUncon(gammasCon2Gamma(gammasCon,dim))
  return(gammasUncon)
}

#' Builds transition probability matrix from unconstrained non-diagonal elements.
#' @param gammasUncon Vector of unconstrained non-diagonal elements of transition probability matrix.
#' @param dim Numeric, dimension of transition probability matrix.
#' @return Transition probability matrix.
#' @keywords internal

gammasUncon2Gamma = function(gammasUncon,dim){
  Gamma         = diag(dim)
  Gamma[!Gamma] = exp(gammasUncon)
  Gamma         = Gamma/rowSums(Gamma)
  return(Gamma)
}

#' Constrains non-diagonal elements of transition probability matrix.
#' @param gammasUncon Vector of unconstrained non-diagonal elements of transition probability matrix.
#' @param dim Numeric, dimension of transition probability matrix.
#' @return Vector of constrained non-diagonal elements of transition probability matrix.
#' @keywords internal

gammasUncon2gammasCon = function(gammasUncon,dim){
  gammasCon = Gamma2gammasCon(gammasUncon2Gamma(gammasUncon,dim))
  return(gammasCon)
}

#' Brings contrained model parameters from vector form \code{thetaCon} to list form \code{ThetaList}.
#' @param thetaCon Constrained model parameters in vector form.
#' @param controls A list of controls.
#' @return Constrained model parameters in list form.
#' @keywords internal

thetaCon2thetaList = function(thetaCon,controls){
  M = controls[["states"]][1] 
  N = controls[["states"]][2]
  
  gammasCon = thetaCon[1:((M-1)*M)]; thetaCon = thetaCon[-(1:((M-1)*M))]
  Gamma = gammasCon2Gamma(gammasCon,M)
  if(controls[["model"]]=="hhmm"){
    Gammas_star = list()
    for(m in seq_len(M)){
      gammasCon_star   = thetaCon[1:((N-1)*N)]; thetaCon = thetaCon[-(1:((N-1)*N))]
      Gammas_star[[m]] = gammasCon2Gamma(gammasCon_star,N)
    }
  }
  
  musCon = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
  if(controls[["model"]]=="hhmm"){ 
    musCon_star = list()
    for(m in seq_len(M)){
      musCon_star[[m]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
    }
  }
  
  sigmasCon = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
  if(controls[["model"]]=="hhmm"){
    sigmasCon_star = list()
    for(m in seq_len(M)){
      sigmasCon_star[[m]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
    }
  }
  
  if(controls[["sdds"]][1] == "t"){
    if(is.na(controls[["fixed_dfs"]][1])){
      dfsCon = thetaCon[1:M]; thetaCon = thetaCon[-(1:M)]
    } else {
      dfsCon = rep(controls[["fixed_dfs"]][1],M)
    }
  }
  if(controls[["sdds"]][1] == "gamma"){
    dfsCon = NULL
  }
  
  if(controls[["model"]]=="hhmm"){ 
    if(controls[["sdds"]][2] == "t"){
      dfsCon_star = list()
      if(is.na(controls[["fixed_dfs"]][2])){
        for(m in seq_len(M)){
          dfsCon_star[[m]] = thetaCon[1:N]; thetaCon = thetaCon[-(1:N)]
        }
      } else {
        for(m in seq_len(M)){
          dfsCon_star[[m]] = rep(controls[["fixed_dfs"]][2],N)
        }
      }
    }
    if(controls[["sdds"]][2] == "gamma"){
      dfsCon_star = NULL
    }
  }
  
  if(controls[["model"]]=="hmm"){
    thetaList = list(
      "Gamma"       = Gamma,
      "mus"         = musCon,
      "sigmas"      = sigmasCon,
      "dfs"         = dfsCon
    ) 
  }
  if(controls[["model"]]=="hhmm"){
    thetaList = list(
      "Gamma"       = Gamma,
      "mus"         = musCon,
      "sigmas"      = sigmasCon,
      "dfs"         = dfsCon,
      "Gammas_star" = Gammas_star,
      "mus_star"    = musCon_star,
      "sigmas_star" = sigmasCon_star,
      "dfs_star"    = dfsCon_star
    ) 
  }
  return(thetaList)
}

#' Unconstrains model parameters.
#' @param thetaCon Constrained model parameters in vector form.
#' @param controls A list of controls.
#' @return Unconstrained model parameters in vector form.
#' @keywords internal

thetaCon2thetaUncon = function(thetaCon,controls){
  return(thetaList2thetaUncon(thetaCon2thetaList(thetaCon,controls),controls))
}

#' Brings constrained model parameters from list form \code{thetaList} to vector form \code{thetaCon}.
#' @param thetaList Constrained model parameters in list form.
#' @param controls A list of controls.
#' @return Constrained model parameters in vector form.
#' @keywords internal

thetaList2thetaCon = function(thetaList,controls){
  return(thetaUncon2thetaCon(thetaList2thetaUncon(thetaList,controls),controls))
}

#' Orders states in \code{thetaList} based on expected values.
#' @param thetaList Constrained model parameters in list form.
#' @param controls A list of controls.
#' @return Constrained and ordered model parameters in list form.
#' @details 
#' If state-dependent distribution is *t*, states are ordered decreasing with respect to expected values. If distribution is *Gamma*, vice versa.
#' @keywords internal

thetaList2thetaListOrdered = function(thetaList,controls){
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
  if(controls$model=="hhmm"){
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

#' Brings constrained model parameters in list form to unconstrained parameters in vector form.
#' @param thetaList Constrained model parameters in list form.
#' @param controls A list of controls.
#' @return Unconstrained model parameters in vector form.
#' @keywords internal

thetaList2thetaUncon = function(thetaList,controls){
  if(controls[["model"]]=="hmm"){
    gammasUncon = Gamma2gammasUncon(thetaList[["Gamma"]])
    musUncon    = muCon2muUncon(thetaList[["mus"]],link=(controls[["sdds"]][1] == "gamma"))
    sigmasUncon = sigmaCon2sigmaUncon(thetaList[["sigmas"]])
    if(controls[["sdds"]][1]=="t" && is.na(controls[["fixed_dfs"]][1])){
      dfsUncon = dfCon2dfUncon(thetaList[["dfs"]])
      thetaUncon = c(gammasUncon,musUncon,sigmasUncon,dfsUncon)
    } else {
      thetaUncon = c(gammasUncon,musUncon,sigmasUncon)
    }
  }
  if(controls[["model"]]=="hhmm"){
    gammasUncon      = Gamma2gammasUncon(thetaList[["Gamma"]])
    gammasUncon_star = vector()
    for(s in seq_len(controls[["states"]][1])){
      gammasUncon_star = c(gammasUncon_star,Gamma2gammasUncon(thetaList[["Gammas_star"]][[s]]))
    }
    musUncon      = muCon2muUncon(thetaList[["mus"]],link=(controls[["sdds"]][1] == "gamma"))
    musUncon_star = vector()
    for(s in seq_len(controls[["states"]][1] )){
      musUncon_star = c(musUncon_star,muCon2muUncon(thetaList[["mus_star"]][[s]],link=(controls[["sdds"]][2] == "gamma")))
    }
    sigmasUncon      = sigmaCon2sigmaUncon(thetaList[["sigmas"]])
    sigmasUncon_star = vector()
    for(s in seq_len(controls[["states"]][1] )){
      sigmasUncon_star = c(sigmasUncon_star,sigmaCon2sigmaUncon(thetaList[["sigmas_star"]][[s]]))
    }
    if(controls[["sdds"]][1]=="t" & is.na(controls[["fixed_dfs"]][1])){
      dfsUncon = dfCon2dfUncon(thetaList[["dfs"]])
      if(controls[["sdds"]][2]=="t" & is.na(controls[["fixed_dfs"]][2])){
        dfsUncon_star = vector()
        for(s in seq_len(controls[["states"]][1] )){
          dfsUncon_star = c(dfsUncon_star,dfCon2dfUncon(thetaList[["dfs_star"]][[s]]))
        }
        thetaUncon = c(gammasUncon,gammasUncon_star,musUncon,musUncon_star,sigmasUncon,sigmasUncon_star,dfsUncon,dfsUncon_star)
      } else {
        thetaUncon = c(gammasUncon,gammasUncon_star,musUncon,musUncon_star,sigmasUncon,sigmasUncon_star,dfsUncon)
      }
    } else {
      if(controls[["sdds"]][2]=="t" & is.na(controls[["fixed_dfs"]][2])){
        dfsUncon_star = vector()
        for(s in seq_len(controls[["states"]][1] )){
          dfsUncon_star = c(dfsUncon_star,dfCon2dfUncon(thetaList[["dfs_star"]][[s]]))
        }
        thetaUncon = c(gammasUncon,gammasUncon_star,musUncon,musUncon_star,sigmasUncon,sigmasUncon_star,dfsUncon_star)
      } else {
        thetaUncon = c(gammasUncon,gammasUncon_star,musUncon,musUncon_star,sigmasUncon,sigmasUncon_star)
      }
    }
  }
  return(thetaUncon)
}

#' Constrains model parameters.
#' @param thetaUncon Unconstrained model parameters in vector form.
#' @param controls A list of controls.
#' @return Constrained model parameters in vector form.
#' @keywords internal

thetaUncon2thetaCon = function(thetaUncon,controls){
  M  = controls[["states"]][1] 
  N  = controls[["states"]][2]
  if(controls[["model"]]=="hmm"){
    gammasUncon = thetaUncon[1:((M-1)*M)]; thetaUncon = thetaUncon[-(1:((M-1)*M))]
    gammasCon   = gammasUncon2gammasCon(gammasUncon,M)
    musUncon    = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
    musCon      = muUncon2muCon(musUncon,link=(controls[["sdds"]][1] == "gamma"))
    sigmasUncon = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
    sigmasCon   = sigmaUncon2sigmaCon(sigmasUncon)
    if(controls[["sdds"]][1] == "t"){
      if(is.na(controls[["fixed_dfs"]][1])){
        dfsUncon = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
        dfsCon   = dfUncon2dfCon(dfsUncon)
      } else {
        dfsCon = integer(0)
      }
      thetaCon = c(gammasCon,musCon,sigmasCon,dfsCon)
    }
    if(controls[["sdds"]][1] == "gamma"){
      thetaCon = c(gammasCon,musCon,sigmasCon)
    }
  }
  if(controls[["model"]]=="hhmm"){
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
        dfsUncon = thetaUncon[1:M]; thetaUncon = thetaUncon[-(1:M)]
        dfsCon   = dfUncon2dfCon(dfsUncon)
      } else {
        dfsCon = integer(0)
      }
      if(controls[["sdds"]][2] == "t"){
        if(is.na(controls[["fixed_dfs"]][2])){
          for(m in seq_len(M)){
            dfsUncon_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
            dfsCon_star   = dfUncon2dfCon(dfsUncon_star)
            dfsCon        = c(dfsCon,dfsCon_star)
          }
        }
        thetaCon = c(gammasCon,musCon,sigmasCon,dfsCon)
      }
      if(controls[["sdds"]][2] == "gamma"){
        thetaCon = c(gammasCon,musCon,sigmasCon,dfsCon) 
      }
    }
    if(controls[["sdds"]][1] == "gamma"){
      dfsCon = integer(0)
      if(controls[["sdds"]][2] == "t"){
        if(is.na(controls[["fixed_dfs"]][2])){
          for(m in seq_len(M)){
            dfsUncon_star = thetaUncon[1:N]; thetaUncon = thetaUncon[-(1:N)]
            dfsCon_star   = dfUncon2dfCon(dfsUncon_star)
            dfsCon        = c(dfsCon,dfsCon_star)
          }
        }
        thetaCon = c(gammasCon,musCon,sigmasCon,dfsCon)
      }
      if(controls[["sdds"]][2] == "gamma"){
        thetaCon = c(gammasCon,musCon,sigmasCon) 
      }
    }
  }
  return(thetaCon)
}

#' Brings uncontrained model parameters \code{thetaUncon} in list form \code{thetaList}.
#' @param thetaUncon Unconstrained model parameters in vector form.
#' @param controls A list of controls.
#' @return Constrained model parameters in list form.
#' @keywords internal

thetaUncon2thetaList = function(thetaUncon,controls){
  return(thetaCon2thetaList(thetaUncon2thetaCon(thetaUncon,controls),controls))
}

#' Splits uncontrained model parameters \code{thetaUncon} by fine-scale HMMs.
#' @param thetaUncon Unconstrained model parameters in vector form.
#' @param controls A list of controls.
#' @return List of unconstrained fine-scale model parameters for each fine-scale HMM.
#' @keywords internal

thetaUncon2thetaUnconSplit = function(thetaUncon,controls){
  if(controls[["model"]]!="hhmm") stop("function only for HHMMs")
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

#' Brings uncontrained fine-scale model parameters in constrained list form.
#' @param thetaUncon Unconstrained fine-scale model parameters in vector form.
#' @param controls A list of controls.
#' @return Constrained fine-scale model parameters in list form.
#' @keywords internal

thetaUnconSplit2thetaList = function(thetaUncon,controls){
  if(controls[["model"]]!="hhmm") stop("Function only for HHMMs")
  N = controls[["states"]][2]
  gammasUncon = thetaUncon[1:((N-1)*N)]; thetaUncon = thetaUncon[-(1:((N-1)*N))]
  Gamma       = gammasUncon2Gamma(gammasUncon,N)
  musCon      = muUncon2muCon(thetaUncon[1:N],link=(controls[["sdds"]][2]=="gamma")); thetaUncon = thetaUncon[-(1:N)]
  sigmasCon   = sigmaUncon2sigmaCon(thetaUncon[1:N]); thetaUncon = thetaUncon[-(1:N)]
  if(controls[["sdds"]][2]=="t"){
    if(is.na(controls[["fixed_dfs"]][2])){
      dfsCon = dfUncon2dfCon(thetaUncon[1:N]); thetaUncon = thetaUncon[-(1:N)]
    } else {
      dfsCon = rep(controls[["fixed_dfs"]][2],N)
    }
  }
  if(controls[["sdds"]][2]=="gamma"){
    dfsCon = NULL
  }
  thetaList = list(
    "Gamma"  = Gamma,
    "mus"    = musCon,
    "sigmas" = sigmasCon,
    "dfs"    = dfsCon
  ) 
  return(thetaList)
}