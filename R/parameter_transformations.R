#' This function transforms an object of class \code{fHMM_parameters} into
#' an object of class \code{parUncon}.
#' @param par
#' An object of class \code{fHMM_parameters}.
#' @param controls 
#' An object of class \code{fHMM_controls}.
#' @return 
#' An object of class \code{parUncon}, i.e. a vector of unconstrained model 
#' parameters to be estimated.
#' @keywords 
#' internal

par2parUncon = function(par, controls){
  stopifnot(class(par) == "fHMM_parameters")
  stopifnot(class(controls) == "fHMM_controls")
  parUncon = Gamma2gammasUncon(par[["Gamma"]])
  if(is.null(controls$sdds[[1]]$pars$mu))
    parUncon = c(parUncon, 
                 muCon2muUncon(muCon = par[["mus"]],
                               link = (controls[["sdds"]][[1]]$name == "gamma")))
  if(is.null(controls$sdds[[1]]$pars$sigma))
    parUncon = c(parUncon, 
                 sigmaCon2sigmaUncon(par[["sigmas"]]))
  if(controls[["sdds"]][[1]]$name == "t")
    if(is.null(controls$sdds[[1]]$pars$df))
      parUncon = c(parUncon, 
                   dfCon2dfUncon(par[["dfs"]]))
  if(controls[["hierarchy"]]){
    for(s in 1:controls[["states"]][1]){
      parUncon = c(parUncon,
                   Gamma2gammasUncon(par[["Gammas_star"]][[s]]))
      if(is.null(controls$sdds[[2]]$pars$mu))
        parUncon = c(parUncon, 
                     muCon2muUncon(par[["mus_star"]][[s]],
                                   link = (controls[["sdds"]][[2]]$name == "gamma")))
      if(is.null(controls$sdds[[2]]$pars$sigma))
        parUncon = c(parUncon, 
                     sigmaCon2sigmaUncon(par[["sigmas_star"]][[s]]))
      if(controls[["sdds"]][[2]]$name == "t")
        if(is.null(controls$sdds[[2]]$pars$df))
          parUncon = c(parUncon, 
                       dfCon2dfUncon(par[["dfs_star"]][[s]]))
    }
  }
  class(parUncon) = "parUncon"
  return(parUncon)
}

#' This function transforms an object of class \code{parUncon} into an object
#' of class \code{parCon}.
#' @param parUncon
#' An object of class \code{parUncon}.
#' @param controls 
#' An object of class \code{fHMM_controls}.
#' @return
#' An object of class \code{parCon}, i.e. a vector of constrained model 
#' parameters to be estimated.
#' @keywords 
#' internal

parUncon2parCon = function(parUncon, controls){
  stopifnot(class(parUncon) == "parUncon")
  stopifnot(class(controls) == "fHMM_controls")
  M = controls[["states"]][1] 
  parCon = gammasUncon2gammasCon(parUncon[1:((M-1)*M)],M)
  parUncon = parUncon[-(1:((M-1)*M))]
  if(is.null(controls$sdds[[1]]$pars$mu)){
    parCon = c(parCon, 
               muUncon2muCon(parUncon[1:M],
                             link = (controls[["sdds"]][[1]]$name == "gamma")))
    parUncon = parUncon[-(1:M)]
  }
  if(is.null(controls$sdds[[1]]$pars$sigma)){
    parCon = c(parCon, 
               sigmaUncon2sigmaCon(parUncon[1:M]))
    parUncon = parUncon[-(1:M)]
  }
  if(controls[["sdds"]][[1]]$name == "t")
    if(is.null(controls$sdds[[1]]$pars$df)){
      parCon = c(parCon, 
                 dfUncon2dfCon(parUncon[1:M]))
      parUncon = parUncon[-(1:M)]
    }
  if(controls[["hierarchy"]]){
    N = controls[["states"]][2]
    for(s in 1:M){
      parCon = c(parCon, 
                 gammasUncon2gammasCon(parUncon[1:((N-1)*N)],N))
      parUncon = parUncon[-(1:((N-1)*N))]
      if(is.null(controls$sdds[[2]]$pars$mu)){
        parCon = c(parCon, 
                   muUncon2muCon(parUncon[1:N],
                                 link = (controls[["sdds"]][[2]]$name == "gamma")))
        parUncon = parUncon[-(1:N)]
      }
      if(is.null(controls$sdds[[2]]$pars$sigma)){
        parCon = c(parCon, 
                   sigmaUncon2sigmaCon(parUncon[1:N]))
        parUncon = parUncon[-(1:N)]
      }
      if(controls[["sdds"]][[2]]$name == "t")
        if(is.null(controls$sdds[[2]]$pars$df)){
          parCon = c(parCon, 
                     dfUncon2dfCon(parUncon[1:N]))
          parUncon = parUncon[-(1:N)]
        }
    }
  }
  class(parCon) = "parCon"
  return(parCon)
}

#' This function transforms an object of class \code{parCon} into an object
#' of class \code{fHMM_parameters}.
#' @param parCon
#' An object of class \code{parCon}.
#' @param controls 
#' An object of class \code{fHMM_controls}.
#' @return
#' An object of class \code{fHMM_parameters}.
#' @keywords 
#' internal

parCon2par = function(parCon, controls){
  stopifnot(class(parCon) == "parCon")
  stopifnot(class(controls) == "fHMM_controls")
  M = controls[["states"]][1] 
  Gamma = gammasCon2Gamma(parCon[1:((M-1)*M)],M)
  parCon = parCon[-(1:((M-1)*M))]
  if(is.null(controls$sdds[[1]]$pars$mu)){
    mus = parCon[1:M]
    parCon = parCon[-(1:M)]
  } else {
    mus = rep(controls$sdds[[1]]$pars$mu,M)
  }
  if(is.null(controls$sdds[[1]]$pars$sigma)){
    sigmas = parCon[1:M]
    parCon = parCon[-(1:M)]
  } else {
    sigmas = rep(controls$sdds[[1]]$pars$sigma,M)
  }
  if(controls[["sdds"]][[1]]$name == "t"){
    if(is.null(controls$sdds[[1]]$pars$df)){
      dfs = parCon[1:M]
      parCon = parCon[-(1:M)]
    } else {
      dfs = rep(controls$sdds[[1]]$pars$df,M)
    }
  } else {
    dfs = NULL
  }
  if(controls[["hierarchy"]]){
    N = controls[["states"]][2]
    Gammas_star = list()
    mus_star = list()
    sigmas_star = list()
    if(controls[["sdds"]][[2]]$name == "t"){
      dfs_star = list()
    } else {
      dfs_star = NULL
    }
    for(s in 1:M){
      Gammas_star[[s]] = gammasCon2Gamma(parCon[1:((N-1)*N)],N)
      parCon = parCon[-(1:((N-1)*N))]
      if(is.null(controls$sdds[[2]]$pars$mu)){
        mus_star[[s]] = parCon[1:M]
        parCon = parCon[-(1:M)]
      } else {
        mus_star[[s]] = rep(controls$sdds[[2]]$pars$mu,M)
      }
      if(is.null(controls$sdds[[2]]$pars$sigma)){
        sigmas_star[[s]] = parCon[1:M]
        parCon = parCon[-(1:M)]
      } else {
        sigmas_star[[s]] = rep(controls$sdds[[2]]$pars$sigma,M)
      }
      if(controls[["sdds"]][[2]]$name == "t"){
        if(is.null(controls$sdds[[2]]$pars$df)){
          dfs_star[[s]] = parCon[1:M]
          parCon = parCon[-(1:M)]
        } else {
          dfs_star[[s]] = rep(controls$sdds[[2]]$pars$df,M)
        }
      }
    }
  } else {
    Gammas_star = NULL
    mus_star = NULL
    sigmas_star = NULL
    dfs_star = NULL
  }
  par = fHMM_parameters(controls = controls, 
                        Gamma = Gamma, mus = mus, sigmas = sigmas, dfs = dfs, 
                        Gammas_star = Gammas_star, mus_star = mus_star, 
                        sigmas_star = sigmas_star, dfs_star = dfs_star)
  return(par)
}

#' This function transforms an object of class \code{fHMM_parameters} into an 
#' object of class \code{parCon}.
#' @param par
#' An object of class \code{fHMM_parameters}.
#' @param controls 
#' An object of class{fHMM_controls}.
#' @return
#' An object of class \code{parCon}.
#' @keywords 
#' internal

par2parCon = function(par, controls){
  stopifnot(class(par) == "fHMM_parameters")
  stopifnot(class(controls) == "fHMM_controls")
  return(parUncon2parCon(par2parUncon(par,controls),controls))
}

#' This function transforms an object of class \code{parCon} into an 
#' object of class \code{parUncon}.
#' @param parCon
#' An object of class \code{parCon}.
#' @param controls 
#' An object of class \code{fHMM_controls}.
#' @return
#' An object of class \code{parUncon}.
#' @keywords 
#' internal

parCon2parUncon = function(parCon,controls){
  stopifnot(class(parCon) == "parCon")
  stopifnot(class(controls) == "fHMM_controls")
  return(par2parUncon(parCon2par(parCon,controls),controls))
}

#' This function transforms an object of class \code{parUncon} into an 
#' object of class \code{fHMM_parameters}.
#' @param parUncon
#' An object of class \code{parUncon}.
#' @param controls 
#' An object of class \code{fHMM_controls}.
#' @return
#' An object of class \code{fHMM_parameters}.
#' @keywords 
#' internal

parUncon2par = function(parUncon,controls){
  stopifnot(class(parUncon) == "parUncon")
  stopifnot(class(controls) == "fHMM_controls")
  return(parCon2par(parUncon2parCon(parUncon,controls),controls))
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

#' This function un-constrains the constrained expected values \code{muCon}.
#' @param muCon 
#' A vector of constrained expected values.
#' @param link 
#' A boolean, determining whether to apply the link function.
#' @return 
#' A vector of un-constrained expected values.
#' @keywords 
#' internal

muCon2muUncon = function(muCon, link){
  if(link){
    muUncon = log(muCon)
  } else {
    muUncon = muCon
  }
  return(muUncon)
}

#' This function constrains the un-constrained expected values \code{muUncon}.
#' @param muUncon 
#' A vector of un-constrained expected values.
#' @param link 
#' A boolean, determining whether to apply the link function.
#' @return 
#' A vector of constrained expected values.
#' @keywords 
#' internal

muUncon2muCon = function(muUncon, link){
  if(link){ 
    muCon = exp(muUncon)
  } else {
    muCon = muUncon
  }
  return(muCon)
}

#' This function un-constrains the constrained standard deviations \code{sigmaCon}.
#' @param sigmaCon 
#' A vector of constrained standard deviations.
#' @return 
#' A vector of un-constrained standard deviations.
#' @keywords 
#' internal

sigmaCon2sigmaUncon = function(sigmaCon){
  return(log(sigmaCon))
}

#' This function constrains the un-constrained standard deviations \code{sigmaUncon}.
#' @param sigmaUncon 
#' A vector of un-constrained standard deviations.
#' @return 
#' A vector of constrained standard deviations.
#' @keywords 
#' internal

sigmaUncon2sigmaCon = function(sigmaUncon){
  return(exp(sigmaUncon))
}

#' This function un-constrains the constrained degrees of freedom \code{dfCon}.
#' @param dfCon 
#' A vector of constrained degrees of freedom.
#' @return 
#' A vector of un-constrained degrees of freedom.
#' @keywords 
#' internal

dfCon2dfUncon = function(dfCon){
  return(log(dfCon))
}

#' This function constrains the un-constrained degrees of freedom \code{dfUncon}.
#' @param dfUncon 
#' A vector of un-constrained degrees of freedom.
#' @return 
#' A vector of constrained degrees of freedom.
#' @keywords 
#' internal

dfUncon2dfCon = function(dfUncon){
  return(exp(dfUncon))
}

#' This function constrains the non-diagonal matrix elements of a transition 
#' probability matrix \code{Gamma}.
#' @param Gamma 
#' A transition probability matrix.
#' @param shift 
#' A numeric value for shifting boundary probabilities.
#' @return 
#' A vector of constrained non-diagonal matrix elements (column-wise).
#' @keywords 
#' internal

Gamma2gammasCon = function(Gamma, shift = 1e-3){
  gammasCon = Gamma[row(Gamma)!=col(Gamma)] 
  gammasCon = replace(gammasCon, gammasCon == 0, shift)
  gammasCon = replace(gammasCon, gammasCon == 1, 1-shift)
  return(gammasCon)
}

#' This function un-constraines the non-diagonal matrix elements of a transition 
#' probability matrix \code{Gamma}.
#' @inheritParams Gamma2gammasCon
#' @return 
#' A vector of un-constrained non-diagonal matrix elements (column-wise).
#' @keywords 
#' internal

Gamma2gammasUncon = function(Gamma){
  diag(Gamma) = 0
  Gamma = log(Gamma/(1-rowSums(Gamma)))
  diag(Gamma) = NA
  return(Gamma[!is.na(Gamma)])
}

#' This function builds a transition probability matrix of dimension \code{dim}
#' from constrained non-diagonal elements \code{gammasCon}.
#' @param gammasCon 
#' A vector of constrained non-diagonal elements of a transition probability 
#' matrix.
#' @param dim 
#' The dimension of the transition probability matrix.
#' @return 
#' A transition probability matrix.
#' @keywords 
#' internal

gammasCon2Gamma = function(gammasCon, dim){
  Gamma = diag(dim)
  Gamma[!Gamma] = gammasCon 
  for(i in 1:dim){
    Gamma[i,i] = 1-(rowSums(Gamma)[i]-1)
  }
  return(Gamma)
}

#' This function un-constrains the constrained non-diagonal elements 
#' \code{gammasCon} of a transition probability matrix of dimension \code{dim}.
#' @inheritParams gammasCon2Gamma
#' @return 
#' A vector of un-constrained non-diagonal elements of the transition probability 
#' matrix.
#' @keywords 
#' internal

gammasCon2gammasUncon = function(gammasCon,dim){
  gammasUncon = Gamma2gammasUncon(gammasCon2Gamma(gammasCon,dim))
  return(gammasUncon)
}

#' This function builds a transition probability matrix from un-constrained 
#' non-diagonal elements \code{gammasUncon}.
#' @param gammasUncon 
#' A vector of un-constrained non-diagonal elements of a transition probability 
#' matrix.
#' @inheritParams gammasCon2Gamma
#' @return 
#' A transition probability matrix.
#' @keywords 
#' internal

gammasUncon2Gamma = function(gammasUncon, dim){
  Gamma = diag(dim)
  Gamma[!Gamma] = exp(gammasUncon)
  Gamma = Gamma/rowSums(Gamma)
  return(Gamma)
}

#' This function constrains non-diagonal elements \code{gammasUncon} of a 
#' transition probability matrix.
#' @param gammasUncon 
#' A vector of un-constrained non-diagonal elements of a transition probability 
#' matrix.
#' @inheritParams gammasUncon2Gamma
#' @return 
#' A vector of constrained non-diagonal elements of a transition probability 
#' matrix.
#' @keywords 
#' internal

gammasUncon2gammasCon = function(gammasUncon, dim){
  gammasCon = Gamma2gammasCon(gammasUncon2Gamma(gammasUncon,dim))
  return(gammasCon)
}

#' This function computes the stationary distribution of a transition 
#' probability matrix \code{Gamma}.
#' @param Gamma 
#' A transition probability matrix.
#' @return 
#' A stationary distribution vector.
#' @details
#' If the stationary distribution vector cannot be computed, it is set to the
#' discrete uniform distribution over the states.
#' @keywords 
#' internal

Gamma2delta = function(Gamma){
  dim = dim(Gamma)[1]
  delta_try = try(solve(t(diag(dim) - Gamma + 1), rep(1,dim)), silent=TRUE)
  if(class(delta_try) == "try-error"){ 
    delta = rep(1/dim, dim)
    warning("F.1")
  } else { 
    delta = delta_try
  }
  return(delta)
}
