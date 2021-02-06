#' Draw initial parameter values for the estimation routine
#'
#' @param controls A list of controls
#'
#' @return Parameters values in format \code{thetaUncon}

init_est = function(controls){
  M  = controls[["states"]][1] 
  N  = controls[["states"]][2]
  scale_par = controls[["scale_par"]]
  
  build_Gamma = function(dim){
    Gamma = diag(dim)
    diag(Gamma) = runif(dim,0.7,1)
    for(d in seq_len(dim)){
      weights = sample.int(dim-1)
      Gamma[d,-d] = (1-Gamma[d,d])*weights/sum(weights)
    }
    return(Gamma)
  }
  
  if(controls[["model"]]=="HMM"){
    gammasUncon = Gamma2gammasUncon(build_Gamma(M))
    sigmasUncon = sigmaCon2sigmaUncon((seq(0.1,1,length.out=M)*runif(1))*scale_par[1])
    if(controls[["sdds"]][1] == "t"){
      musUncon    = muCon2muUncon((seq(1.1,-0.9,length.out=M)*runif(1))*scale_par[1],link=FALSE)
      dfsUncon    = if(is.na(controls[["fixed_dfs"]][1])) log(sample.int(30,M)) else integer(0)
      thetaUncon  = c(gammasUncon,musUncon,sigmasUncon,dfsUncon)
    }
    if(controls[["sdds"]][1] == "gamma"){
      musUncon    = muCon2muUncon((seq(0.1,1,length.out=M)*runif(1))*scale_par[1],link=TRUE)
      thetaUncon  = c(gammasUncon,musUncon,sigmasUncon)
    }
  }
  
  if(controls[["model"]]=="HHMM"){
    gammasUncon = Gamma2gammasUncon(build_Gamma(M))
    sigmasUncon = sigmaCon2sigmaUncon((seq(0.1,1,length.out=M)*runif(1))*scale_par[1])
    if(controls[["sdds"]][1] == "t"){
      musUncon = muCon2muUncon((seq(1.1,-0.9,length.out=M)*runif(1))*scale_par[1],link=FALSE)
      dfsUncon = if(is.na(controls[["fixed_dfs"]][1])) log(sample.int(30,M)) else integer(0)
      if(controls[["sdds"]][2] == "t"){
        gammasUncon_star = c()
        musUncon_star    = c()
        sigmasUncon_star = c()
        dfsUncon_star    = c()
        for(m in seq_len(M)){
          gammasUncon_star = c(gammasUncon_star,Gamma2gammasUncon(build_Gamma(N)))
          musUncon_star    = c(musUncon_star,sort(muCon2muUncon(muUncon2muCon(musUncon[m],link=FALSE)/scale_par[1]*scale_par[2]*runif(N,0.5,1.5),link=FALSE),decreasing=TRUE))
          sigmasUncon_star = c(sigmasUncon_star,sort(sigmaCon2sigmaUncon(sigmaUncon2sigmaCon(sigmasUncon[m])/scale_par[1]*scale_par[2]*runif(N,0.5,1.5)),decreasing=FALSE))
          dfsUncon_star    = c(dfsUncon_star,if(is.na(controls[["fixed_dfs"]][2])) log(sample.int(30,N)) else integer(0))
        }
        gammasUncon = c(gammasUncon,gammasUncon_star)
        musUncon    = c(musUncon,musUncon_star)
        sigmasUncon = c(sigmasUncon,sigmasUncon_star)
        dfsUncon    = c(dfsUncon,dfsUncon_star)
        thetaUncon  = c(gammasUncon,musUncon,sigmasUncon,dfsUncon)
      }
      if(controls[["sdds"]][2] == "gamma"){
        gammasUncon_star = c()
        musUncon_star    = c()
        sigmasUncon_star = c()
        for(m in seq_len(M)){
          gammasUncon_star = c(gammasUncon_star,Gamma2gammasUncon(build_Gamma(N)))
          musUncon_star    = c(musUncon_star,sort(muCon2muUncon(abs(muUncon2muCon(musUncon[m],link=FALSE)/scale_par[1]*scale_par[2]*runif(N,0.5,1.5)),link=TRUE),decreasing=TRUE))
          sigmasUncon_star = c(sigmasUncon_star,sort(sigmaCon2sigmaUncon(sigmaUncon2sigmaCon(sigmasUncon[m])/scale_par[1]*scale_par[2]*runif(N,0.5,1.5)),decreasing=FALSE))
        }
        gammasUncon = c(gammasUncon,gammasUncon_star)
        musUncon    = c(musUncon,musUncon_star)
        sigmasUncon = c(sigmasUncon,sigmasUncon_star)
        thetaUncon  = c(gammasUncon,musUncon,sigmasUncon,dfsUncon)
      }
    }
    if(controls[["sdds"]][1] == "gamma"){
      musUncon = muCon2muUncon((seq(0.1,1,length.out=M)+runif(1))*scale_par[1],link=TRUE)
      if(controls[["sdds"]][2] == "t"){
        gammasUncon_star = c()
        musUncon_star    = c()
        sigmasUncon_star = c()
        dfsUncon_star   = c()
        for(m in seq_len(M)){
          gammasUncon_star = c(gammasUncon_star,Gamma2gammasUncon(build_Gamma(N)))
          musUncon_star    = c(musUncon_star,sort(muCon2muUncon(muUncon2muCon(musUncon[m]-mean(musUncon),link=FALSE)/scale_par[1]*scale_par[2]*runif(N,0.5,1.5),link=FALSE),decreasing=TRUE))
          sigmasUncon_star = c(sigmasUncon_star,sort(sigmaCon2sigmaUncon(sigmaUncon2sigmaCon(sigmasUncon[m])/scale_par[1]*scale_par[2]*runif(N,0.5,1.5)),decreasing=FALSE))
          dfsUncon_star    = c(dfsUncon_star,if(is.na(controls[["fixed_dfs"]][2])) log(sample.int(30,N)) else integer(0))
        }
        gammasUncon = c(gammasUncon,gammasUncon_star)
        musUncon    = c(musUncon,musUncon_star)
        sigmasUncon = c(sigmasUncon,sigmasUncon_star)
        dfsUncon    = dfsUncon_star
        thetaUncon  = c(gammasUncon,musUncon,sigmasUncon,dfsUncon)
      }
      if(controls[["sdds"]][2] == "gamma"){
        gammasUncon_star = c()
        musUncon_star    = c()
        sigmasUncon_star = c()
        for(m in seq_len(M)){
          gammasUncon_star = c(gammasUncon_star,Gamma2gammasUncon(build_Gamma(N)))
          musUncon_star    = c(musUncon_star,sort(muCon2muUncon(muUncon2muCon(musUncon[m],link=TRUE)/scale_par[1]*scale_par[2]*runif(N,0.5,1.5),link=TRUE),decreasing=TRUE))
          sigmasUncon_star = c(sigmasUncon_star,sort(sigmaCon2sigmaUncon(sigmaUncon2sigmaCon(sigmasUncon[m])/scale_par[1]*scale_par[2]*runif(N,0.5,1.5)),decreasing=FALSE))
        }
        gammasUncon = c(gammasUncon,gammasUncon_star)
        musUncon    = c(musUncon,musUncon_star)
        sigmasUncon = c(sigmasUncon,sigmasUncon_star)
        thetaUncon  = c(gammasUncon,musUncon,sigmasUncon)
      }
    }
  }
  
  return(thetaUncon)
}
