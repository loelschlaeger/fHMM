### maximize log-likelihood function
max_likelihood = function(data,controls){
  if(is.null(controls[["controls_checked"]])) stop("'controls' invalid",call.=FALSE)
  if(!is.null(controls[["seed"]])) set.seed(controls[["seed"]])
  
  runs = controls[["runs"]]
  llks = rep(NA,runs) 
  mods = list() 
  
  ### define optimizer
  if(controls[["model"]]=="HMM") target = nLL_hmm
  if(controls[["model"]]=="HHMM") target = nLL_hhmm
  optimized = function(start_values){
    nlm_out = nlm(f = target,
                  p = start_values,
                  observations = data[["logReturns"]],
                  controls = controls,
                  iterlim = controls[["iterlim"]],
                  steptol = controls[["steptol"]],
                  gradtol = controls[["gradtol"]],
                  print.level = controls[["print_level"]],
                  typsize = start_values,
                  hessian = FALSE)
    return(nlm_out)
  }
  
  ### generate start values
  start_values = list()
  if(controls[["at_true"]]){
    start_values[[1]] = data[["thetaUncon0"]]
  }
  if(!controls[["at_true"]]){
    for(run in seq_len(runs)){
      start_values[[run]] = init_est(controls)
    }
  }
  
  ### check if log-likelihoods at start values can be computed
  no_failes = function(llks){
    total = length(llks)
    failes = length(llks[is.nan(llks)||is.na(llks)||abs(llks)>1e100])
    return(failes/total)
  }
  ll_at_start_values = numeric(runs)
  for(run in seq_len(runs)){
    ll_at_start_values[run] = suppressWarnings(target(start_values[[run]],data[["logReturns"]],controls))
    if(no_failes(ll_at_start_values)>0.5){
      stop("Bad parameter scaling. (Code 10)",call.=FALSE)
    }
  }
  
  ### define progress-bar
  pb = progress::progress_bar$new(
    format = "MLE: [:bar] :percent complete, :eta ETA", 
    total = runs, 
    clear = TRUE, 
    width = 60, 
    show_after = 0
    )
  
  ### start maximization
  start = Sys.time()
	for (run in seq_len(runs)){
	  pb$tick(0)
	  suppressWarnings({ tryCatch({ mods[[run]] = optimized(start_values[[run]])
    		                          if(mods[[run]][["code"]] %in% controls[["accept_codes"]] || controls[["at_true"]]){
    		                            llks[run] = -mods[[run]]$minimum
    		                          }
    		                         },error = function(e){})
  		                })
	  pb$tick()
	}
  end = Sys.time()
  estimation_time = ceiling(difftime(end,start,units='mins'))
  
  if(all(is.na(llks))){
    stop("None of the estimation runs ended successfully.",call.=FALSE)
  } else {
    ### compute Hessian
    message("Computing the Hessian...",appendLF = FALSE)
    hessian = suppressWarnings(nlm(f = target,
                                   p = mods[[which.max(llks)]][["estimate"]],
                                   observations = data[["logReturns"]],
                                   controls = controls,
                                   iterlim = 1,
                                   hessian = TRUE,
                                   typsize = mods[[which.max(llks)]][["estimate"]])[["hessian"]])
	  message("\r",sprintf("Hessian computed. %10s"," "))
    fit = check_estimation(estimation_time,mods,llks,data,hessian,controls)
	  return(fit)
  }
}

### initialize the estimation routine randomly
init_est = function(controls){
  M  = controls[["states"]][1] 
  N  = controls[["states"]][2]
  scale_par = controls[["scale_par"]]
  
  build_gammasUncon = function(dim){
    Gamma = diag(dim)
    diag(Gamma) = runif(dim,0.8,1)
    for(d in seq_len(dim)){
      Gamma[d,-d] = runif(dim-1,0,0.2)
    }
    return(Gamma2gammasUncon(Gamma))
  }
  
  if(controls[["model"]]=="HMM"){
    gammasUncon = build_gammasUncon(M)
    sigmasUncon = sigmaCon2sigmaUncon((seq(0,1,length.out=M)+runif(1))*scale_par[1])
    if(controls[["sdds"]][1] == "t"){
      musUncon    = muCon2muUncon((seq(1,-1,length.out=M)+runif(1))*scale_par[1],link=FALSE)
      dfs         = if(is.na(controls[["fixed_dfs"]][1])) runif(M,1,30) else integer(0)
      thetaUncon  = c(gammasUncon,musUncon,sigmasUncon,dfs)
    }
    if(controls[["sdds"]][1] == "gamma"){
      musUncon    = muCon2muUncon((seq(0,1,length.out=M)+runif(1))*scale_par[1],link=TRUE)
      thetaUncon  = c(gammasUncon,musUncon,sigmasUncon)
    }
  }
  
  if(controls[["model"]]=="HHMM"){
    gammasUncon = build_gammasUncon(M)
    sigmasUncon = sigmaCon2sigmaUncon((seq(0,1,length.out=M)+runif(1))*scale_par[1])
    if(controls[["sdds"]][1] == "t"){
      musUncon = muCon2muUncon((seq(1,-1,length.out=M)+runif(1))*scale_par[1],link=FALSE)
      dfs      = if(is.na(controls[["fixed_dfs"]][1])) runif(M,1,30) else integer(0)
      if(controls[["sdds"]][2] == "t"){
        gammasUncon_star = c()
        musUncon_star    = c()
        sigmasUncon_star = c()
        dfs_star         = c()
        for(m in seq_len(M)){
          gammasUncon_star = c(gammasUncon_star,build_gammasUncon(N))
          musUncon_star    = c(musUncon_star,sort(muCon2muUncon(muUncon2muCon(musUncon[m],link=FALSE)/scale_par[1]*scale_par[2]*runif(N,0.5,1.5),link=FALSE),decreasing=TRUE))
          sigmasUncon_star = c(sigmasUncon_star,sort(sigmaCon2sigmaUncon(sigmaUncon2sigmaCon(sigmasUncon[m])/scale_par[1]*scale_par[2]*runif(N,0.5,1.5)),decreasing=FALSE))
          dfs_star         = c(dfs_star,if(is.na(controls[["fixed_dfs"]][2])) runif(N,1,30) else integer(0))
        }
        gammasUncon = c(gammasUncon,gammasUncon_star)
        musUncon    = c(musUncon,musUncon_star)
        sigmasUncon = c(sigmasUncon,sigmasUncon_star)
        dfs         = c(dfs,dfs_star)
        thetaUncon  = c(gammasUncon,musUncon,sigmasUncon,dfs)
      }
      if(controls[["sdds"]][2] == "gamma"){
        gammasUncon_star = c()
        musUncon_star    = c()
        sigmasUncon_star = c()
        for(m in seq_len(M)){
          gammasUncon_star = c(gammasUncon_star,build_gammasUncon(N))
          musUncon_star    = c(musUncon_star,sort(muCon2muUncon(abs(muUncon2muCon(musUncon[m],link=FALSE)/scale_par[1]*scale_par[2]*runif(N,0.5,1.5)),link=TRUE),decreasing=TRUE))
          sigmasUncon_star = c(sigmasUncon_star,sort(sigmaCon2sigmaUncon(sigmaUncon2sigmaCon(sigmasUncon[m])/scale_par[1]*scale_par[2]*runif(N,0.5,1.5)),decreasing=FALSE))
        }
        gammasUncon = c(gammasUncon,gammasUncon_star)
        musUncon    = c(musUncon,musUncon_star)
        sigmasUncon = c(sigmasUncon,sigmasUncon_star)
        thetaUncon  = c(gammasUncon,musUncon,sigmasUncon,dfs)
      }
    }
    if(controls[["sdds"]][1] == "gamma"){
      musUncon = muCon2muUncon((seq(0,1,length.out=M)+runif(1))*scale_par[1],link=FALSE)
      if(controls[["sdds"]][2] == "t"){
        gammasUncon_star = c()
        musUncon_star    = c()
        sigmasUncon_star = c()
        dfs_star         = c()
        for(m in seq_len(M)){
          gammasUncon_star = c(gammasUncon_star,build_gammasUncon(N))
          musUncon_star    = c(musUncon_star,sort(muCon2muUncon(muUncon2muCon(musUncon[m]-mean(musUncon),link=FALSE)/scale_par[1]*scale_par[2]*runif(N,0.5,1.5),link=FALSE),decreasing=TRUE))
          sigmasUncon_star = c(sigmasUncon_star,sort(sigmaCon2sigmaUncon(sigmaUncon2sigmaCon(sigmasUncon[m])/scale_par[1]*scale_par[2]*runif(N,0.5,1.5)),decreasing=FALSE))
          dfs_star         = c(dfs_star,if(is.na(controls[["fixed_dfs"]][2])) runif(N,1,30) else integer(0))
        }
        gammasUncon = c(gammasUncon,gammasUncon_star)
        musUncon    = c(musUncon,musUncon_star)
        sigmasUncon = c(sigmasUncon,sigmasUncon_star)
        dfs         = dfs_star
        thetaUncon  = c(gammasUncon,musUncon,sigmasUncon,dfs)
      }
      if(controls[["sdds"]][2] == "gamma"){
        gammasUncon_star = c()
        musUncon_star    = c()
        sigmasUncon_star = c()
        for(m in seq_len(M)){
          gammasUncon_star = c(gammasUncon_star,build_gammasUncon(N))
          musUncon_star    = c(musUncon_star,sort(muCon2muUncon(muUncon2muCon(musUncon[m],link=FALSE)/scale_par[1]*scale_par[2]*runif(N,0.5,1.5),link=FALSE),decreasing=TRUE))
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

### INPUT:  unconstrained parameter vector, observations, states 
### OUTPUT: negative log-likelihood of HMM
nLL_hmm = function(thetaUncon,observations,controls){
  T = length(observations)
  if(controls[["model"]]=="HMM"){
    nstates   = controls[["states"]][1]
    thetaList = thetaUncon2thetaList(thetaUncon,controls)
    sdd       = controls[["sdds"]][1]
  }
  if(controls[["model"]]=="HHMM"){
    nstates   = controls[["states"]][2]
    thetaList = thetaUnconSplit2thetaList(thetaUncon,controls)
    sdd       = controls[["sdds"]][2]
  }
  
  Gamma  = thetaList[["Gamma"]]
  delta  = Gamma2delta(Gamma)
  mus    = thetaList[["mus"]]
  sigmas = thetaList[["sigmas"]]
  dfs    = thetaList[["dfs"]]
  
  allprobs = matrix(0,nstates,T)
  for (i in 1:nstates){
    if(sdd=="t"){
      allprobs[i,] = 1/sigmas[i]*dt((observations-mus[i])/sigmas[i],dfs[i])
    }
    if(sdd=="gamma"){
      allprobs[i,] = dgamma(observations,shape=mus[i]^2/sigmas[i]^2,scale=sigmas[i]^2/mus[i])
    }
  }
  
  nLL = -LL_HMM_Rcpp(allprobs,Gamma,delta,nstates,T) 
  return(nLL)
}

### INPUT:  unconstrained parameter vector, observations, control parameters
### OUTPUT: negative log-likelihood of HHMM
nLL_hhmm = function(thetaUncon,observations,controls){
  M  = controls[["states"]][1] 
  N  = controls[["states"]][2]
  
  observations_cs = observations[,1]
  observations_fs = observations[,-1]
  T = length(observations_cs)
  
  thetaList = thetaUncon2thetaList(thetaUncon,controls)
  
  Gamma  = thetaList[["Gamma"]]
  delta  = Gamma2delta(Gamma)
  mus    = thetaList[["mus"]]
  sigmas = thetaList[["sigmas"]]
  dfs    = thetaList[["dfs"]]
  
  allprobs = matrix(0,M,T)
  log_likelihoods = matrix(0,M,T)
  thetaUnconSplit = thetaUncon2thetaUnconSplit(thetaUncon,controls)
  
  for (m in seq_len(M)){
    if(controls[["sdds"]][1]=="t"){
      allprobs[m,] = 1/sigmas[m]*dt((observations_cs-mus[m])/sigmas[m],dfs[m])
    }
    if(controls[["sdds"]][1]=="gamma"){
      allprobs[m,] = dgamma(observations_cs,shape=mus[m]^2/sigmas[m]^2,scale=sigmas[m]^2/mus[m])
    }
    for(t in seq_len(T)){
      log_likelihoods[m,t] = -nLL_hmm(thetaUnconSplit[[m]],observations[t,-1][!is.na(observations[t,-1])],controls)
    }
  }
  
  nLL = -LL_HHMM_Rcpp(log_likelihoods=log_likelihoods,allprobs=allprobs,Gamma=Gamma,delta=delta,M=M,T=T) 
  
  return(nLL)
}