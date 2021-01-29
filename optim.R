### maximize log-likelihood function
max_likelihood = function(data,controls){
  if(is.null(controls[["controls_checked"]])) stop("'controls' is invalid (Code C.1)",call.=FALSE)
  if(!is.null(controls[["seed"]])) set.seed(controls[["seed"]])
  
  runs = controls[["runs"]]
  llks = rep(NA,runs) 
  mods = list() 
  
  ### define optimizer
  if(controls[["model"]]=="HMM") target = nLL_hmm
  if(controls[["model"]]=="HHMM") target = nLL_hhmm
  optimized = function(start_value){
    nlm_out = nlm(f = target,
                  p = start_value,
                  observations = data[["logReturns"]],
                  controls = controls,
                  iterlim = controls[["iterlim"]],
                  steptol = controls[["steptol"]],
                  gradtol = controls[["gradtol"]],
                  print.level = controls[["print_level"]],
                  typsize = start_value,
                  hessian = FALSE)
    return(nlm_out)
  }
  
  ### generate start values
  generate_start_values = function(controls,runs){
    start_values = list()
    if(controls[["at_true"]]){
      start_values[[1]] = data[["thetaUncon0"]]
    }
    if(!controls[["at_true"]]){
      for(run in seq_len(runs)){
        start_values[[run]] = init_est(controls)
      }
    }
    return(start_values)
  }
  
  ### check if log-likelihoods at start values can be computed 
  failed_start_values = function(values){
    failed = union(which(is.nan(values)),which(is.na(values)))
    failed = c(failed,which(abs(values[-failed])>1e100))
    return(failed)
  }
  
  ### adjust 'scale_par' based on method of moments estimates
  adjust_scale_par = function(controls,data){
    scale_par = c(NA,NA)
    if(controls[["model"]]=="HMM"){
      scale_par[1] = mean(c(mean(data,na.rm="TRUE"),sd(data,na.rm="TRUE")))
    }
    if(controls[["model"]]=="HHMM"){
      scale_par[1] = mean(c(mean(data[,1],na.rm="TRUE"),sd(data[,1],na.rm="TRUE")))
      scale_par[2] = mean(c(mean(data[,-1],na.rm="TRUE"),sd(data[,-1],na.rm="TRUE")))
    }
    return(scale_par)
  }
  
  message("Selecting start values...",appendLF = FALSE)
  controls[["scale_par"]] = adjust_scale_par(controls,data[["logReturns"]])
  start_values = generate_start_values(controls,runs)
  ll_at_start_values = rep(NA,runs)
  for(run in seq_len(runs)){
    ll_at_start_values[run] = suppressWarnings(target(start_values[[run]],data[["logReturns"]],controls))
  }
  message("\r",sprintf("Start values selected. %10s"," "))
  fails = failed_start_values(ll_at_start_values)
  if(length(fails)/runs>0.5){
    stop("... (Code F.*)",call.=FALSE)
  }
  if(length(fails)>0){
    runs_seq = seq_len(runs)[-fails]
  } else {
    runs_seq = seq_len(runs)
  }
  
  ### define progress-bar
  pb = progress::progress_bar$new(
    format = "Estimation: [:bar] :percent, :eta ETA", 
    total = length(runs_seq), 
    clear = TRUE, 
    width = 45, 
    show_after = 0
    )
  
  ### start maximization
  start = Sys.time()
	for (run in runs_seq){
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
      dfs         = if(is.na(controls[["fixed_dfs"]][1])) sample.int(30,M) else integer(0)
      thetaUncon  = c(gammasUncon,musUncon,sigmasUncon,dfs)
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
      dfs      = if(is.na(controls[["fixed_dfs"]][1])) sample.int(30,M) else integer(0)
      if(controls[["sdds"]][2] == "t"){
        gammasUncon_star = c()
        musUncon_star    = c()
        sigmasUncon_star = c()
        dfs_star         = c()
        for(m in seq_len(M)){
          gammasUncon_star = c(gammasUncon_star,Gamma2gammasUncon(build_Gamma(N)))
          musUncon_star    = c(musUncon_star,sort(muCon2muUncon(muUncon2muCon(musUncon[m],link=FALSE)/scale_par[1]*scale_par[2]*runif(N,0.5,1.5),link=FALSE),decreasing=TRUE))
          sigmasUncon_star = c(sigmasUncon_star,sort(sigmaCon2sigmaUncon(sigmaUncon2sigmaCon(sigmasUncon[m])/scale_par[1]*scale_par[2]*runif(N,0.5,1.5)),decreasing=FALSE))
          dfs_star         = c(dfs_star,if(is.na(controls[["fixed_dfs"]][2])) sample.int(30,N) else integer(0))
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
          gammasUncon_star = c(gammasUncon_star,Gamma2gammasUncon(build_Gamma(N)))
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
      musUncon = muCon2muUncon((seq(0.1,1,length.out=M)+runif(1))*scale_par[1],link=TRUE)
      if(controls[["sdds"]][2] == "t"){
        gammasUncon_star = c()
        musUncon_star    = c()
        sigmasUncon_star = c()
        dfs_star         = c()
        for(m in seq_len(M)){
          gammasUncon_star = c(gammasUncon_star,Gamma2gammasUncon(build_Gamma(N)))
          musUncon_star    = c(musUncon_star,sort(muCon2muUncon(muUncon2muCon(musUncon[m]-mean(musUncon),link=FALSE)/scale_par[1]*scale_par[2]*runif(N,0.5,1.5),link=FALSE),decreasing=TRUE))
          sigmasUncon_star = c(sigmasUncon_star,sort(sigmaCon2sigmaUncon(sigmaUncon2sigmaCon(sigmasUncon[m])/scale_par[1]*scale_par[2]*runif(N,0.5,1.5)),decreasing=FALSE))
          dfs_star         = c(dfs_star,if(is.na(controls[["fixed_dfs"]][2])) sample.int(30,N) else integer(0))
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
  
  allprobs = matrix(NA,nstates,T)
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
      log_likelihoods[m,t] = -nLL_hmm(thetaUnconSplit[[m]],observations_fs[t,][!is.na(observations_fs[t,])],controls)
    }
  }
  
  nLL = -LL_HHMM_Rcpp(log_likelihoods=log_likelihoods,allprobs=allprobs,Gamma=Gamma,delta=delta,M=M,T=T) 
  
  return(nLL)
}