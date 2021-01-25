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
                  print.level = controls[["print_level"]],
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
  
  ### check if log-likelihood at true values can be computed
  if(controls[["at_true"]]){
    if(is.nan(target(start_values[[1]], data[["logReturns"]], controls))){
      stop("Log-likelihood is 'NaN' at the true parameters.",call.=FALSE)
    }
  }
  
  ### define progress-bar
  pb = progress_bar$new(
    format = "Model fitting: [:bar] :percent complete, :eta ETA", 
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
    		                          if(mods[[run]]$code %in% controls[["accept_codes"]] || controls[["at_true"]]){
    		                            llks[run] = -mods[[run]]$minimum
    		                          }
    		                         },error = function(e){})
  		                })
	  pb$tick()
	}
  end = Sys.time()
  estimation_time = ceiling(difftime(end,start,units='mins'))
  
  if(all(is.na(llks))){
    stop("None of the estimation runs ended successfully. Consider increasing 'runs' in 'controls'.",call.=FALSE)
  } else {
    ### compute Hessian
    message("Computing the Hessian...",appendLF = FALSE)
    hessian = suppressWarnings(nlm(f=target,p=mods[[which.max(llks)]][["estimate"]],observations=data[["logReturns"]],controls=controls,iterlim=1,hessian=TRUE)[["hessian"]])
	  message("\r",sprintf("Hessian computed. %10s"," "))
    fit = check_estimation(estimation_time,mods,llks,data,hessian,controls)
	  return(fit)
  }
}

### initialize the estimation routine randomly
init_est = function(controls){
  M  = controls[["states"]][1] 
  N  = controls[["states"]][2] 
  df_cs = controls[["fixed_dfs"]][1]
  df_fs = controls[["fixed_dfs"]][2]
  
  gammasUncon = gammasCon2gammasUncon(runif((M-1)*M,0,M/10),M)
  mus         = (seq(1,-1,length.out=M)+runif(1))*10^(-2)
  sigmasUncon = log((seq(0,1,length.out=M)+runif(1))*10^(-2.5))
  dfs         = if(is.na(df_cs)) runif(M,1,30) else integer(0)
  if(controls[["model"]]=="HHMM"){
    gammasUncon_star = c()
    mus_star         = c()
    sigmasUncon_star = c()
    dfs_star    = c()
    for(m in seq_len(M)){
      gammasUncon_star = c(gammasUncon_star,gammasCon2gammasUncon(runif((N-1)*N,0,N/10),N))
      mus_star         = c(mus_star,sort(mus[m]*runif(N,0,10),decreasing=TRUE))
      sigmasUncon_star = c(sigmasUncon_star,sort(sigmasUncon[m]+log(runif(N,0,10)),decreasing=FALSE))
      dfs_star         = c(dfs_star,if(is.na(df_fs)) runif(N,1,30) else integer(0))
    }
    gammasUncon      = c(gammasUncon,gammasUncon_star)
    mus              = c(mus,mus_star)
    sigmasUncon      = c(sigmasUncon,sigmasUncon_star)
    dfs              = c(dfs,dfs_star)
  }
  thetaUncon = c(gammasUncon,mus,sigmasUncon,dfs)
  return(thetaUncon)
}

### INPUT:  unconstrained parameter vector, observations, states 
### OUTPUT: negative log-likelihood of HMM
nLL_hmm = function(thetaUncon,observations,controls){
  if(controls[["model"]]=="HMM"){
    nstates = controls[["states"]][1]
    thetaList = thetaUncon2thetaList(thetaUncon,controls)
  }
  if(controls[["model"]]=="HHMM"){
    nstates = controls[["states"]][2]
    thetaList = thetaUnconSplit2thetaList(thetaUncon,controls)
  }
  T = length(observations)
  
  Gamma = thetaList[["Gamma"]]
  delta = Gamma2delta(Gamma)
  mus = thetaList[["mus"]]
  sigmas = thetaList[["sigmas"]]
  dfs = thetaList[["dfs"]]
  
  allprobs = matrix(0,nstates,T)
  for (i in 1:nstates){
    allprobs[i,] = 1/sigmas[i]*dt((observations-mus[i])/sigmas[i],dfs[i])
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
  
  Gamma = thetaList[["Gamma"]]
  delta = Gamma2delta(Gamma)
  mus = thetaList[["mus"]]
  sigmas = thetaList[["sigmas"]]
  dfs = thetaList[["dfs"]]
  
  allprobs = matrix(0,M,T)
  log_likelihoods = matrix(0,M,T)
  thetaUnconSplit = thetaUncon2thetaUnconSplit(thetaUncon,controls)
  
  for (m in seq_len(M)){
    allprobs[m,] = 1/sigmas[m]*dt((observations_cs-mus[m])/sigmas[m],dfs[m])
    for(t in seq_len(T)){
      log_likelihoods[m,t] = -nLL_hmm(thetaUnconSplit[[m]],observations[t,-1][!is.na(observations[t,-1])],controls)
    }
  }
  
  nLL = -LL_HHMM_Rcpp(log_likelihoods=log_likelihoods,allprobs=allprobs,Gamma=Gamma,delta=delta,M=M,T=T) 
  
  return(nLL)
}