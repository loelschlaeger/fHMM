maxLikelihood = function(data,controls){
  if(is.null(controls[["controls_checked"]])) stop("'controls' invalid",call.=FALSE)
  
  runs = controls[["runs"]]
  llks = rep(NA,runs) 
  mods = list() 
  
  if(controls[["model"]]=="HMM") target = nLL_hmm
  if(controls[["model"]]=="HHMM") target = nLL_hhmm
  if(!is.null(controls[["seed"]])) set.seed(controls[["seed"]])
  
  pb = progress_bar$new(format = "Model fitting: [:bar] :percent complete, :eta ETA", total = runs, clear = FALSE, width = 60, show_after = 0)
  start = Sys.time()
	for (k in 1:runs){
	  pb$tick(0)
	  suppressWarnings({ tryCatch({ mods[[k]] = nlm(f = target,
    			                                        p = init_est(controls),
                            			                observations = data[["observations"]],
                            			                controls = controls,
                            			                iterlim = controls[["iterlim"]],
                            			                steptol = controls[["steptol"]],
                            			                print.level = controls[["print.level"]],
                            			                hessian = controls[["hessian"]])
    		                          if(mods[[k]]$code %in% controls[["accept_codes"]]) llks[k] = -mods[[k]]$minimum
    		                          },error = function(e){})
  		              })
	  pb$tick()
	}
  end = Sys.time()
  
	fit = check_estimation(round(difftime(end,start,units='mins')),mods,llks,data,controls)
	
	return(fit)
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
  
  LL = LogLikeHMM_Rcpp(allprobs,Gamma,delta,nstates,T) 
  return(-LL)
}

### INPUT:  unconstrained parameter vector, observations, control parameters
### OUTPUT: negative log-likelihood of HHMM
nLL_hhmm = function(thetaUncon,observations,controls){
  M  = controls[["states"]][1] #coarse-scale states
  N  = controls[["states"]][2] #fine-scale states
  
  observations_fs = observations[,-1]
  observations_cs = observations[,1]
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
      log_likelihoods[m,t] = -nLL_hmm(thetaUnconSplit[[m]],observations[t,-1],controls)
    }
  }
  
  nloglike = -LogLikeHHMM_Rcpp(log_likelihoods=log_likelihoods,allprobs=allprobs,Gamma=Gamma,delta=delta,M=M,T=T) 
  
  return(nloglike)
}