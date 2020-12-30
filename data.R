getData = function(controls){
  if(is.null(controls[["controls_checked"]])) stop("'controls' invalid",call.=FALSE)
  if(controls[["sim"]]){
    data = simulateData(controls)
    writeLines("Data simulation successful.")
  }
  if(!controls[["sim"]]){
    data = readData(controls)
    writeLines("Data processing successful.")
  }
  check_data(controls,data)
  return(data)
}

### simulate data
simulateData = function(controls){
  if(!is.null(controls[["seed"]])) set.seed(controls[["seed"]])
  M = controls[["states"]][1] #coarse-scale states
  N = controls[["states"]][2] #fine-scale states
  if(!controls[["est_dfs"]]){
    df_cs = controls[["fix_dfs"]][1]
    df_fs = controls[["fix_dfs"]][2]
  }
  T = controls[["time_horizon"]][1]
  T_star = controls[["time_horizon"]][2]
  
  thetaUncon = init_est(controls)
  thetaCon = thetaUncon2thetaCon(thetaUncon,controls)
  thetaList = thetaCon2thetaList(thetaCon,controls)
  
  simulateStates = function(delta,Gamma,T){
    N = length(delta)
    states = numeric(T)
    states[1] = sample(1:N,1,prob=delta)
    for(t in 2:T){
      states[t] = sample(1:N,1,prob=Gamma[states[t-1],])
    }
    return(states)
  }
  
  simulateObservations = function(states,mus,sigmas,dfs,T){
    obs = numeric(T)
    for(t in 1:T){
      obs[t] = rt(1,dfs[states[t]])*sigmas[states[t]]+mus[states[t]]
    }
    return(obs)
  }
  
  if(controls$model=="HMM"){ 
    states = simulateStates(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    observations = simulateObservations(states,thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],T)
  }
  
  if(controls$model=="HHMM"){ 
    states = matrix(0,T,T_star+1) 
    observations = matrix(0,T,T_star+1)
    states[,1] = simulateStates(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    observations[,1] = simulateObservations(states[,1],thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],T)
    for(t in 1:T){
      S_t = states[t,1]
      states[t,-1] = simulateStates(Gamma2delta(thetaList[["Gammas_star"]][[S_t]]),thetaList[["Gammas_star"]][[S_t]],T_star)
      observations[t,-1] = simulateObservations(states[t,-1],thetaList[["mus_star"]][[S_t]],thetaList[["sigmas_star"]][[S_t]],thetaList[["dfs_star"]][[S_t]],T_star)
    }
  }
  
  out = list(
    "observations" = observations,
    "states0"      = states,
    "thetaUncon0"  = thetaUncon,
    "thetaCon0"    = thetaCon,
    "thetaList0"   = thetaList
  )
  
  return(out)
}

### read financial data from csv file
readData = function(controls){
  dataSource = controls[["data_source"]]
  data = list()
  for(i in 1:2){
    if(!is.na(dataSource)[i]){
      data[[i]] = read.csv(file=paste0("data/",dataSource[i]),head=TRUE,sep=",",na.strings="null")
      if(!all(c("Date","Close") %in% colnames(data[[i]]))) stop("Data file must have two columns named 'Date' and 'Close'.")
      data[[i]] = data[[i]][,colnames(data[[i]]) %in% c("Date","Close"), drop = F]
      data[[i]][["Date"]] = as.Date(data[[i]][["Date"]], format="%Y-%m-%d")
      for(na_close in which(is.na(data[[i]][["Close"]]))){ ### replace NA-close values of neighbour mean
        incr = 1
        while(TRUE){
          range = unique(abs(c((na_close-incr):(na_close-1),(na_close+1):(na_close+incr))))
          replace = mean(data[[i]][["Close"]][range],na.rm=TRUE)
          if(!is.na(replace)) data[[i]][["Close"]][na_close] = replace; break
          incr = incr + 1
        }
      }
      data[[i]][["LogReturns"]] = numeric(length(data[[i]][["Close"]]))
      for(t in seq_len(length(data[[i]][["Close"]]))[-1]){
        data[[i]][["LogReturns"]][t] = log(data[[i]][["Close"]][t]/data[[i]][["Close"]][t-1])
      }
    }
  }
  
  ### truncate 'data' based on 'controls[["trunc_data"]]'
  truncateData = function(controls,data){
    ### find exact or nearest position of 'date' in 'data' 
    findDate = function(date,data){
      incr = 0
      while(TRUE){
        candidate = which(data[["Date"]]==as.Date(date)+incr)
        if(length(candidate)==1) return(candidate)
        candidate = which(data[["Date"]]==as.Date(date)-incr)
        if(length(candidate)==1) return(candidate)
        incr = incr + 1
      }
    }
    t_max = controls[["truncate_data"]][2]
    if(!is.na(t_max)){
      data = data[seq_len(findDate(t_max,data)),]
    }
    t_min = controls[["truncate_data"]][1]
    if(!is.na(t_min)){
      temp = seq_len(findDate(t_min,data)-1)
      if(length(temp)>0) data = data[-temp,]
    }
    return(data)
  }
  
  ### HMM data
  if(controls[["model"]]=="HMM"){
    data[[1]] = truncateData(controls,data[[1]])
    
    out = list(
      "observations" = data[[1]][["LogReturns"]],
      "closes" = data[[1]][["Close"]],
      "dates" = data[[1]][["Date"]]
    )
  }
  
  ### HHMM data
  if(controls[["model"]]=="HHMM"){
    
    ### same data on fine and coarse scale
    if(controls[["HHMM_av"]]){
      data[[2]] = truncateData(controls,data[[2]])
      
      T_star = controls[["time_horizon"]][2]
      T = floor(length(data[[2]]$Close)/T_star)
      data[[2]] = data[[2]][1:(T*T_star),]
      fs_obs = matrix(data[[2]][["LogReturns"]],ncol=T_star,nrow=T,byrow=TRUE)
      cs_obs = numeric(T)
      for(t in 1:T){
        cs_obs[t] = mean(fs_obs[t,])
      }
      
      out = list(
        "observations" = cbind(cs_obs,fs_obs,deparse.level=0),
        "closes" = data[[2]][["Close"]],
        "dates" = data[[2]][["Date"]]
      )
    }
      
    ### different data on fine and coarse scale
    if(!controls[["HHMM_av"]]){
      ### remove data points that do not occur in both files
      data[[1]] = data[[1]][data[[1]][["Date"]] %in% intersect(data[[1]][["Date"]],data[[2]][["Date"]]),]
      data[[2]] = data[[2]][data[[2]][["Date"]] %in% intersect(data[[2]][["Date"]],data[[1]][["Date"]]),]
      
      data[[1]] = truncateData(controls,data[[1]])
      data[[2]] = truncateData(controls,data[[2]])
      
      if(any(dim(data[[1]])!=dim(data[[2]]))) stop()
      
      T_star = controls[["time_horizon"]][2]
      T = floor(length(data[[1]]$Close)/T_star)
      data[[1]] = data[[1]][1:(T*T_star),]
      data[[2]] = data[[2]][1:(T*T_star),]
      fs_obs = matrix(data[[2]][["LogReturns"]],ncol=T_star,nrow=T,byrow=TRUE)
      cs_obs = rowMeans(matrix(data[[1]][["LogReturns"]],ncol=T_star,nrow=T,byrow=TRUE))
      
      out = list(
        "observations" = cbind(cs_obs,fs_obs,deparse.level=0),
        "closes" = data[[2]][["Close"]],
        "closes_cs" = data[[1]][["Close"]],
        "dates" = data[[2]][["Date"]]
      )
    }
  }
	
	return(out)
}



