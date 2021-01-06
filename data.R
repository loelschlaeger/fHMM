getData = function(controls){
  if(is.null(controls[["controls_checked"]])) stop("'controls' invalid",call.=FALSE)
  if(controls[["sim"]]) data = simulateData(controls)
  if(!controls[["sim"]]) data = readData(controls)
  if(any(dim(t(data[["logReturns"]]))==0)) stop("Too few data points.",call.=FALSE)
  writeLines("Data processing successful.")
  check_data(controls,data)
  return(data)
}

### simulate data
simulateData = function(controls){
  if(!is.null(controls[["seed"]])) set.seed(controls[["seed"]])
  M = controls[["states"]][1] #HMM states / HHMM coarse-scale states
  N = controls[["states"]][2] #HHMM fine-scale states
  T = controls[["time_horizon"]][1]
  T_star = controls[["time_horizon"]][2]
  
  thetaUncon = init_est(controls)
  thetaCon = thetaUncon2thetaCon(thetaUncon,controls)
  thetaList = thetaCon2thetaList(thetaCon,controls)
  
  simulateStates = function(delta,Gamma,T){
    no_states = length(delta)
    states = numeric(T)
    states[1] = sample(1:no_states,1,prob=delta)
    for(t in 2:T){
      states[t] = sample(1:no_states,1,prob=Gamma[states[t-1],])
    }
    return(states)
  }
  
  simulateLogReturns = function(states,mus,sigmas,dfs){
    T = length(states)
    logReturns = numeric(T)
    for(t in 1:T){
      logReturns[t] = rt(1,dfs[states[t]])*sigmas[states[t]]+mus[states[t]]
    }
    return(logReturns)
  }
  
  if(controls[["model"]]=="HMM"){ 
    states = simulateStates(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    logReturns = simulateLogReturns(states,thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]])
  }
  
  if(controls[["model"]]=="HHMM"){ 
    states = matrix(0,T,T_star+1) 
    logReturns = matrix(0,T,T_star+1)
    states[,1] = simulateStates(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    logReturns[,1] = simulateLogReturns(states[,1],thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]])
    for(t in 1:T){
      S_t = states[t,1]
      states[t,-1] = simulateStates(Gamma2delta(thetaList[["Gammas_star"]][[S_t]]),thetaList[["Gammas_star"]][[S_t]],T_star)
      logReturns[t,-1] = simulateLogReturns(states[t,-1],thetaList[["mus_star"]][[S_t]],thetaList[["sigmas_star"]][[S_t]],thetaList[["dfs_star"]][[S_t]])
    }
  }
  
  out = list(
    "logReturns"   = logReturns,
    "states0"      = states,
    "thetaUncon0"  = thetaUncon,
    "thetaCon0"    = thetaCon,
    "thetaList0"   = thetaList
    )
  
  return(out)
}

### read financial data from csv file
readData = function(controls){
  data_source = controls[["data_source"]]
  data_col = controls[["data_col"]]
  
  data = list()
  for(i in 1:2){
    if(is.na(data_source)[i]){
      data[[i]] = NA
    }
    
    if(!is.na(data_source)[i]){
      
      ### extract data
      data[[i]] = read.csv(file=paste0("data/",data_source[i]),head=TRUE,sep=",",na.strings="null")
      if(!"Date" %in% colnames(data[[i]])) stop(paste0("Data file '",data_source[i],"' must have a column named 'Date'."))
      if(!data_col[i] %in% colnames(data[[i]])) stop(paste0("Data file '",data_source[i],"' does not have a column named '",data_col[i],"'."))
      data[[i]] = data[[i]][,colnames(data[[i]]) %in% c("Date",data_col[i]), drop = FALSE]
      data[[i]][["Date"]] = as.Date(data[[i]][["Date"]], format="%Y-%m-%d")
      data[[i]][[data_col[i]]] = as.numeric(data[[i]][[data_col[i]]])
      
      ### remove NA dates
      data[[i]] = data[[i]][!is.na(data[[i]][["Date"]]),]
      
      ### replace NA values by neighbour means
      for(na_value in which(is.na(data[[i]][[data_col[i]]]))){
        incr = 1
        while(TRUE){
          range = unique(abs(c((na_value-incr):(na_value-1),(na_value+1):(na_value+incr))))
          replace = mean(data[[i]][[data_col[i]]][range],na.rm=TRUE)
          if(!is.nan(replace)){
            data[[i]][[data_col[i]]][na_value] = replace
            break
          }
          incr = incr + 1
        }
      }
      
      ### compute log-returns
      data_length = length(data[[i]][[data_col[i]]])
      data[[i]][["LogReturns"]] = numeric(data_length)
      for(t in seq_len(data_length)[-1]){
        data[[i]][["LogReturns"]][t] = log(data[[i]][[data_col[i]]][t]/data[[i]][[data_col[i]]][t-1])
      }
    }
  }
  
  ### truncate the data based on 'controls[["trunc_data"]]'
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
      "logReturns" = data[[1]][["LogReturns"]],
      "dataRaw" = data[[1]][[data_col[1]]],
      "dates" = data[[1]][["Date"]]
      )
  }
  
  ### HHMM data
  if(controls[["model"]]=="HHMM"){
      
    ### remove data points that do not occur in both files
    data[[1]] = data[[1]][ data[[1]][["Date"]] %in% intersect(data[[1]][["Date"]],data[[2]][["Date"]]), ]
    data[[2]] = data[[2]][ data[[2]][["Date"]] %in% intersect(data[[2]][["Date"]],data[[1]][["Date"]]), ]
    
    data[[1]] = truncateData(controls,data[[1]])
    data[[2]] = truncateData(controls,data[[2]])
    
    if(any(dim(data[[1]])!=dim(data[[2]]))) stop("Processing of the datasets failed.")
    data_length = length(data[[1]][["Date"]])
    
    T_star = controls[["time_horizon"]][2]
    T = floor(data_length/T_star)
    data[[1]] = data[[1]][1:(T*T_star),]
    data[[2]] = data[[2]][1:(T*T_star),]
    
    cs_logReturns = rowMeans(matrix(data[[1]][["LogReturns"]],ncol=T_star,nrow=T,byrow=TRUE))
    fs_logReturns = matrix(data[[2]][["LogReturns"]],ncol=T_star,nrow=T,byrow=TRUE)
    logReturns = cbind(cs_logReturns,fs_logReturns,deparse.level=0)
    
    out = list(
      "logReturns" = logReturns,
      "dataRaw"    = data[[2]][[data_col[2]]],
      "dataRaw_cs" = data[[1]][[data_col[1]]],
      "dates"      = data[[2]][["Date"]]
      )
  }
	
	return(out)
}


### download data from www.finance.yahoo.com
downloadData = function(name,symbol=NULL,from=as.Date("1902-01-01"),to=Sys.Date()){
  from = as.Date(from)
  to = as.Date(to)
  
  ### search stock via name
  
  ### if name is not in data.frame, symbol has to be supplied. then symbol is saved in .RData file (data.frame) in /data with stock name.
  
  ### if name="all", print all elements of data.frame
  
  min_date = as.Date("1902-01-01")
  if(from < min_date) stop(call.=FALSE)
  t1 = as.integer(ISOdate(1902,1,1,hour=0))
  t2 = as.integer(ISOdate(2025,6,19,hour=0))
  url = paste("https://query1.finance.yahoo.com/v7/finance/download/",stock,"?period1=",t1,"&period2=",t2,"&interval=1d&events=history",sep="")
  dataset = read.csv(url)
}


