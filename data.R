### process simulated or empirical data
get_data = function(controls,sim_par){
  if(is.null(controls[["controls_checked"]])) stop("'controls' invalid",call.=FALSE)
  if(controls[["sim"]])  data = simulate_data(controls,sim_par)
  if(!controls[["sim"]]) data = read_data(controls)
  message("Data processed.")
  check_data(controls,data)
  return(data)
}

### compute (flexible) fine-scale chunk lengths
compute_fs_lengths = function(fs_time_horizon,T=NULL,fs_dates=NULL){
  if(is.null(fs_dates)){
    if(is.numeric(fs_time_horizon)){
      T_star = rep(fs_time_horizon,T)
    }
    if(fs_time_horizon %in% c("w","m","q","y")){
      if(fs_time_horizon == "w") size = 5
      if(fs_time_horizon == "m") size = 25
      if(fs_time_horizon == "q") size = 70
      if(fs_time_horizon == "y") size = 260
      T_star = sample(1:size,T,replace=TRUE,prob=dbinom(1:size,size,0.9)/sum(dbinom(1:size,size,0.9)))
    }
  }
  if(!is.null(fs_dates)){
    dates_overview = data.frame("w" = as.numeric(strftime(fs_dates,format ="%W")),
                                "m" = as.numeric(strftime(fs_dates,format ="%m")),
                                "q" = as.numeric(substr(quarters(fs_dates),2,2)),
                                "y" = as.numeric(strftime(fs_dates,format ="%Y")))
    if(fs_time_horizon == "w"){
      T_star = vector()
      for(y in unique(dates_overview[["y"]])){
        dates_overview_subset = dates_overview[dates_overview[["y"]]==y,]
        T_star = c(T_star,as.vector(table(dates_overview_subset[["w"]])))
      }
    }
    if(fs_time_horizon == "m"){
      T_star = vector()
      for(y in unique(dates_overview[["y"]])){
        dates_overview_subset = dates_overview[dates_overview[["y"]]==y,]
        T_star = c(T_star,as.vector(table(dates_overview_subset[["m"]])))
      }
    }
    if(fs_time_horizon == "q"){
      T_star = vector()
      for(y in unique(dates_overview[["y"]])){
        dates_overview_subset = dates_overview[dates_overview[["y"]]==y,]
        T_star = c(T_star,as.vector(table(dates_overview_subset[["q"]])))
      }
    }
    if(fs_time_horizon == "y"){
      T_star = as.vector(table(dates_overview[["y"]]))
    }
    if(is.numeric(fs_time_horizon)){
      T_star = rep(fs_time_horizon,floor(length(fs_dates)/fs_time_horizon))
    }
  }
  return(T_star)
}

### simulate data
simulate_data = function(controls,sim_par){
  if(!is.null(controls[["seed"]])) set.seed(controls[["seed"]])
  
  M = controls[["states"]][1] 
  N = controls[["states"]][2] 
  T = as.numeric(controls[["time_horizon"]][1])
  
  if(!is.null(sim_par)){
    thetaUncon = sim_par
  } else {
    thetaUncon = init_est(controls)
  }
  thetaCon   = thetaUncon2thetaCon(thetaUncon,controls)
  thetaList  = thetaCon2thetaList(thetaCon,controls)
  
  simulate_states = function(delta,Gamma,T){
    no_states = length(delta)
    states = numeric(T)
    states[1] = sample(1:no_states,1,prob=delta)
    for(t in 2:T){
      states[t] = sample(1:no_states,1,prob=Gamma[states[t-1],])
    }
    return(states)
  }
  
  simulate_logReturns = function(states,mus,sigmas,dfs,sdd){
    T = length(states)
    logReturns = numeric(T)
    for(t in 1:T){
      if(sdd == "t"){
        logReturns[t] = rt(1,dfs[states[t]])*sigmas[states[t]]+mus[states[t]]
      }
      if(sdd == "gamma"){
        logReturns[t] = rgamma(1,shape=mus[states[t]]^2/sigmas[states[t]]^2,scale=sigmas[states[t]]^2/mus[states[t]])
      }
    }
    return(logReturns)
  }
  
  if(controls[["model"]]=="HMM"){
    T_star = NA
    states = simulate_states(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    logReturns = simulate_logReturns(states,thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],controls[["sdds"]][1])
  }
  
  if(controls[["model"]]=="HHMM"){ 
    T_star = compute_fs_lengths(fs_time_horizon = controls[["time_horizon"]][2], T = T)
    states = matrix(NA,T,max(T_star)+1) 
    logReturns = matrix(NA,T,max(T_star)+1)
    states[,1] = simulate_states(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    logReturns[,1] = simulate_logReturns(states[,1],thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],controls[["sdds"]][1])
    for(t in 1:T){
      S_t = states[t,1]
      states[t,-1] = c(simulate_states(Gamma2delta(thetaList[["Gammas_star"]][[S_t]]),thetaList[["Gammas_star"]][[S_t]],T_star[t]),rep(NA,max(T_star)-T_star[t]))
      logReturns[t,-1] = c(simulate_logReturns(states[t,-1][!is.na(states[t,-1])],thetaList[["mus_star"]][[S_t]],thetaList[["sigmas_star"]][[S_t]],thetaList[["dfs_star"]][[S_t]],controls[["sdds"]][2]),rep(NA,max(T_star)-T_star[t]))
    }
  }
  
  out = list(
    "logReturns"   = logReturns,
    "states0"      = states,
    "thetaUncon0"  = thetaUncon,
    "thetaCon0"    = thetaCon,
    "thetaList0"   = thetaList,
    "T_star"       = T_star
    )
  
  return(out)
}

### read financial data from csv file
read_data = function(controls){
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
  
  ### function that truncates the data
  truncate_data = function(controls,data){
    ### find exact or nearest position of 'date' in 'data' 
    find_date = function(date,data){
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
      data = data[seq_len(find_date(t_max,data)),]
    }
    t_min = controls[["truncate_data"]][1]
    if(!is.na(t_min)){
      temp = seq_len(find_date(t_min,data)-1)
      if(length(temp)>0) data = data[-temp,]
    }
    return(data)
  }
  
  ### HMM data
  if(controls[["model"]]=="HMM"){
    data[[1]] = truncate_data(controls,data[[1]])
    
    out = list(
      "logReturns" = data[[1]][["LogReturns"]],
      "dataRaw"    = data[[1]][[data_col[1]]],
      "dates"      = data[[1]][["Date"]],
      "T_star"     = NA
      )
  }
  
  ### HHMM data
  if(controls[["model"]]=="HHMM"){
      
    ### remove data points that do not occur in both files
    data[[1]] = data[[1]][ data[[1]][["Date"]] %in% intersect(data[[1]][["Date"]],data[[2]][["Date"]]), ]
    data[[2]] = data[[2]][ data[[2]][["Date"]] %in% intersect(data[[2]][["Date"]],data[[1]][["Date"]]), ]
    
    data[[1]] = truncate_data(controls,data[[1]])
    data[[2]] = truncate_data(controls,data[[2]])
    
    if(any(dim(data[[1]])!=dim(data[[2]]))) stop("Processing of the datasets failed.",call.=FALSE)
    
    T_star = compute_fs_lengths(fs_time_horizon = controls[["time_horizon"]][2], fs_dates = data[[2]][["Date"]])
    T = length(T_star)

    data[[1]] = data[[1]][seq_len(sum(T_star)),]
    data[[2]] = data[[2]][seq_len(sum(T_star)),]
    
    ### format CS and FS data
    cs_data_tbt = matrix(NA,nrow=T,ncol=max(T_star))
    fs_data     = matrix(NA,nrow=T,ncol=max(T_star))
    for(t in seq_len(T)){
      cs_data_tbt[t,] = c(data[[1]][["LogReturns"]][(sum(T_star[seq_len(t-1)])+1):sum(T_star[seq_len(t)])],rep(NA,max(T_star)-T_star[t]))
      fs_data[t,]     = c(data[[2]][["LogReturns"]][(sum(T_star[seq_len(t-1)])+1):sum(T_star[seq_len(t)])],rep(NA,max(T_star)-T_star[t]))
    }
    
    ### transform CS data
    if(controls[["data_cs_type"]] == "mean"){
      cs_data = rowMeans(cs_data_tbt,na.rm = TRUE)
    }
    if(controls[["data_cs_type"]] == "sum_abs"){
      cs_data = rowSums(abs(cs_data_tbt),na.rm = TRUE)
    }
    
    out = list(
      "logReturns" = cbind(cs_data,fs_data,deparse.level=0),
      "dataRaw"    = data[[2]][[data_col[2]]],
      "dataRaw_cs" = data[[1]][[data_col[1]]],
      "dates"      = data[[2]][["Date"]],
      "T_star"     = T_star
      )
  }
	
	return(out)
}

### download data from www.finance.yahoo.com
download_data = function(name=NULL,symbol=NULL,from=as.Date("1902-01-01"),to=Sys.Date(),show_symbols=FALSE){
  
  ### load and sort or create 'stock_symbols'
  if(file.exists("data/stock_symbols.rds")){
    stock_symbols = readRDS("data/stock_symbols.rds")
    stock_symbols = stock_symbols[order(stock_symbols["name"]),]
  } else {
    stock_symbols = data.frame("name"=character(),"symbol"=character())
    saveRDS(stock_symbols,file="data/stock_symbols.rds")
  }
  
  ### print 'stock_symbols'
  if(show_symbols){
    if(dim(stock_symbols)[1]!=0){
      print(stock_symbols,row.names = FALSE)
    } else {
      message("No saved stock symbols.")
    }
  } 
  if(!is.null(name)){  
    ### convert 'from' and 'to' to dates
    from = as.Date(from)
    to = as.Date(to)
  
    ### define minimum date 'from'
    min_date = as.Date("1902-01-01")
    if(from < min_date) from = min_date
    
    ### function to create finance.yahoo.com-URL
    create_url = function(symbol,from,to){
      t1 = as.integer(ISOdate(as.numeric(format(from,format="%Y")),as.numeric(format(from,format="%m")),as.numeric(format(from,format="%d")),hour=0))
      t2 = as.integer(ISOdate(as.numeric(format(to,format="%Y")),as.numeric(format(to,format="%m")),as.numeric(format(to,format="%d")),hour=24))
      url = paste("https://query1.finance.yahoo.com/v7/finance/download/",symbol,"?period1=",t1,"&period2=",t2,"&interval=1d&events=history",sep="")
      return(url)
    }
    
    ### covert 'name' to lowercase
    name = tolower(name)
    
    ### search 'name' in 'stock_symbols' and get corresponding 'symbol'
    if(is.null(symbol)){
      if(name %in% stock_symbols[["name"]]){
        symbol = stock_symbols[which(stock_symbols["name"]==name),"symbol"]
      } else {
        stop(paste0("Symbol for the stock '",name,"' is unknown."),call.=FALSE)
      }
    } else {
      if(name %in% stock_symbols["name"]){
        symbol = stock_symbols[which(stock_symbols["name"]==name),"symbol"]
      } else {
        read_try = suppressWarnings(try(read.csv(create_url(symbol,from,to)),silent=TRUE))
        if(inherits(read_try, "try-error")){
          stop(paste0("Symbol '",symbol,"' is invalid."),call.=FALSE)
        } else {
          ### save new symbol
          stock_symbols[nrow(stock_symbols)+1,] = c(name,symbol)
          saveRDS(stock_symbols,file="data/stock_symbols.rds")
        }
      }
    }
    
    ### download and save data
    filename = paste0("data/",name,".csv")
    download.file(create_url(symbol,from,to),destfile=filename,quiet=TRUE)
    
    ### print summary of new data
    data = read.csv(file=filename,head=TRUE,sep=",",na.strings="null") 
    message("Data download successful.")
    message(paste("Source:",paste0(name,".csv")))
    message(paste("Symbol:",symbol))
    message(paste("From:",head(data$Date,n=1)))
    message(paste("To:",tail(data$Date,n=1)))
  }
  
}