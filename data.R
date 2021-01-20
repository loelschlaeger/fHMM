### process simulated or empirical data
get_data = function(controls){
  if(is.null(controls[["controls_checked"]])) stop("'controls' invalid",call.=FALSE)
  if(controls[["sim"]])  data = simulate_data(controls)
  if(!controls[["sim"]]) data = read_data(controls)
  writeLines("Data processing successful.")
  check_data(controls,data)
  return(data)
}

### simulate data
simulate_data = function(controls){
  if(!is.null(controls[["seed"]])) set.seed(controls[["seed"]])
  
  M = controls[["states"]][1] #HMM states / HHMM coarse-scale states
  N = controls[["states"]][2] #HHMM fine-scale states
  T = controls[["time_horizon"]][1]
  T_star = controls[["time_horizon"]][2]
  
  thetaUncon = init_est(controls)
  thetaCon = thetaUncon2thetaCon(thetaUncon,controls)
  thetaList = thetaCon2thetaList(thetaCon,controls)
  
  simulate_states = function(delta,Gamma,T){
    no_states = length(delta)
    states = numeric(T)
    states[1] = sample(1:no_states,1,prob=delta)
    for(t in 2:T){
      states[t] = sample(1:no_states,1,prob=Gamma[states[t-1],])
    }
    return(states)
  }
  
  simulate_logReturns = function(states,mus,sigmas,dfs){
    T = length(states)
    logReturns = numeric(T)
    for(t in 1:T){
      logReturns[t] = rt(1,dfs[states[t]])*sigmas[states[t]]+mus[states[t]]
    }
    return(logReturns)
  }
  
  if(controls[["model"]]=="HMM"){ 
    states = simulate_states(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    logReturns = simulate_logReturns(states,thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]])
  }
  
  if(controls[["model"]]=="HHMM"){ 
    states = matrix(0,T,T_star+1) 
    logReturns = matrix(0,T,T_star+1)
    states[,1] = simulate_states(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    logReturns[,1] = simulate_logReturns(states[,1],thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]])
    for(t in 1:T){
      S_t = states[t,1]
      states[t,-1] = simulate_states(Gamma2delta(thetaList[["Gammas_star"]][[S_t]]),thetaList[["Gammas_star"]][[S_t]],T_star)
      logReturns[t,-1] = simulate_logReturns(states[t,-1],thetaList[["mus_star"]][[S_t]],thetaList[["sigmas_star"]][[S_t]],thetaList[["dfs_star"]][[S_t]])
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
  
  ### truncate the data
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
      "dataRaw" = data[[1]][[data_col[1]]],
      "dates" = data[[1]][["Date"]]
      )
  }
  
  ### HHMM data
  if(controls[["model"]]=="HHMM"){
      
    ### remove data points that do not occur in both files
    data[[1]] = data[[1]][ data[[1]][["Date"]] %in% intersect(data[[1]][["Date"]],data[[2]][["Date"]]), ]
    data[[2]] = data[[2]][ data[[2]][["Date"]] %in% intersect(data[[2]][["Date"]],data[[1]][["Date"]]), ]
    
    data[[1]] = truncate_data(controls,data[[1]])
    data[[2]] = truncate_data(controls,data[[2]])
    
    if(any(dim(data[[1]])!=dim(data[[2]]))) stop("Processing of the datasets failed.")
    data_length = length(data[[1]][["Date"]])
    
    T_star = controls[["time_horizon"]][2]
    T = floor(data_length/T_star)
    data[[1]] = data[[1]][1:(T*T_star),]
    data[[2]] = data[[2]][1:(T*T_star),]
    
    ### compute CS data
    if(controls[["data_cs_type"]] == "mean") cs_logReturns = rowMeans(matrix(data[[1]][["LogReturns"]],ncol=T_star,nrow=T,byrow=TRUE))
    if(controls[["data_cs_type"]] == "mean_abs") cs_logReturns = rowMeans(abs(matrix(data[[1]][["LogReturns"]],ncol=T_star,nrow=T,byrow=TRUE)))
    if(controls[["data_cs_type"]] == "sum_abs") cs_logReturns = rowSums(abs(matrix(data[[1]][["LogReturns"]],ncol=T_star,nrow=T,byrow=TRUE)))
    
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
download_data = function(name=NA,symbol=NA,from=as.Date("1902-01-01"),to=Sys.Date(),show_symbols=FALSE){
  
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
      writeLines("No saved stock symbols.")
    }
  } 
  if(!is.na(name)){  
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
    if(is.na(symbol)){
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
    writeLines("Download successful.")
    writeLines(paste("Source:",paste0(name,".csv")))
    writeLines(paste("From:  ",head(data$Date,n=1)))
    writeLines(paste("To:    ",tail(data$Date,n=1)))
  }
  
}