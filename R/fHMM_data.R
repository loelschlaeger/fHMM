#' Data download
#' @description 
#' Download financial data from <https://finance.yahoo.com>.
#' @param name A character, personal identifier for a stock, default \code{NA}.
#' @param symbol A character, the stock's symbol, default \code{NA}.
#' @param from A date, setting the lower data bound, default is \code{"1902-01-01"}.
#' @param to A date, setting the upper data bound, default is the current date \code{Sys.date()}.
#' @param show_symbols A boolean, determining whether all saved symbols should be printed, default \code{FALSE}.
#' @param path A character, setting the data saving path.
#' @return 
#' No return value. Downloaded data is saved as "\code{name}.csv" in the folder "\code{path}/data".
#' @details 
#' \code{symbol} has to match the official symbol on <https://finance.yahoo.com>. 
#' Once used stock symbols are saved in "stock_symbols.rds" in the folder "\code{path}/data".
#' Values for \code{from} earlier than its default value are set to the default value.
#' @examples
#' ### download 21st century DAX data
#' download_data(name="dax",symbol="^GDAXI",from=as.Date("2000-01-03"),path=tempdir())
#' @export

download_data = function(name=NA, symbol=NA, from="1902-01-01", to=Sys.Date(), show_symbols=FALSE, path){
  
  ### load and sort or create 'stock_symbols'
  save_path = paste0(path,"/data")
  if(!dir.exists(save_path)){
    dir.create(save_path)
  }
  if(file.exists(paste0(save_path,"/stock_symbols.rds"))){
    stock_symbols = readRDS(paste0(save_path,"/stock_symbols.rds"))
    stock_symbols = unique(stock_symbols)
  } else {
    stock_symbols = data.frame("name"=character(),"symbol"=character())
    saveRDS(stock_symbols,file=paste0(save_path,"/stock_symbols.rds"))
  }
  
  ### print 'stock_symbols'
  if(show_symbols){
    if(dim(stock_symbols)[1]!=0){
      print(stock_symbols,row.names = FALSE)
    } else {
      message("No saved stock symbols.")
    }
  } 
  if(!is.na(name)){  
    ### convert 'from' and 'to' to dates
    from = as.Date(from)
    to = as.Date(to)
    ### define minimum date 'from'
    min_date = as.Date("1902-01-01")
    if(from < min_date){
      warning("D.1")
      from = min_date
    }
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
        stop("D.2")
      }
    } else {
      if(name %in% stock_symbols["name"]){
        symbol = stock_symbols[which(stock_symbols["name"]==name),"symbol"]
      } else {
        read_try = suppressWarnings(try(read.csv(create_url(symbol,from,to)),silent=TRUE))
        if(inherits(read_try, "try-error")){
          stop("D.3")
        } else {
          ### save new symbol
          stock_symbols[nrow(stock_symbols)+1,] = c(name,symbol)
          saveRDS(stock_symbols,file=paste0(save_path,"/stock_symbols.rds"))
        }
      }
    }
    
    ### download and save data
    filename = paste0(save_path,"/",name,".csv")
    download.file(create_url(symbol,from,to),destfile=filename,quiet=TRUE)
    
    ### print summary of new data
    data = read.csv(file=filename,header=TRUE,sep=",",na.strings="null") 
    message(paste0("Downloaded data ",symbol," from ",head(data$Date,n=1)," to " ,tail(data$Date,n=1)),".")
  }
}

#' Prepare data for the fHMM package.
#' @description 
#' This function simulates or reads financial data for the fHMM package.
#' @param controls 
#' An object of class \code{fHMM_controls}.
#' @param true_parameter 
#' An object of class \code{fHMM_parameters}.
#' @return 
#' An object of class \code{fHMM_data}.
#' @export

prepare_data = function(controls, true_parameter){
  
  ### process data
  if(controls[["sim"]])
    data = simulate_data(controls, true_parameter)
  if(!controls[["sim"]])
    data = read_data(controls)
  
  ### check for improper use of state-dependent gamma distribution
  if(controls[["model"]]=="hmm"){
    if(controls[["sdds"]][1]=="gamma" & any(data[["data"]]<0))
      stop("C.7")
  }
  if(controls[["model"]]=="hhmm"){
    if(controls[["sdds"]][1]=="gamma" & any(data[["data"]][,1]<0))
      stop("C.7")
    if(controls[["sdds"]][2]=="gamma" & any(data[["data"]][,-1]<0,na.rm=TRUE))
      stop("C.7")
  }
  
  ### build and return object of class 'fHMM_data'
  data = data
  class(data) = "fHMM_data"
  return(data)
}

#' @title Data simulation
#' @description Simulates data from a (hierarchical) hidden Markov model.
#' @param controls A list of controls.
#' @param true_parameter A list of model parameters for simulation in \code{thetaList} format.
#' @return A list containing the following elements:
#' \item{data}{A matrix of simulated data.}
#' \item{states0}{A matrix of simulated hidden states.}
#' \item{thetaUncon0}{True parameters in format \code{thetaUncon}.}
#' \item{thetaCon0}{True parameters in format \code{thetaCon}.}
#' \item{thetaList0}{True parameters in format \code{thetaList}.}
#' \item{T_star}{A vector of fine-scale chunk sizes.}

simulate_data = function(controls, true_parameter){

  if(!is.null(controls[["fit"]][["seed"]]))
    set.seed(controls[["fit"]][["seed"]])
  T = as.numeric(controls[["horizon"]][1])
  
  ### define simulation parameters
  if(!is.null(true_parameter)){
    thetaUncon = thetaList2thetaUncon(true_parameter,controls)
  } else {
    thetaUncon = init_est(controls)
  }
  thetaCon   = thetaUncon2thetaCon(thetaUncon,controls)
  thetaList  = thetaCon2thetaList(thetaCon,controls)
  
  ### simulate hidden states
  simulate_states = function(delta,Gamma,T){
    no_states = length(delta)
    states = numeric(T)
    states[1] = sample(1:no_states,1,prob=delta)
    for(t in 2:T){
      states[t] = sample(1:no_states,1,prob=Gamma[states[t-1],])
    }
    return(states)
  }
  
  ### simulate observations
  simulate_observations = function(states,mus,sigmas,dfs,sdd){
    T = length(states)
    data = numeric(T)
    for(t in 1:T){
      if(sdd == "t"){
        data[t] = rt(1,dfs[states[t]])*sigmas[states[t]]+mus[states[t]]
      }
      if(sdd == "gamma"){
        data[t] = rgamma(1,shape=mus[states[t]]^2/sigmas[states[t]]^2,scale=sigmas[states[t]]^2/mus[states[t]])
      }
    }
    return(data)
  }
  
  if(controls[["model"]]=="hmm"){
    T_star = NA
    states = simulate_states(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    data = simulate_observations(states,thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],controls[["sdds"]][1])
  }
  if(controls[["model"]]=="hhmm"){ 
    T_star = compute_fs(fs_time_horizon = controls[["horizon"]][2], T = T)
    states = matrix(NA,T,max(T_star)+1) 
    data = matrix(NA,T,max(T_star)+1)
    states[,1] = simulate_states(Gamma2delta(thetaList[["Gamma"]]),thetaList[["Gamma"]],T) 
    data[,1] = simulate_observations(states[,1],thetaList[["mus"]],thetaList[["sigmas"]],thetaList[["dfs"]],controls[["sdds"]][1])
    for(t in 1:T){
      S_t = states[t,1]
      states[t,-1] = c(simulate_states(Gamma2delta(thetaList[["Gammas_star"]][[S_t]]),thetaList[["Gammas_star"]][[S_t]],T_star[t]),rep(NA,max(T_star)-T_star[t]))
      data[t,-1] = c(simulate_observations(states[t,-1][!is.na(states[t,-1])],thetaList[["mus_star"]][[S_t]],thetaList[["sigmas_star"]][[S_t]],thetaList[["dfs_star"]][[S_t]],controls[["sdds"]][2]),rep(NA,max(T_star)-T_star[t]))
    }
  }
  
  out = list(
    "data"        = data,
    "states0"     = states,
    "thetaUncon0" = thetaUncon,
    "thetaCon0"   = thetaCon,
    "thetaList0"  = thetaList,
    "T_star"      = T_star
  )
  return(out)
}

#' @title Read .csv-file
#' @description Reads financial data from .csv-file.
#' @param controls A list of controls.
#' @return A list containing the following elements:
#' \item{data}{A matrix of data that is modeled.}
#' \item{data_raw}{A matrix of raw data.}
#' \item{data_fs_raw}{A matrix of raw fine-scale data.}
#' \item{data_cs_raw}{A matrix of raw coarse-scale data.}
#' \item{dates}{A vector of dates.}
#' \item{T_star}{A vector of fine-scale chunk sizes.}

read_data = function(controls){
  
  if(is.null(controls[["controls_checked"]]))
    stop("F.6")
  
  data_source = controls[["data"]][["source"]]
  data_col = controls[["data"]][["column"]]
  data_raw = list()
  for(i in 1:2){
    if(is.na(data_source[i])){
      data_raw[[i]] = NA
    }
    if(!is.na(data_source[i])){
      
      ### extract data
      data_raw[[i]] = read.csv(file=paste0(controls[["path"]],"/data/",data_source[i]),header=TRUE,sep=",",na.strings="null")
      if(!"Date" %in% colnames(data_raw[[i]]) || !data_col[i] %in% colnames(data_raw[[i]]))
        stop("D.4")
      
      data_raw[[i]] = data_raw[[i]][,colnames(data_raw[[i]]) %in% c("Date",data_col[i]), drop = FALSE]
      data_raw[[i]][["Date"]] = as.Date(data_raw[[i]][["Date"]], format="%Y-%m-%d")
      data_raw[[i]][[data_col[i]]] = as.numeric(data_raw[[i]][[data_col[i]]])
      
      ### remove NA dates
      data_raw[[i]] = data_raw[[i]][!is.na(data_raw[[i]][["Date"]]),]
      
      ### replace NA values by neighbour means
      for(na_value in which(is.na(data_raw[[i]][[data_col[i]]]))){
        incr = 1
        while(TRUE){
          range = unique(abs(c((na_value-incr):(na_value-1),(na_value+1):(na_value+incr))))
          replace = mean(data_raw[[i]][[data_col[i]]][range],na.rm=TRUE)
          if(!is.nan(replace)){
            data_raw[[i]][[data_col[i]]][na_value] = replace
            break
          }
          incr = incr + 1
        }
      }
      
      if(controls[["data"]][["log_returns"]][i]){
        ### compute log-returns
        data_length = length(data_raw[[i]][[data_col[i]]])
        data_raw[[i]][["LogReturns"]] = numeric(data_length)
        for(t in seq_len(data_length)[-1]){
          data_raw[[i]][["LogReturns"]][t] = log(data_raw[[i]][[data_col[i]]][t]/data_raw[[i]][[data_col[i]]][t-1])
        }
        
        ### remove 0 log-returns in case of gamma sdd to avoid numerical conflicts
        if(controls[["sdds"]][i]=="gamma"){
          for(t in seq_len(data_length)){
            if(data_raw[[i]][["LogReturns"]][t]==0){
              step = 1
              cand = 0
              while(cand==0){
                cand = mean(data_raw[[i]][["LogReturns"]][abs((t-step):(t+step))],na.rm=TRUE)
                step = step + 1
              }
              data_raw[[i]][["LogReturns"]][t] = cand
            }
          }
        }
      }
    }
  }
  
  ### function that truncates data_raw
  truncate_data = function(controls,data_raw){
    
    ### find exact or nearest position of 'date' in 'data_raw' 
    find_date = function(date,data_raw){
      incr = 0
      while(TRUE){
        candidate = which(data_raw[["Date"]]==as.Date(date)+incr)
        if(length(candidate)==1) return(candidate)
        candidate = which(data_raw[["Date"]]==as.Date(date)-incr)
        if(length(candidate)==1) return(candidate)
        incr = incr + 1
      }
    }
    t_max = controls[["data"]][["truncate"]][2]
    if(!is.na(t_max)){
      data_raw = data_raw[seq_len(find_date(t_max,data_raw)),]
    }
    t_min = controls[["data"]][["truncate"]][1]
    if(!is.na(t_min)){
      temp = seq_len(find_date(t_min,data_raw)-1)
      if(length(temp)>0) data_raw = data_raw[-temp,]
    }
    return(data_raw)
  }
  
  ### HMM data
  if(controls[["model"]]=="hmm"){
    data_raw[[1]] = truncate_data(controls,data_raw[[1]])
    
    out = list(
      "data"     = data_raw[[1]][[if(controls[["data"]][["log_returns"]][1]) "LogReturns" else data_col[1]]],
      "data_raw" = data_raw[[1]][[data_col[1]]],
      "dates"    = data_raw[[1]][["Date"]],
      "T_star"   = NA
    )
  }
  
  ### HHMM data
  if(controls[["model"]]=="hhmm"){
    
    ### remove data points that do not occur in both files
    data_raw[[1]] = data_raw[[1]][ data_raw[[1]][["Date"]] %in% intersect(data_raw[[1]][["Date"]],data_raw[[2]][["Date"]]), ]
    data_raw[[2]] = data_raw[[2]][ data_raw[[2]][["Date"]] %in% intersect(data_raw[[2]][["Date"]],data_raw[[1]][["Date"]]), ]
    data_raw[[1]] = truncate_data(controls,data_raw[[1]])
    data_raw[[2]] = truncate_data(controls,data_raw[[2]])
    T_star = compute_fs(fs_time_horizon = controls[["horizon"]][2], fs_dates = data_raw[[2]][["Date"]])
    T = length(T_star)
    data_raw[[1]] = data_raw[[1]][seq_len(sum(T_star)),]
    data_raw[[2]] = data_raw[[2]][seq_len(sum(T_star)),]
    
    ### format CS and FS data
    cs_data_tbt = matrix(NA,nrow=T,ncol=max(T_star))
    fs_data     = matrix(NA,nrow=T,ncol=max(T_star))
    for(t in seq_len(T)){
      cs_data_tbt[t,] = c(data_raw[[1]][[if(controls[["data"]][["log_returns"]][1]) "LogReturns" else data_col[1]]][(sum(T_star[seq_len(t-1)])+1):sum(T_star[seq_len(t)])],rep(NA,max(T_star)-T_star[t]))
      fs_data[t,]     = c(data_raw[[2]][[if(controls[["data"]][["log_returns"]][2]) "LogReturns" else data_col[2]]][(sum(T_star[seq_len(t-1)])+1):sum(T_star[seq_len(t)])],rep(NA,max(T_star)-T_star[t]))
    }
    
    ### transform CS data_raw
    f = controls[["data"]][["cs_transform"]]
    cs_data = apply(cs_data_tbt,1,function(x) return(f(x[!is.na(x)]))) 
    
    out = list(
      "data"        = cbind(cs_data,fs_data,deparse.level=0),
      "data_fs_raw" = data_raw[[2]][[data_col[2]]],
      "data_cs_raw" = data_raw[[1]][[data_col[1]]],
      "dates"       = data_raw[[2]][["Date"]],
      "T_star"      = T_star
    )
  }
  return(out)
}

print.fHMM_data = function() {
  
}

summary.fHMM_data = function() {
  
}

print.summary.fHMM_data = function() {
  
  ### print data characteristics
  if(controls[["sim"]]){
    if(controls[["model"]]=="hmm"){
      writeLines(sprintf("- %s %s","sample size:",length(data[["data"]])))
    }
    if(controls[["model"]]=="hhmm"){
      writeLines(sprintf("- %s %s / %s","sample size:",dim(data[["data"]])[1],length(data[["data"]][,-1][!is.na(data[["data"]][,-1])])))
    }
  } 
  if(!controls[["sim"]]){
    if(controls[["model"]]=="hmm"){
      writeLines(sprintf("- %s %s","data source:",controls[["data"]][["source"]][1]))
      writeLines(sprintf("- %s %s","data column:",controls[["data"]][["column"]][1]))
      writeLines(sprintf("- %s %s to %s","time horizon:",data[["dates"]][1],rev(data[["dates"]])[1]))
      writeLines(sprintf("- %s %s","data points:",length(data[["data"]])))
      writeLines(sprintf("- %s %s","log-returns:",controls[["data"]][["log_returns"]][1]))
    }
    if(controls[["model"]]=="hhmm"){
      writeLines(sprintf("- %s %s / %s","data source:",controls[["data"]][["source"]][1],controls[["data"]][["source"]][2]))
      writeLines(sprintf("- %s %s / %s","data column:",controls[["data"]][["column"]][1],controls[["data"]][["column"]][2]))
      writeLines(sprintf("- %s %s to %s","time horizon:",data[["dates"]][1],rev(data[["dates"]])[1]))
      writeLines(sprintf("- %s %s / %s","data points:",dim(data[["data"]])[1],length(data[["data"]][,-1][!is.na(data[["data"]][,-1])])))
      writeLines(sprintf("- %s %s / %s","log-returns:",controls[["data"]][["log_returns"]][1],controls[["data"]][["log_returns"]][2]))
      writeLines(sprintf("- %s %s","CS transformation:",gsub(" ", "", paste(deparse(controls[["data"]][["cs_transform"]]),collapse=""),fixed = TRUE)))
      if(is.numeric(controls[["horizon"]][2])) writeLines(sprintf("- %s %s","FS dimension:",controls[["horizon"]][2]))
      if(controls[["horizon"]][2]=="w") writeLines(sprintf("- %s %s","FS dimension:","weekly"))
      if(controls[["horizon"]][2]=="m") writeLines(sprintf("- %s %s","FS dimension:","monthly"))
      if(controls[["horizon"]][2]=="q") writeLines(sprintf("- %s %s","FS dimension:","quarterly"))
      if(controls[["horizon"]][2]=="y") writeLines(sprintf("- %s %s","FS dimension:","yearly"))
    }
  }
  
}