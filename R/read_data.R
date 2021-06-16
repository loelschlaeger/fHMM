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
    stop(sprintf("%s (%s)",exception("F.6")[2],exception("F.6")[1]),call.=FALSE)
  
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
      if(!"Date" %in% colnames(data_raw[[i]]) || !data_col[i] %in% colnames(data_raw[[i]])){
        stop(sprintf("%s (%s)",exception("D.4")[2],exception("D.4")[1]),call.=FALSE)
      }
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