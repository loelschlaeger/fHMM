#' Read financial data from .csv-file
#' @param controls A list of controls
#' @return A list containing the following elements:
#' \item{logReturns}{Log-returns, the data that is modeled}
#' \item{dataRaw}{Raw data}
#' \item{dataRaw_cs}{Raw data on coarse-scale}
#' \item{dates}{Vector of dates}
#' \item{T_star}{Vector of fine-scale chunk sizes}
read_data = function(controls){
  if(is.null(controls[["controls_checked"]])) stop(sprintf("%s (%s)",exception("F.6")[2],exception("F.6")[1]),call.=FALSE)
  data_source = controls[["data_source"]]
  data_col = controls[["data_col"]]
  
  data = list()
  for(i in 1:2){
    if(is.na(data_source)[i]){
      data[[i]] = NA
    }
    if(!is.na(data_source)[i]){
      ### extract data
      data[[i]] = read.csv(file=paste0("data/",data_source[i]),header=TRUE,sep=",",na.strings="null")
      if(!"Date" %in% colnames(data[[i]]) || !data_col[i] %in% colnames(data[[i]])){
        stop(sprintf("%s (%s)",exception("D.4")[2],exception("D.4")[1]),call.=FALSE)
      }
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
      
      ### remove 0 log-returns in case of gamma sdd to avoid numerical conflicts
      if(controls[["sdds"]][i]=="gamma"){
        for(t in seq_len(data_length)){
          if(data[[i]][["LogReturns"]][t]==0){
            step = 1
            cand = 0
            while(cand==0){
              cand = mean(data[[i]][["LogReturns"]][abs((t-step):(t+step))],na.rm=TRUE)
              step = step + 1
            }
            data[[i]][["LogReturns"]][t] = cand
          }
        }
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
    
    T_star = compute_fs(fs_time_horizon = controls[["time_horizon"]][2], fs_dates = data[[2]][["Date"]])
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
      cs_data = rowMeans(cs_data_tbt,na.rm=TRUE)
    }
    if(controls[["data_cs_type"]] == "mean_abs"){
      cs_data = rowMeans(abs(cs_data_tbt),na.rm=TRUE)
    }
    if(controls[["data_cs_type"]] == "sum_abs"){
      cs_data = rowSums(abs(cs_data_tbt),na.rm=TRUE)
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