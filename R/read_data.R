#' Read data for the fHMM package.
#' @description 
#' This function reads financial data for the fHMM package.
#' @inheritParams prepare_data
#' @return 
#' A list containing the following elements:
#' \itemize{
#'  \item the matrix of the \code{dates},
#'  \item the matrix of the simulated \code{data},
#'  \item the vector of fine-scale chunk sizes \code{T_star}.
#' }

read_data = function(controls){
  
  ### check inputs
  if(class(controls) != "fHMM_controls")
    stop("Not of class 'fHMM_controls'.")
  
  ### read data
  data_raw = list()
  for(i in 1:ifelse(controls[["hierarchy"]],2,1))
    data_raw[[i]] = read.csv(file = controls[["data"]][["file"]][i],
                             header = TRUE, sep = ",", na.strings = "null")
  
  
  ### check columns in data
  column = controls[["data"]][["column"]][i]
  for(i in 1:ifelse(controls[["hierarchy"]],2,1)){
    if(!"Date" %in% colnames(data_raw[[i]]) || !column[i] %in% colnames(data_raw[[i]]))
      stop("D.4")
    data_raw[[i]] = data_raw[[i]][,colnames(data_raw[[i]]) %in% c("Date",column[i]), drop = FALSE]
    data_raw[[i]][["Date"]] = as.Date(data_raw[[i]][["Date"]], format="%Y-%m-%d")
    data_raw[[i]][[column[i]]] = as.numeric(data_raw[[i]][[column[i]]]) 
  }
  
  ### remove NA dates
  for(i in 1:ifelse(controls[["hierarchy"]],2,1))
    data_raw[[i]] = data_raw[[i]][!is.na(data_raw[[i]][["Date"]]),]
      
  ### replace NA values by neighbor means
  for(i in 1:ifelse(controls[["hierarchy"]],2,1)){
    for(na_value in which(is.na(data_raw[[i]][[column[i]]]))){
      incr = 1
      while(TRUE){
        range = unique(abs(c((na_value-incr):(na_value-1),(na_value+1):(na_value+incr))))
        replace = mean(data_raw[[i]][[column[i]]][range],na.rm=TRUE)
        if(!is.nan(replace)){
          data_raw[[i]][[column[i]]][na_value] = replace
          break
        }
        incr = incr + 1
      }
    }
  }
  
  ### compute log-returns
  for(i in 1:ifelse(controls[["hierarchy"]],2,1)){
    if(controls[["data"]][["logreturns"]][i]){
      data_length = length(data_raw[[i]][[column[i]]])
      data_raw[[i]][["logreturns"]] = numeric(data_length)
      for(t in seq_len(data_length)[-1])
        data_raw[[i]][["logreturns"]][t] = log(data_raw[[i]][[column[i]]][t]/data_raw[[i]][[column[i]]][t-1])
      
      ### remove 0 log-returns in case of gamma sdd to avoid numerical conflicts
      if(controls[["sdds"]][[i]]$name == "gamma"){
        for(t in seq_len(data_length)){
          if(data_raw[[i]][["logreturns"]][t] == 0){
            step = 1
            cand = 0
            while(cand == 0){
              cand = mean(data_raw[[i]][["logreturns"]][abs((t-step):(t+step))],na.rm=TRUE)
              step = step + 1
            }
            data_raw[[i]][["logreturns"]][t] = cand
          }
        }
      }
    }
  }
  
  ### remove data points that do not occur in both files
  if(controls[["hierarchy"]]){
    data_raw[[1]] = data_raw[[1]][ data_raw[[1]][["Date"]] %in% intersect(data_raw[[1]][["Date"]],data_raw[[2]][["Date"]]), ]
    data_raw[[2]] = data_raw[[2]][ data_raw[[2]][["Date"]] %in% intersect(data_raw[[2]][["Date"]],data_raw[[1]][["Date"]]), ]
  }
  
  ### truncate data
  for(i in 1:ifelse(controls[["hierarchy"]],2,1)){
    ### find exact or nearest position of 'date' in 'data' 
    find_date = function(date, data){
      incr = 0
      while(TRUE){
        candidate = which(data[["Date"]] == as.Date(date)+incr)
        if(length(candidate)==1) return(candidate)
        candidate = which(data[["Date"]] == as.Date(date)-incr)
        if(length(candidate)==1) return(candidate)
        incr = incr + 1
      }
    }
    t_max = controls[["data"]][["to"]]
    if(!is.na(t_max)){
      data_raw[[i]] = data_raw[[i]][seq_len(find_date(t_max, data_raw[[i]])),]
    }
    t_min = controls[["data"]][["from"]]
    if(!is.na(t_min)){
      temp = seq_len(find_date(t_min,data_raw[[i]])-1)
      if(length(temp) > 0) 
        data_raw[[i]] = data_raw[[i]][-temp,]
    }
  }
  
  ### compute 'T_star' and merge coarse-scale data
  if(controls[["hierarchy"]]){
    T_star = compute_T_star(horizon = controls[["horizon"]], 
                            period = controls[["period"]],
                            dates = data_raw[[2]][["Date"]])
    T = length(T_star)
    for(i in 1:2)
      data_raw[[i]] = data_raw[[i]][seq_len(sum(T_star)),]
    cs_data_tbt = matrix(NA,nrow=T,ncol=max(T_star))
    fs_data     = matrix(NA,nrow=T,ncol=max(T_star))
    for(t in seq_len(T)){
      cs_data_tbt[t,] = c(data_raw[[1]][[if(controls[["data"]][["log_returns"]][1]) "LogReturns" else data_col[1]]][(sum(T_star[seq_len(t-1)])+1):sum(T_star[seq_len(t)])],rep(NA,max(T_star)-T_star[t]))
      fs_data[t,]     = c(data_raw[[2]][[if(controls[["data"]][["log_returns"]][2]) "LogReturns" else data_col[2]]][(sum(T_star[seq_len(t-1)])+1):sum(T_star[seq_len(t)])],rep(NA,max(T_star)-T_star[t]))
    }
    f = controls[["data"]][["cs_transform"]]
    cs_data = apply(cs_data_tbt,1,function(x) return(f(x[!is.na(x)]))) 
  }
  
  ### return
  out = list(
    "dates" = NA,
    "data" = NA,
    "ts" = NA,
    "T_star"   = if(controls[["hierarchy"]]) T_star else NA
  )
  return(out)
}