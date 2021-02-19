#' Compute (flexible) fine-scale chunk lengths
#' @param fs_time_horizon Setting the fine-scale dimension, either a numeric or one of "w","m","q","y"
#' @param T The dimension of the coarse-scale process, default \code{NULL}
#' @param fs_dates A vector of dates of empirical fine-scale observations, default \code{NULL}
#' @return Vector of fine-scale chunk sizes
compute_fs = function(fs_time_horizon,T=NULL,fs_dates=NULL){
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