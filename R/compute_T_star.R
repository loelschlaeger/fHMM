#' Compute lengths of fine-scale chunks
#' @description  
#' This helper-function computes the lengths of fine-scale chunks.
#' @param horizon 
#' The element \code{controls$horizon}, i.e. an integer vector of length 2,
#' where the second entry can be \code{NA}.
#' @param period
#' The element \code{controls$period}, i.e. one of \code{"w"}, 
#' \code{"m"}, code{"q"}, or \code{"y"}.
#' @param dates
#' A vector of dates of empirical fine-scale data. 
#' @param seed
#' Set a seed for the simulation of flexible chunk lengths.
#' @return 
#' A vector of fine-scale chunk sizes.

compute_T_star = function(horizon, period, dates = NULL, seed = NULL){
  if(is.null(dates)){
    if(!is.null(seed))
      set.seed(seed)
    if(!is.na(horizon[2])){
      T_star = rep(horizon[2], horizon[1])
    } else {
      if(period == "w") 
        size = 5
      if(period == "m") 
        size = 25
      if(period == "q") 
        size = 70
      if(period == "y") 
        size = 260
      T_star = sample(1:size, horizon[1], replace=TRUE, 
                      prob = dbinom(1:size, size, 0.9))
    }
  } else {
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
