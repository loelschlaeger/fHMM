#' Compute lengths of fine-scale chunks
#' @description  
#' This helper-function computes the lengths of fine-scale chunks.
#' @param horizon 
#' The element \code{controls$horizon}, i.e. an integer vector of length 2,
#' where alternatively the second entry can be one of \code{"w"}, \code{"m"},
#' \code{"q"}, or \code{"y"}.
#' @param dates
#' A vector of dates of empirical fine-scale data. 
#' @param seed
#' Set a seed for the simulation of flexible chunk lengths.
#' @return 
#' A vector of fine-scale chunk sizes.

compute_T_star = function(horizon, dates = NULL, seed = NULL){
  if(is.null(dates)){
    if(!is.null(seed))
      set.seed(seed)
    if(is.numeric(horizon[2]))
      T_star = rep(horizon[2], horizon[1])
    if(horizon[2] %in% c("w","m","q","y")){
      if(horizon[2] == "w") 
        size = 5
      if(horizon[2] == "m") 
        size = 25
      if(horizon[2] == "q") 
        size = 70
      if(horizon[2] == "y") 
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
