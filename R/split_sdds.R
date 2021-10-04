#' Split fixed parameters from state-dependent distribution.
#' @description 
#' This is a helper function to split fixed parameters from a state-dependent
#' distribution in \code{controls}.
#' @param sdds
#' A character (vector) that can be specified in \code{controls$sdds}.
#' @param hierarchy
#' The element \code{controls$hierarchy}.
#' @return
#' A list with the \code{"name"} of the distribution and a list of \code{fixed}
#' parameters.
#' @examples 
#' split_sdds(sdds = "t")
#' split_sdds(sdds = "gamma")
#' split_sdds(sdds = c("t(mu = 1, sigma = 2, df = 3)","gamma(sigma = 1)"))

split_sdds = function(sdds) {
  out = list()
  for(sdd in sdds){
    sdd_tws = gsub(" ", "", sdd)
    sdd_tws_split = unlist(strsplit(sdd_tws, split = "[()]"))
    distr = sdd_tws_split[1]
    if(!distr %in% c("t","gamma"))
      stop("'distr' must be one of 't' or 'gamma'.")
    fixed_pars = strsplit(strsplit(sdd_tws_split[2], split = c(","))[[1]],"=")
    for(par in fixed_pars)
      if(!par[1] %in% c("mu","sigma","df"))
        fixed_pars[which(lapply(fixed_pars,function(x)x[1]) == par[1])] = NULL
    if(sum(!is.na(unlist(fixed_pars))) == 0) {
      fixed = list()
    } else {
      fixed = lapply(fixed_pars, function(x) as.numeric(x[[2]]))
      names(fixed) = unlist(lapply(fixed_pars, `[[`, 1))
      if(distr == "t")
       fixed[!names(fixed) %in% c("mu","sigma","df")] = NULL
      if(distr == "gamma")
        fixed[!names(fixed) %in% c("mu","sigma","df")] = NULL
      if(!is.null(fixed$mu)){ 
        if(distr == "t")
          if(!any(is_number(fixed$mu)))
            stop("'mu' must be a numeric.")
        if(distr == "gamma")
          if(!any(is_number(fixed$mu, pos = TRUE)))
            stop("'mu' must be a positive numeric.")
      }
      if(!is.null(fixed$sigma)) 
        if(!any(is_number(fixed$sigma, pos = TRUE)))
          stop("'sigma' must be a positive numeric.")
      if(!is.null(fixed$df)) 
        if(!any(is_number(fixed$df, pos = TRUE)))
          stop("'df' must be a positive numeric.")
    }
    out[[length(out)+1]] = list("name" = distr, fixed = fixed)
  }
  class(out) = "fHMM_sdds"
  return(out)
}
