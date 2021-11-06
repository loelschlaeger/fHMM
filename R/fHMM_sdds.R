#' Define state-dependent distributions.
#' @description 
#' This function defines state-dependent distributions. 
#' @param sdds
#' A character or a character vector of length two that can be specified for 
#' \code{"sdds"} in \code{link{set_controls}}.
#' @return
#' A list of length \code{length(sdds)}. Each element is a list, containing
#' \itemize{
#'   \item the \code{"name"} of the distribution 
#'   \item and a list \code{"pars"} of its parameters.
#' }
#' Unknown parameters are set to \code{NULL}.

fHMM_sdds = function(sdds) {
  out = list()
  for(sdd in sdds){
    sdd_tws = gsub(" ", "", sdd)
    sdd_tws_split = unlist(strsplit(sdd_tws, split = "[()]"))
    distr = sdd_tws_split[1]
    if(!distr %in% c("t","gamma"))
      stop("'distr' must be one of 't' or 'gamma'.")
    if(is.na(sdd_tws_split[2])){
      pars = NULL
    } else {
      pars = strsplit(strsplit(sdd_tws_split[2], split = c(","))[[1]],"=")
    }
    for(par in pars)
      if(!par[1] %in% c("mu","sigma","df"))
        pars[which(lapply(pars,function(x)x[1]) == par[1])] = NULL
    names = unlist(lapply(pars, `[[`, 1))
    pars = lapply(pars, 
                  function(x) as.numeric(unlist(strsplit(x[2], split = "|", fixed = TRUE))))
    names(pars) = names
    if(distr == "t")
     pars[!names(pars) %in% c("mu","sigma","df")] = NULL
    if(distr == "gamma")
      pars[!names(pars) %in% c("mu","sigma","df")] = NULL
    if(!is.null(pars$mu)){ 
      if(distr == "t")
        if(!any(is_number(pars$mu)))
          stop("'mu' must be a numeric.")
      if(distr == "gamma")
        if(!any(is_number(pars$mu, pos = TRUE)))
          stop("'mu' must be a positive numeric.")
    } else {
      pars$mu = NULL
    }
    if(!is.null(pars$sigma)){
      if(!any(is_number(pars$sigma, pos = TRUE)))
        stop("'sigma' must be a positive numeric.")
    } else {
      pars$sigma = NULL
    }
    if(!is.null(pars$df)){
      if(!any(is_number(pars$df, pos = TRUE)))
        stop("'df' must be a positive numeric.")
    } else if(distr == "t"){
      pars$df = NULL
    }
    out[[length(out)+1]] = list("name" = distr, pars = pars)
  }
  class(out) = "fHMM_sdds"
  return(out)
}
