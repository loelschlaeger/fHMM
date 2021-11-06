#' Set and check parameters for the fHMM package.
#' @description 
#' This function sets and checks parameters for the fHMM package. 
#' @details 
#' See the vignettes for more information on how to specify parameters.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @param Gamma
#' A tpm of dimension \code{controls$states[1]}.
#' @param mus
#' A vector of expectations of length \code{controls$states[1]}.
#' @param sigmas
#' A vector of standard deviations of length \code{controls$states[1]}.
#' @param dfs
#' A vector of degrees of freedom of length \code{controls$states[1]}.
#' Only relevant if sdd is a t-distribution.
#' @param Gammas_star
#' A list of length \code{controls$states[1]} of tpm's. Each tpm must be of
#' dimension \code{controls$states[2]}.
#' @param mus_star
#' A list of length \code{controls$states[1]} of vectors of expectations. 
#' Each vector must be of length \code{controls$states[2]}. 
#' @param sigmas_star
#' A list of length \code{controls$states[1]} of vectors of standard deviations. 
#' Each vector must be of length \code{controls$states[2]}. 
#' @param dfs_star
#' A list of length \code{controls$states[1]} of vectors of degrees of freedom. 
#' Each vector must be of length \code{controls$states[2]}. 
#' Only relevant if sdd is a t-distribution.
#' @param seed
#' Set a seed for the sampling of parameters.
#' @param scale_par
#' A positive numeric vector of length two, containing scales for sampled 
#' expectations and standard deviations. The first entry is the scale for 
#' \code{mus} and \code{sigmas}, the second entry is the scale for 
#' \code{mus_star} and \code{sigmas_star}. Set an entry to \code{1} for no 
#' scaling.
#' @return 
#' An object of class \code{fHMM_parameters}.
#' @export

fHMM_parameters = function(controls, 
                           Gamma = NULL, mus = NULL, sigmas = NULL, dfs = NULL, 
                           Gammas_star = NULL, mus_star = NULL, 
                           sigmas_star = NULL, dfs_star = NULL, seed = NULL,
                           scale_par = c(1,1)) {
  
  ### set seed
  if(!is.null(seed))
    set.seed(seed)
  
  ### check 'controls' and 'scale_par'
  if(class(controls) != "fHMM_controls")
    stop("'controls' is not of class fHMM_controls.")
  if(!(length(scale_par) == 2 && is_number(scale_par, pos = TRUE)))
    stop("'scale_par' must be a positive numeric vector of length 2.")
  
  ### function that samples tpm's
  sample_tpm = function(dim) {
    Gamma = matrix(runif(dim^2),dim,dim)
    Gamma = Gamma / rowSums(Gamma)
    return(Gamma)
  }
  
  ### extract number of states
  M = controls[["states"]][1] 
  N = controls[["states"]][2]
  
  ### specify missing parameters
  if(is.null(Gamma))
    Gamma = sample_tpm(M)
  if(is.null(mus)){
    if(controls[["sdds"]][[1]]$name == "t")
      mus = runif(M,-1,1)*scale_par[1]
    if(controls[["sdds"]][[1]]$name == "gamma")
      mus = runif(M,0,1)*scale_par[1]
  }
  if(is.null(sigmas))
    sigmas = runif(M,0,1)*scale_par[1]
  if(controls[["sdds"]][[1]]$name == "t"){
    if(is.null(dfs))
      dfs = runif(M,0,30)
  } else {
    dfs = NULL
  }
  if(controls[["hierarchy"]]){
    if(is.null(Gammas_star)){
      Gammas_star = list()
      for(i in 1:M)
        Gammas_star[[i]] = sample_tpm(N)
    }
    if(is.null(mus_star)){
      mus_star = list()
      for(i in 1:M){
        if(controls[["sdds"]][[2]]$name == "t")
          mus_star[[i]] = runif(N,-1,1)*scale_par[2]
        if(controls[["sdds"]][[2]]$name == "gamma")
          mus_star[[i]] = runif(N,0,1)*scale_par[2]
      }
    }
    if(is.null(sigmas_star)){
      sigmas_star = list()
      for(i in 1:M)
        sigmas_star[[i]] = runif(N,0,1)*scale_par[2]
    }
    if(controls[["sdds"]][[2]]$name == "t"){
      if(is.null(dfs_star)){
        dfs_star = list()
        for(i in 1:M)
          dfs_star[[i]] = runif(N,0,30)
      }
    } else {
      dfs_star = NULL
    }
  }
  
  ### set fixed parameters
  if(!is.null(controls[["sdds"]][[1]]$pars$mu))
    mus = rep_len(controls[["sdds"]][[1]]$pars$mu,M)
  if(!is.null(controls[["sdds"]][[1]]$pars$sigma))
    sigmas = rep_len(controls[["sdds"]][[1]]$pars$sigma,M)
  if(controls[["sdds"]][[1]]$name == "t")
    if(!is.null(controls[["sdds"]][[1]]$pars$df))
      dfs = rep_len(controls[["sdds"]][[1]]$pars$df,M)
  if(controls[["hierarchy"]]){
    if(!is.null(controls[["sdds"]][[2]]$pars$mu))
      mus_star = rep(list(rep_len(controls[["sdds"]][[2]]$pars$mu,N)),M)
    if(!is.null(controls[["sdds"]][[2]]$pars$sigma))
      sigmas_star = rep(list(rep_len(controls[["sdds"]][[2]]$pars$sigma,N)),M)
    if(controls[["sdds"]][[2]]$name == "t")
      if(!is.null(controls[["sdds"]][[2]]$pars$df))
        dfs_star = rep(list(rep_len(controls[["sdds"]][[2]]$pars$df,N)),M)
  }
    
  ### check parameters
  if(!is_tpm(Gamma) || nrow(Gamma) != M)
    stop("'Gamma' must be a tpm of dimension 'controls$states[1]'.")
  if(controls[["sdds"]][[1]]$name == "t")
    if(!all(is_number(mus)) || length(mus) != M)
      stop("'mu' must be a numeric vector of length 'controls$states[1]'.")
  if(controls[["sdds"]][[1]]$name == "gamma")
    if(!all(is_number(mus, pos = TRUE)) || length(mus) != M)
      stop("'mu' must be a positive numeric vector of length 'controls$states[1]'.")
  if(!all(is_number(sigmas, pos = TRUE)) || length(sigmas) != M)
    stop("'sigma' must be a positive numeric vector of length 'controls$states[1]'.")
  if(controls[["sdds"]][[1]]$name == "t")
    if(!all(is_number(dfs, pos = TRUE)) || length(dfs) != M)
      stop("'dfs' must be a positive numeric vector of length 'controls$states[1]'.")
  if(controls[["hierarchy"]]){
    if(!is.list(Gammas_star) || length(Gammas_star) != M)
      stop("'Gammas_star' must be a list of length 'controls$states[1]'.")
    for(i in 1:M)
      if(!is_tpm(Gammas_star[[i]]) || nrow(Gammas_star[[i]]) != N)
        stop("Each element in 'Gammas_star' must be a tpm of dimension 'controls$states[2]'.")
    if(!is.list(mus_star) || length(mus_star) != M)
      stop("'mus_star' must be a list of length 'controls$states[1]'.")
    for(i in 1:M){
      if(controls[["sdds"]][[2]]$name == "t")
        if(!all(is_number(mus_star[[i]])) || length(mus_star[[i]]) != N)
          stop("Each element in 'mus_star' must be a numeric vector of length 'controls$states[2]'.")
      if(controls[["sdds"]][[2]]$name == "gamma")
        if(!all(is_number(mus_star[[i]])) || length(mus_star[[i]]) != N)
          stop("Each element in 'mus_star' must be a numeric vector of length 'controls$states[2]'.")
    }
    if(!is.list(sigmas_star) || length(sigmas_star) != M)
      stop("'sigmas_star' must be a list of length 'controls$states[1]'.")
    for(i in 1:M)
      if(!all(is_number(sigmas_star[[i]], pos = TRUE)) || length(sigmas_star[[i]]) != N)
        stop("Each element in 'sigmas_star' must be a positive numeric vector of length 'controls$states[2]'.")
    if(controls[["sdds"]][[2]]$name == "t"){
      if(!is.list(dfs_star) || length(dfs_star) != M)
        stop("'dfs_star' must be a list of length 'controls$states[1]'.")
      if(!all(is_number(dfs_star[[i]], pos = TRUE)) || length(dfs_star[[i]]) != N)
        stop("Each element in 'dfs_star' must be a positive numeric vector of length 'controls$states[2]'.")
    }
  }
  
  ### build 'fHMM_parameters'
  out = list(
    "Gamma" = Gamma, 
    "mus" = mus, 
    "sigmas" = sigmas, 
    "dfs" = dfs, 
    "sdds" = controls[["sdds"]],
    "Gammas_star" = if(controls[["hierarchy"]]) Gammas_star else NULL, 
    "mus_star" = if(controls[["hierarchy"]]) mus_star else NULL, 
    "sigmas_star" = if(controls[["hierarchy"]]) sigmas_star else NULL, 
    "dfs_star" = if(controls[["hierarchy"]])dfs_star else NULL
  )
  class(out) = "fHMM_parameters"
  return(out)
}