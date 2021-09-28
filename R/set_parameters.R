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
#' @return 
#' An object of class \code{fHMM_parameters}.
#' @export

set_parameters = function(controls, 
                          Gamma = NULL, mus = NULL, sigmas = NULL, dfs = NULL, 
                          Gammas_star = NULL, mus_star = NULL, 
                          sigmas_star = NULL, dfs_star = NULL) {
  
  ### check 'controls'
  if(class(controls) != "fHMM_controls")
    stop("not of class fHMM_controls.")
  
  ### function that samples tpm's
  sample_tpm = function(dim) {
    Gamma = matrix(runif(dim^2),dim,dim)
    Gamma = Gamma / rowSums(Gamma)
    return(Gamma)
  }
  
  ### specify missing parameters
  M = controls[["states"]][1] 
  if(is.null(Gamma))
    Gamma = sample_tpm(M)
  if(is.null(mus))
    mus = runif(M,-1,1)
  if(is.null(sigmas))
    sigmas = runif(M,0,1)
  if(is.null(dfs))
    dfs = runif(M,0,30)
  if(controls[["model"]] == "hhmm"){
    N  = controls[["states"]][2]
    if(is.null(Gammas_star)){
      Gammas_star = list()
      for(i in 1:M)
        Gammas_star[[i]] = sample_tpm(N)
    }
    if(is.null(mus_star)){
      mus_star = list()
      for(i in 1:M)
        mus_star[[i]] = runif(N,-1,1)
    }
    if(is.null(sigmas_star)){
      sigmas_star = list()
      for(i in 1:M)
        sigmas_star[[i]] = runif(M,0,1)
    }
    if(is.null(dfs_star)){
      dfs_star = list()
      for(i in 1:M)
        dfs_star[[i]] = runif(M,0,30)
    }
  }
    
  ### check parameters

  ### build 'fHMM_parameters'
  out = list(
    "Gamma" = Gamma, 
    "mus" = mus, 
    "sigmas" = sigmas, 
    "dfs" = dfs, 
    "Gammas_star" = Gammas_star, 
    "mus_star" = mus_star, 
    "sigmas_star" = sigmas_star, 
    "dfs_star" = dfs_star
  )
  class(out) = "fHMM_parameters"
  return(out)
}