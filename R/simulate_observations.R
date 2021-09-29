#' Simulate state-dependent observations.
#' @description 
#' This function simulates state-dependent observations.
#' @param markov_chain
#' A numeric vector of states of a Markov chain.
#' @param sdd
#' The name of the state-dependent distribution, one of \code{"t"} and 
#' \code{"gamma"}.
#' @param mus
#' A vector of expected values.
#' @param sigmas
#' A vector of standard deviations.
#' @param dfs
#' A vector of degrees of freedom (only relevant if \code{sdd = "t"}).
#' @param seed
#' Set a seed.
#' @return
#' A numeric vector of length \code{T} with states.
#' @example
#' Gamma = rbind(c(0.8,0.2),c(0.1,0.9))
#' markov_chain = simulate_markov_chain(Gamma = Gamma, T = 10)
#' simulate_observations(markov_chain = markov_chain, sdd = "t", mus = c(-1,1),
#'                       sigmas = c(0.5,2), dfs = c(1,Inf), seed = 1)
#' @export

simulate_observations = function(markov_chain, sdd, mus, sigmas, dfs = NULL, 
                                 seed = NULL){
  
  ### check inputs
  if(!all(is_number(markov_chain, int = TRUE, pos = TRUE)))
    stop("'markov_chain' must be a numberic vector of states of a Markov chain.")
  if(!(length(sdd) == 1 && sdd %in% c("t","gamma")))
    stop("'sdd' must be one of 't' or 'gamma'.")
  if(!all(is_number(mus)))
     stop("'mus' must be a numberic vector.")
  if(!all(is_number(sigmas, pos = TRUE)))
    stop("'sigmas' must be a positive numeric vector.")
  if(!(length(mus) == length(sigmas)))
    stop("'mus' and 'sigmas' must be of the same length.")
  if(sdd != "t"){
    if(!is.null(dfs))
      stop("'dfs' must only be specified if 'sdd' = 't'.")
  } else {
    if(is.null(dfs))
      stop("'dfs' must be specified if 'sdd' = 't'.") 
    if(!(all(is_number(dfs, pos = TRUE)) && length(dfs) == length(mus)))
      stop("'dfs' must be a positive number vector of length equal to 'mus' and 'sigmas'.")
  }
  
  ### set seed
  if(!is.null(seed))
    set.seed(seed)

  ### simulate observations
  T = length(markov_chain)
  observations = numeric(T)
  for(t in 1:T){
    s = markov_chain[t]
    if(sdd == "t")
      observations[t] = rt(1, dfs[s]) * sigmas[s] + mus[s]
    if(sdd == "gamma")
      observations[t] = rgamma(1, shape = mus[s]^2/sigmas[s]^2, scale = sigmas[s]^2/mus[s])
  }
  
  ### return observations
  return(observations)
}