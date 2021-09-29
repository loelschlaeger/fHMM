#' Simulate a Markov chain.
#' @description 
#' This function simulates a Markov chain.
#' @param Gamma
#' A transition probability matrix.
#' @param T
#' The length of the Markov chain.
#' @param delta
#' A probability vector, the initial distribution. If not specified, \code{delta}
#' is set to the stationary distribution vector.
#' @param seed
#' Set a seed.
#' @return
#' A numeric vector of length \code{T} with states.
#' @example
#' Gamma = rbind(c(0.8,0.2),c(0.1,0.9))
#' simulate_markov_chain(Gamma = Gamma, T = 10)
#' @export

simulate_markov_chain = function(Gamma, T, delta = Gamma2delta(Gamma), seed = NULL){
  
  ### input checks
  if(!(is.matrix(Gamma) && all(rowSums(Gamma) == 1) && ncol(Gamma) == nrow(Gamma)))
    stop("Gamma must be a transition probability matrix.")
  if(!(length(T) == 1 && is_number(T, int = TRUE, pos = TRUE)))
    stop("'T' must be a positive number.")
  if(!(is.numeric(delta) && length(delta) == nrow(Gamma)))
    stop("'delta' must be a numberic vector of length equal to the dimension of 'Gamma'.")
  
  ### set a seed
  if(!is.null(seed))
    set.seed(seed)
  
  ### simulate Markov chain 
  N = length(delta)
  markov_chain = numeric(T)
  markov_chain[1] = sample(1:N, 1, prob = delta)
  for(t in 2:T)
    markov_chain[t] = sample(1:N, 1 ,prob = Gamma[markov_chain[t-1],])
  
  ### return Markov chain
  return(markov_chain)
}
