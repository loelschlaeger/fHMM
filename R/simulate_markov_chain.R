#' Simulate a Markov chain
#' 
#' @description
#' This function simulates a Markov chain.
#' 
#' @param Gamma
#' A tpm (transition probability matrix).
#' @param T
#' The length of the Markov chain.
#' @param delta
#' A probability vector, the initial distribution. If not specified, \code{delta}
#' is set to the stationary distribution vector.
#' @param seed
#' Set a seed.
#' @param total_length
#' An integer, the total length of the output vector. Must be greater or equal
#' than \code{T}.
#' 
#' @return
#' A numeric vector of length \code{T} with states.
#' 
#' @keywords
#' utils
#' 
#' @examples
#' Gamma <- matrix(c(0.5,0.3,0.5,0.7),2,2)
#' T <- 10
#' simulate_markov_chain(Gamma = Gamma, T = T)
#' 
#' @export

simulate_markov_chain <- function(Gamma, T, delta = Gamma2delta(Gamma),
                                  seed = NULL, total_length = T) {

  ### input checks
  if (!(is.matrix(Gamma) && all(rowSums(Gamma) == 1) && ncol(Gamma) == nrow(Gamma))) {
    stop("Gamma must be a transition probability matrix.")
  }
  if (!(length(T) == 1 && is_number(T, int = TRUE, pos = TRUE))) {
    stop("'T' must be a positive number.")
  }
  if (!(is.numeric(delta) && length(delta) == nrow(Gamma))) {
    stop("'delta' must be a numberic vector of length equal to the dimension of 'Gamma'.")
  }
  if (!is_number(total_length) || length(total_length) != 1 || total_length < T) {
    stop("'total_length' must be an integer greater or equal than 'T'.")
  }

  ### set a seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### simulate Markov chain
  N <- length(delta)
  markov_chain <- numeric(T)
  markov_chain[1] <- sample(1:N, 1, prob = delta)
  for (t in 2:T) {
    markov_chain[t] <- sample(1:N, 1, prob = Gamma[markov_chain[t - 1], ])
  }

  ### append NA's
  markov_chain <- c(markov_chain, rep(NA, total_length - T))

  ### return Markov chain
  return(markov_chain)
}
