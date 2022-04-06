#' Check date format "YYYY-MM-DD"
#'
#' @description
#' This function checks if the input \code{date} has the format "YYYY-MM-DD".
#'
#' @param date
#' A character, specifying a date in format "YYYY-MM-DD".
#'
#' @return
#' \code{as.Date(date)} if \code{date} has the format "YYYY-MM-DD".
#' Otherwise, the function throws an error.
#'
#' @keywords
#' utils
#'
#' @examples
#' fHMM:::check_date(date = "2000-01-01")
check_date <- function(date) {
  date <- try(as.Date(date), silent = TRUE)
  if (inherits(date,"try-error") || any(is.na(as.Date(date, format = "%Y-%m-%d")))) {
    stop("Date not in required format 'YYYY-MM-DD'.")
  }
  return(date)
}

#' Check for integers
#'
#' @description
#' This function checks if \code{x} is a ((non)-negative) ((non-)positive)
#' (integer) numeric (vector).
#'
#' @param x
#' An R object.
#' @param int
#' A boolean, if \code{TRUE} checks if \code{x} is an integer.
#' @param neg
#' A boolean, if \code{TRUE} checks if \code{x} is negative.
#' @param non_neg
#' A boolean, if \code{TRUE} checks if \code{x} is non-negative.
#' @param pos
#' A boolean, if \code{TRUE} checks if \code{x} is positive.
#' @param non_pos
#' A boolean, if \code{TRUE} checks if \code{x} is non-positive.
#'
#' @return
#' A boolean.
#'
#' @keywords
#' utils
#'
#' @examples
#' fHMM:::is_number(1, int = TRUE)
#' fHMM:::is_number(pi, int = TRUE)
is_number <- function(x, int = FALSE, neg = FALSE, non_neg = FALSE, pos = FALSE,
                      non_pos = FALSE) {
  if (length(x) == 0) {
    return(TRUE)
  }
  for (i in 1:length(x)) {
    out <- rep(TRUE, length(x))
    if (!is.numeric(x[i])) {
      out[i] <- FALSE
      break
    }
    if (int) {
      if (x[i] %% 1 != 0) {
        out[i] <- FALSE
        break
      }
    }
    if (neg) {
      if (!x[i] < 0) {
        out[i] <- FALSE
        break
      }
    }
    if (non_neg) {
      if (!x[i] >= 0) {
        out[i] <- FALSE
        break
      }
    }
    if (pos) {
      if (!x[i] > 0) {
        out[i] <- FALSE
        break
      }
    }
    if (non_pos) {
      if (!x[i] <= 0) {
        out[i] <- FALSE
        break
      }
    }
  }
  return(out)
}

#' Check for tpm
#'
#' @description
#' This function checks if \code{x} is a tpm (transition probability matrix).
#'
#' @param x
#' A matrix.
#'
#' @return
#' A boolean.
#'
#' @keywords
#' utils
#'
#' @examples
#' fHMM:::is_tpm(diag(2))
#' fHMM:::is_tpm(matrix(1, 2, 2))
is_tpm <- function(x) {
  if (nrow(x) != ncol(x) ||
    any(abs(rowSums(x) - 1) > .Machine$double.eps) ||
    any(x < 0)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

#' Best-possible match of two numeric vectors
#'
#' @description
#' This function matches the positions of two numeric vectors as good as possible.
#'
#' @param x
#' A numeric vector.
#' @param y
#' Another numeric vector of the same length as \code{x}.
#'
#' @return
#' An integer vector of length \code{length(x)} with the positions of \code{y}
#' in \code{x}.
#'
#' @keywords
#' utils
#'
#' @importFrom stats dist
#'
#' @examples
#' x <- c(-1, 0, 1)
#' y <- c(0.1, 2, -1.2)
#' fHMM:::match_all(x = x, y = y)
match_all <- function(x, y) {
  stopifnot(length(x) == length(y))
  matches <- numeric(length(x))
  distances <- unique(sort(stats::dist(c(x, y)))) + sqrt(.Machine$double.eps)
  for (d in distances) {
    if (any(c(!is.na(x), !is.na(y)))) {
      for (i_x in 1:length(x)) {
        for (i_y in 1:length(y)) {
          if (!is.na(x[i_x]) && !is.na(y[i_y])) {
            if (isTRUE(all.equal(x[i_x], y[i_y], d))) {
              matches[i_y] <- i_x
              x[i_x] <- NA
              y[i_y] <- NA
            }
          }
        }
      }
    }
  }
  return(matches)
}

#' Sample transition probability matrices
#'
#' @description
#' This function returns a random, squared matrix of dimension \code{dim}
#' that fulfills the properties of a transition probability matrix.
#'
#' @param dim
#' The matrix dimension.
#'
#' @return
#' A transition probability matrix.
#'
#' @keywords
#' utils
#'
#' @importFrom stats runif
#'
#' @examples
#' fHMM:::sample_tpm(dim = 3)
sample_tpm <- function(dim) {
  Gamma <- matrix(stats::runif(dim^2), dim, dim)
  Gamma <- Gamma / rowSums(Gamma)
  return(Gamma)
}

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
#' Gamma <- matrix(c(0.5, 0.3, 0.5, 0.7), 2, 2)
#' T <- 10
#' fHMM:::simulate_markov_chain(Gamma = Gamma, T = T)
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
