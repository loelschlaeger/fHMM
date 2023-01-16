#' Check date format
#'
#' @description
#' This function checks if the input \code{date} has the format 
#' \code{"YYYY-MM-DD"}.
#'
#' @param date
#' A \code{character}, specifying a date in format \code{"YYYY-MM-DD"}.
#'
#' @return
#' \code{as.Date(date)} if \code{date} has the format \code{"YYYY-MM-DD"}.
#' Otherwise, the function throws an error.
#'
#' @keywords internal utils
#'
#' @examples
#' \dontrun{
#' check_date(date = "2000-01-01")
#' }

check_date <- function(date) {
  date <- try(as.Date(date), silent = TRUE)
  if (inherits(date,"try-error") || anyNA(as.Date(date, format = "%Y-%m-%d"))) {
    stop("Date not in required format 'YYYY-MM-DD'.",
         call. = FALSE)
  }
  return(date)
}

#' Check for integers
#'
#' @description
#' This function checks if \code{x} is a ((non)-negative) ((non-)positive)
#' (integer) numeric (vector).
#'
#' @details 
#' The function is vectorized.
#'
#' @param x
#' Any R object.
#' @param int
#' A \code{logical}, if \code{TRUE} checks if \code{x} is an integer.
#' @param neg
#' A \code{logical}, if \code{TRUE} checks if \code{x} is negative.
#' @param non_neg
#' A \code{logical}, if \code{TRUE} checks if \code{x} is non-negative.
#' @param pos
#' A \code{logical}, if \code{TRUE} checks if \code{x} is positive.
#' @param non_pos
#' A \code{logical}, if \code{TRUE} checks if \code{x} is non-positive.
#'
#' @return
#' A \code{logical}.
#'
#' @keywords internal utils
#'
#' @examples
#' \dontrun{
#' is_number(1, int = TRUE)
#' is_number(pi, int = TRUE)
#' }

is_number <- function(
    x, int = FALSE, neg = FALSE, non_neg = FALSE, pos = FALSE, non_pos = FALSE
  ) {
  if (length(x) == 0) {
    return(FALSE)
  }
  out <- rep(TRUE, length(x))
  for (i in 1:length(x)) {
    if (!is.numeric(x[i])) {
      out[i] <- FALSE
    } else {
      if (int) {
        if (x[i] %% 1 != 0) {
          out[i] <- FALSE
        }
      }
      if (neg) {
        if (!x[i] < 0) {
          out[i] <- FALSE
        }
      }
      if (non_neg) {
        if (!x[i] >= 0) {
          out[i] <- FALSE
        }
      }
      if (pos) {
        if (!x[i] > 0) {
          out[i] <- FALSE
        }
      }
      if (non_pos) {
        if (!x[i] <= 0) {
          out[i] <- FALSE
        }
      }
    }
  }
  return(out)
}

#' Check for transition probability matrix
#'
#' @description
#' This function checks if \code{x} is a tpm (transition probability matrix).
#'
#' @param x
#' A \code{matrix}.
#'
#' @return
#' Either \code{TRUE} or \code{FALSE}.
#'
#' @keywords internal utils
#'
#' @examples
#' \dontrun{
#' is_tpm(diag(2))
#' is_tpm(matrix(1, 2, 2))
#' }

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
#' A \code{numeric} vector.
#' @param y
#' Another \code{numeric} vector of the same length as \code{x}.
#'
#' @return
#' An \code{integer} vector of length \code{length(x)} with the positions of \code{y}
#' in \code{x}.
#'
#' @keywords internal utils
#'
#' @importFrom stats dist
#'
#' @examples
#' \dontrun{
#' x <- c(-1, 0, 1)
#' y <- c(0.1, 2, -1.2)
#' match_all(x = x, y = y)
#' }

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
              x[i_x] <- NA_integer_
              y[i_y] <- NA_integer_
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
#' An \code{integer}, the matrix dimension.
#'
#' @return
#' A transition probability \code{matrix}.
#'
#' @keywords internal utils
#'
#' @importFrom stats runif
#'
#' @examples
#' \dontrun{
#' sample_tpm(dim = 3)
#' }

sample_tpm <- function(dim) {
  Gamma <- matrix(stats::runif(dim^2), dim, dim)
  Gamma <- Gamma / rowSums(Gamma)
  return(Gamma)
}

#' Simulate Markov chain
#'
#' @description
#' This function simulates a Markov chain.
#'
#' @param Gamma
#' A transition probability \code{matrix} (tpm).
#' @param T
#' AN \code{integer}, the length of the Markov chain.
#' @param delta
#' A \code{numeric} probability vector, the initial distribution. 
#' If not specified, \code{delta} is set to the stationary distribution vector.
#' @param seed
#' Set a seed.
#' @param total_length
#' An \code{integer}, the total length of the output vector. 
#' Must be greater or equal than \code{T}.
#'
#' @return
#' A \code{numeric} vector of length \code{T} with states.
#'
#' @keywords internal utils
#'
#' @examples
#' \dontrun{
#' Gamma <- matrix(c(0.5, 0.3, 0.5, 0.7), 2, 2)
#' T <- 10
#' simulate_markov_chain(Gamma = Gamma, T = T)
#' }

simulate_markov_chain <- function(Gamma, T, delta = Gamma2delta(Gamma),
                                  seed = NULL, total_length = T) {

  ### input checks
  if (!(is.matrix(Gamma) && all(rowSums(Gamma) == 1) && ncol(Gamma) == nrow(Gamma))) {
    stop("Gamma must be a transition probability matrix.", call. = FALSE)
  }
  if (!(length(T) == 1 && is_number(T, int = TRUE, pos = TRUE))) {
    stop("'T' must be a positive number.", call. = FALSE)
  }
  if (!(is.numeric(delta) && length(delta) == nrow(Gamma))) {
    stop("'delta' must be a numberic vector of length equal to the dimension of 'Gamma'.", call. = FALSE)
  }
  if (!is_number(total_length) || length(total_length) != 1 || total_length < T) {
    stop("'total_length' must be an integer greater or equal than 'T'.", call. = FALSE)
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
  markov_chain <- c(markov_chain, rep(NA_integer_, total_length - T))

  ### return Markov chain
  return(markov_chain)
}
