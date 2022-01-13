#' Prediction
#' @description
#' This function...
#' @param x
#' An object of class \code{fHMM_model}.
#' @param ahead
#' A positive integer.
#' @inheritParams compute_ci
#' @return
#' An object of class \code{fHMM_model}.
#' @export
#' @importFrom stats qt qgamma

predict <- function(x, ahead, ci_level = 0.05) {

  ### check input
  if (class(x) != "fHMM_model") {
    stop("'x' must be of class 'fHMM_model'.")
  }
  if (!(length(ahead) == 1 && is_number(ahead, int = TRUE, pos = TRUE))) {
    stop("'ahead' must be a positive integer")
  }
  if (!(length(ci_level) == 1 && is_number(ci_level, pos = TRUE) && ci_level <= 1)) {
    stop("'ci_level' must be a numeric between 0 and 1.")
  }

  ### extract parameters
  par <- parUncon2par(x$estimate, x$data$controls)
  M <- x$data$controls$states[1]
  N <- x$data$controls$states[2]
  sdds <- x$data$controls$sdds

  ### predict states
  state_prediction <- matrix(NA, nrow = ahead, ncol = M)
  last_state <- tail(if (x$data$controls$hierarchy) x$decoding[, 1] else x$decoding, n = 1)
  state_prob <- replace(numeric(M), last_state, 1)
  for (i in 1:ahead) {
    state_prob <- state_prob %*% par$Gamma
    state_prediction[i, ] <- state_prob
  }
  rownames(state_prediction) <- 1:ahead
  colnames(state_prediction) <- paste("state", 1:M, sep = "_")
  if (x$data$controls$hierarchy) {
    for (s in 1:M) {
      state_prob <- rep(1 / N, N)
      fs_state_prediction <- matrix(NA, nrow = ahead, ncol = N)
      for (i in 1:ahead) {
        state_prob <- state_prob %*% par$Gammas_star[[s]]
        fs_state_prediction[i, ] <- state_prediction[i, s] * state_prob
      }
      rownames(fs_state_prediction) <- 1:ahead
      colnames(fs_state_prediction) <- paste("state", s, 1:M, sep = "_")
      state_prediction <- cbind(state_prediction, fs_state_prediction)
    }
  }

  ### predict data
  data_prediction <- matrix(NA, nrow = ahead, ncol = 3)
  props <- sort(c(ci_level, 0.5, 1 - ci_level))
  if (!x$data$controls$hierarchy) {
    if (sdds[[1]]$name == "t") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, ] %*%
            (stats::qt(p = x, df = par$dfs) * par$sigmas + par$mus)
        })
      }
    }
    if (sdds[[1]]$name == "gamma") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, ] %*%
            stats::qgamma(
              p = x, shape = par$mus^2 / par$sigmas^2,
              scale = par$sigmas^2 / par$mus
            )
        })
      }
    }
  } else {
    if (sdds[[2]]$name == "t") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, -(1:M)] %*%
            (stats::qt(p = x, df = unlist(par$dfs_star)) * unlist(par$sigmas_star) + unlist(par$mus_star))
        })
      }
    }
    if (sdds[[2]]$name == "gamma") {
      for (i in 1:ahead) {
        data_prediction[i, ] <- sapply(props, function(x) {
          state_prediction[i, -(1:M)] %*%
            stats::qgamma(
              p = x, shape = unlist(par$mus_star)^2 / unlist(par$sigmas_star)^2,
              scale = unlist(par$sigmas_star)^2 / unlist(par$mus_star)
            )
        })
      }
    }
  }
  rownames(data_prediction) <- 1:ahead
  colnames(data_prediction) <- c("lb", "estimate", "ub")

  ### return 'fHMM_prediction' object
  prediction <- list("states" = state_prediction, "data" = data_prediction)
  class(prediction) <- "fHMM_predict"
  return(prediction)
}
