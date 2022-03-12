#' Prediction
#'
#' @description
#' This function predicts the next \code{ahead} states and data points based on
#' an \code{fHMM_model} object.
#'
#' @param object
#' An object of class \code{fHMM_model}.
#' @param ahead
#' A positive integer, the forecast horizon.
#' @inheritParams compute_ci
#' @param ...
#' Ignored.
#'
#' @return
#' An data frame of state probabilities and data point estimates along with
#' confidence intervals.
#'
#' @examples
#' data(dax_model_3t)
#' predict(dax_model_3t)
#' @export
#' @importFrom stats qt qgamma

predict.fHMM_model <- function(object, ahead = 5, alpha = 0.05, ...) {

  ### check input
  if (class(object) != "fHMM_model") {
    stop("'object' must be of class 'fHMM_model'.")
  }
  if (!(length(ahead) == 1 && is_number(ahead, int = TRUE, pos = TRUE))) {
    stop("'ahead' must be a positive integer")
  }
  if (!(length(alpha) == 1 && is_number(alpha, pos = TRUE) && alpha <= 1)) {
    stop("'alpha' must be a numeric between 0 and 1.")
  }

  ### extract parameters
  par <- parUncon2par(object$estimate, object$data$controls)
  M <- object$data$controls$states[1]
  N <- object$data$controls$states[2]
  sdds <- object$data$controls$sdds

  ### predict states
  state_prediction <- matrix(NA, nrow = ahead, ncol = M)
  last_state <- tail(if (object$data$controls$hierarchy) object$decoding[, 1] else object$decoding, n = 1)
  state_prob <- replace(numeric(M), last_state, 1)
  for (i in 1:ahead) {
    state_prob <- state_prob %*% par$Gamma
    state_prediction[i, ] <- state_prob
  }
  rownames(state_prediction) <- 1:ahead
  colnames(state_prediction) <- paste("state", 1:M, sep = "_")
  if (object$data$controls$hierarchy) {
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
  props <- sort(c(alpha, 0.5, 1 - alpha))
  if (!object$data$controls$hierarchy) {
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

#' @noRd
#' @export

print.fHMM_predict <- function(x, ...) {
  print(cbind(x$states, x$data))
  return(invisible(x))
}
