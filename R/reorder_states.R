#' Reorder estimated states
#'
#' @description
#' This function reorders the estimated states, which can be useful for a 
#' comparison to true parameters or the interpretation of states.
#'
#' @param x
#' An object of class \code{\link{fHMM_model}}.
#' @param state_order
#' Either
#' - \code{"mean"}, in which case the states are ordered according to the means
#'   of the state-dependent distributions,
#' - or a vector (or a matrix) which determines the new ordering:
#'   \itemize{
#'   \item If \code{x$data$controls$hierarchy = FALSE}, \code{state_order} must
#'         be a vector of length \code{x$data$controls$states} with integer
#'         values from \code{1} to \code{x$data$controls$states}. If the old
#'         state number \code{x} should be the new state number \code{y}, put
#'         the value \code{x} at the position \code{y} of \code{state_order}.
#'         E.g. for a 2-state HMM, specifying \code{state_order = c(2, 1)} swaps
#'         the states.
#'   \item If \code{x$data$controls$hierarchy = TRUE}, \code{state_order} must
#'         be a matrix of dimension \code{x$data$controls$states[1]} x
#'         \code{x$data$controls$states[2] + 1}. The first column orders the
#'         coarse-scale states with the logic as described above. For each row,
#'         the elements from second to last position order the fine-scale states
#'         of the coarse-scale state specified by the first element. E.g. for an
#'         HHMM with 2 coarse-scale and 2 fine-scale states, specifying
#'         \code{state_order = matrix(c(2, 1, 2, 1, 1, 2), 2, 3)} swaps the
#'         coarse-scale states and the fine-scale states connected to 
#'         coarse-scale state 2.
#'   }
#'
#' @return
#' An object of class \code{\link{fHMM_model}}, in which states are reordered.
#'
#' @examples
#' dax_model_3t_reordered <- reorder_states(dax_model_3t, state_order = 3:1)
#' 
#' @export

reorder_states <- function(x, state_order = "mean") {

  ### check inputs
  if (!inherits(x,"fHMM_model")) {
    stop("'x' is not of class 'fHMM_model'.", call. = FALSE)
  }
  if (identical(state_order, "mean")) {
    pars <- parUncon2par(x$estimate, x$data$controls)
    if (!x$data$controls$hierarchy) {
      state_order <- as.matrix(order(pars$mu))
    } else {
      states <- x$data$controls$states
      state_order <- matrix(0, nrow = states[1], ncol = states[2] + 1)
      state_order_cs <- order(pars$mu)
      state_order[, 1] <- state_order_cs
      for (s in state_order_cs) {
        state_order[s, -1] <- order(pars$mu_star[[s]])
      }
    }
  } else {
    if (!x$data$controls$hierarchy) {
      if (!(is.numeric(state_order) &&
        length(state_order) == x$data$controls$states &&
        all(state_order %in% 1:x$data$controls$states))) {
        stop("'state_order' is missspecified, please check the documentation.", call. = FALSE)
      }
      state_order <- as.matrix(state_order)
    } else {
      if (!(is.numeric(state_order) && is.matrix(state_order) &&
        all(dim(state_order) == x$data$controls$states + c(0, 1)) &&
        all(state_order[1, ] %in% 1:x$data$controls$states[1]) &&
        all(sapply(1:x$data$controls$states[2], 
                   function(col) 1:x$data$controls$states[2] %in% state_order[col,-1])))
        ) {
        stop("'state_order' is missspecified, please check the documentation.", call. = FALSE)
      }
    }
  }

  ### reorder states
  par <- parUncon2par(x$estimate, x$data$controls)
  permut <- diag(x$data$controls$states[1])[state_order[, 1], ]
  par$Gamma <- permut %*% par$Gamma %*% t(permut)
  par$mu <- as.vector(permut %*% par$mu)
  if (x$data$controls$sdds[[1]]$name != "poisson") {
    par$sigma <- as.vector(permut %*% par$sigma)
  }
  if (x$data$controls$sdds[[1]]$name == "t") {
    par$df <- as.vector(permut %*% par$df)
  }
  if (x$data$controls$hierarchy) {
    par$Gamma_star <- par$Gamma_star[state_order[, 1]]
    par$mu_star <- par$mu_star[state_order[, 1]]
    if (x$data$controls$sdds[[2]]$name != "poisson") {
      par$sigma_star <- par$sigma_star[state_order[, 1]]
    }
    if (x$data$controls$sdds[[2]]$name == "t") {
      par$df_star <- par$df_star[state_order[, 1]]
    }
    for (s in state_order[, 1]) {
      permut <- diag(x$data$controls$states[2])[state_order[which(state_order[, 1] == s), -1], ]
      par$Gamma_star[[s]] <- permut %*% par$Gamma_star[[s]] %*% t(permut)
      par$mu_star[[s]] <- as.vector(permut %*% par$mu_star[[s]])
      if (x$data$controls$sdds[[2]]$name != "poisson") {
        par$sigma_star[[s]] <- as.vector(permut %*% par$sigma_star[[s]])
      }
      if (x$data$controls$sdds[[2]]$name == "t") {
        par$df_star[[s]] <- as.vector(permut %*% par$df_star[[s]])
      }
    }
  }
  parUncon <- par2parUncon(par, x$data$controls)
  match <- oeli::match_numerics(unname(x$estimate), unname(parUncon))
  permut_all <- diag(length(x$estimate))[match, ]
  x$estimate <- parUncon
  x$gradient <- as.vector(permut_all %*% x$gradient)
  x$inverse_fisher <- as.vector(permut_all %*% x$inverse_fisher)
  x$nlm_output$estimate <- x$estimate
  x$nlm_output$gradient <- x$gradient

  ### redo decoding and residual computation
  if (!is.null(x$decoding)) {
    x <- decode_states(x, verbose = FALSE)
  }
  if (!is.null(x$residuals)) {
    x <- compute_residuals(x, verbose = FALSE)
  }

  ### return reorderd 'fHMM_model'
  return(x)
}
