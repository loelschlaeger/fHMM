#' Compute confidence intervals for the estimates
#'
#' @param fit A fitted model
#' @param alpha confidene level, default 0.95
#'
#' @return A list containing the following elements:
#' \item{\code{lower_limit}}{the lower limits}
#' \item{\code{estimate}}{the estimates}
#' \item{\code{upper_limit}}{the upper limits}
#'
#' @examples
#' conf_int(fit = example$fit)
conf_int <- function (fit, alpha = 0.95) {
  # Input checks
  if (is.null(fit$hessian)) {
    stop("No Hessian matrix supplied. Set hessian = TRUE in controls.")
  }
  if (alpha < 0 | alpha > 1) {
    stop("alpha needs to be between 0 and 1.")
  }
  fisher = fit$hessian
  # Hessian checks (to do)
  inv_fisher = MASS::ginv(fisher)
  sds = suppressWarnings(sqrt(diag(inv_fisher)))
  lower_limit = fit$estimate + qnorm(p = (1 - alpha) / 2) * sds
  upper_limit = fit$estimate + qnorm(p = 1 - (1 - alpha) / 2) * sds
  out = list(lower_limit = lower_limit, estimate = fit$estimate, upper_limit = upper_limit)
  # Output checks
  if (any(is.na(out))) {
    warning("Some CIs could not be computed. The corresponding estimates may lie close to the boundaries of their parameter space.")
  }
  return(out)
}
