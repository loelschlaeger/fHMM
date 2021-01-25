#' Compute confidence intervals for the estimates
#'
#' @param fit A fitted model
#' @param alpha Confidence level, default 0.95
#'
#' @return A list containing the following elements:
#' \item{lower_limit}{the lower limits of the intervals}
#' \item{estimate}{the estimates}
#' \item{upper_limit}{the upper limits of the intervals}
#'
#' @examples
#' conf_int(fit = fit)
conf_int = function (fit, alpha = 0.95) {
  # Input checks
  if (alpha < 0 | alpha > 1) {
    stop("Value of 'alpha' needs to be between 0 and 1.",call.=FALSE)
  }
  fisher = fit$hessian
  # Hessian checks (to do)
  inv_fisher = MASS::ginv(fisher)
  sds = suppressWarnings(sqrt(diag(inv_fisher)))
  lower_limit = fit$estimate + qnorm(p = (1 - alpha) / 2) * sds
  upper_limit = fit$estimate + qnorm(p = 1 - (1 - alpha) / 2) * sds
  out = list(lower_limit = lower_limit, estimate = fit$estimate, upper_limit = upper_limit)
  # Output checks
  if(any(is.na(out))) {
    warning("Some confidence intervals could not be computed. The corresponding estimates may lie close to the boundaries of their parameter space.")
  }
  return(out)
}
