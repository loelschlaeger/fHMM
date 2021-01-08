## Compute confidence intervals for the estimates
## Inputs:
##  fit: a fitted model
##  alpha: confidence level, default 0.95
## Output: a list with elements lower_limit, estimate, and upper limit
conf_int <- function (fit, alpha = 0.95) {
  # Input checks
  if (is.null(fit$hessian)) {
    stop("Error: no Hessian matrix supplied. Set hessian = TRUE in controls.")
  }
  if (alpha < 0 | alpha > 1) {
    stop("Error: alpha needs to be between 0 and 1.")
  }
  fisher = fit$hessian
  # Hessian checks (to do)
  inv_fisher = ginv(fisher)
  sds = sqrt(diag(inv_fisher))
  lower_limit = fit$estimate + qnorm(p = (1 - alpha) / 2) * sds
  upper_limit = fit$estimate + qnorm(p = 1 - (1 - alpha) / 2) * sds
  df = data.frame(lower_limit = lower_limit, estimate = fit$estimate, upper_limit = upper_limit)
  return(df)
}
