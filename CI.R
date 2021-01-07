## Compute confidence intervals for the estimates
## Inputs:
##  fit: fitted model
##  alpha: confidence level
## Output: a list with elements lower_limit, estimate, and upper limit

conf_int = function (fit, alpha = 0.95) {
  if (is.null(fit$hessian)) {
    stop("Error: no Hessian matrix supplied.")
  }
  fisher = fit$hessian
  ## Hessian checks (to do)
  inv_fisher = ginv(fisher)
  sds = sqrt(diag(inv_fisher))
  lower_limit = fit$estimates + qnorm(p = (1 - alpha)) * sds
  upper_limit = fit$estimates + qnorm(p = alpha) * sds
  out = list(lower_limit = lower_limit, estimate = fit$estimate, upper_limit = upper_limit)
  return(out)
}
