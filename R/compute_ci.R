#' Compute confidence intervals for the estimates
#'
#' @param fit A fitted model
#' @param controls A list of controls
#' @param alpha Confidence level, default \code{alpha=0.95}
#'
#' @return A list containing the following elements:
#' \item{lower_limit}{the lower limits of the intervals}
#' \item{estimate}{the estimates}
#' \item{upper_limit}{the upper limits of the intervals}

compute_ci = function(fit,controls,alpha=0.95){
  ### input checks
  if(alpha < 0 | alpha > 1){
    stop(sprintf("%s (%s)",exception("F.5")[2],exception("F.5")[1]),call.=FALSE)
  }
  if(is.null(controls[["controls_checked"]])){
    stop(sprintf("%s (%s)",exception("F.6")[2],exception("F.6")[1]),call.=FALSE)
  }
  
  ### extract estimates
  fisher = fit[["hessian"]]
  estimates = thetaUncon2thetaCon(fit[["mod"]][["estimate"]],controls)
  
  ### Hessian checks (to do)
  inv_fisher = MASS::ginv(fisher)
  sds = suppressWarnings(sqrt(diag(inv_fisher)))
  lower_limit = estimates + qnorm(p = (1 - alpha) / 2) * sds
  upper_limit = estimates + qnorm(p = 1 - (1 - alpha) / 2) * sds
  out = list(lower_limit = lower_limit, estimate = fit$estimate, upper_limit = upper_limit)
  
  ### output checks
  if(any(is.na(out))) {
    warning(sprintf("%s (%s)",exception("C.3")[2],exception("C.3")[1]),call.=FALSE)
  }
  return(out)
}
