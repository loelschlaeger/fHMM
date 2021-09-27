#' @title Confidence intervals
#' @description Computes confidence intervals for the estimates.
#' @param fit A list of fitted model information.
#' @param controls A list of controls.
#' @return A list containing the following elements:
#' \item{lb_ci_level}{lower bound of the intervals}
#' \item{estimate}{estimates}
#' \item{ub_ci_level}{upper bound of the intervals}
#' where \code{ci_level} is set in \code{controls}.

compute_ci = function(fit,controls){
  if(is.null(controls[["controls_checked"]]))
    stop("F.6")
  ci_level = controls[["results"]][["ci_level"]]
  if(ci_level < 0 | ci_level > 1)
    stop("F.5")
  
  ### extract estimates
  fisher = fit[["hessian"]]
  estimatesUncon = fit[["thetaUncon"]]
  
  ### compute confidence intervals using the inverse Hessian approach
  inv_fisher = MASS::ginv(fisher)
  sds = suppressWarnings(sqrt(diag(inv_fisher)))
  lower_limit = estimatesUncon + qnorm(p = (1 - ci_level) / 2) * sds
  upper_limit = estimatesUncon + qnorm(p = 1 - (1 - ci_level) / 2) * sds
 
  ### if negative variance, replace by NA
  lower_limit[diag(inv_fisher) < 0] = NA
  upper_limit[diag(inv_fisher) < 0] = NA
  out = lapply(list(lower_limit, estimatesUncon, upper_limit),thetaUncon2thetaCon,controls)
  names(out) = c(paste0("lb_",ci_level),"estimate",paste0("ub_",ci_level))
  if(any(is.na(out)))
    warning("F.6")
  
  return(out)
}
