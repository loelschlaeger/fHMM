#' Compute confidence intervals for the estimates
#'
#' @param fit A fitted model
#' @param controls A list of controls
#'
#' @return A list containing the following elements:
#' \item{lb_alpha}{the lower bound of the intervals}
#' \item{estimate}{the estimates}
#' \item{ub_limit}{the upper bound of the intervals}

compute_ci = function(fit,controls){
  if(is.null(controls[["controls_checked"]])){
    stop(sprintf("%s (%s)",exception("F.6")[2],exception("F.6")[1]),call.=FALSE)
  }
  
  alpha = controls[["ci_level"]]
  if(alpha < 0 | alpha > 1){
    stop(sprintf("%s (%s)",exception("F.5")[2],exception("F.5")[1]),call.=FALSE)
  }
  
  ### extract estimates
  fisher = fit[["hessian"]]
  estimatesUncon = fit[["model"]][["estimate"]]
  
  ### compute confidence intervals using the inverse Hessian approach
  inv_fisher = MASS::ginv(fisher)
  sds = suppressWarnings(sqrt(diag(inv_fisher)))
  lower_limit = estimatesUncon + qnorm(p = (1 - alpha) / 2) * sds
  upper_limit = estimatesUncon + qnorm(p = 1 - (1 - alpha) / 2) * sds
  
  ### Hessian checks
  eigen_values = eigen(inv_fisher,only.values=TRUE,symmetric=TRUE)[["values"]]
  lower_limit[eigen_values < 0] = NA
  upper_limit[eigen_values < 0] = NA
  out = lapply(list(lower_limit, estimatesUncon, upper_limit),thetaUncon2thetaCon,controls)
  names(out) = c(paste0("lb_",alpha),"estimate",paste0("ub_",alpha))
  if(any(eigen_values < 0) || any(is.na(out))) {
    warning(sprintf("%s (%s)",exception("F.6")[2],exception("F.6")[1]),call.=FALSE)
  }
  
  return(out)
}
