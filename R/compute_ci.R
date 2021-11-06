#' Confidence intervals
#' @description 
#' Computes confidence intervals for the estimates.
#' @param x
#' An object of class \code{fHMM_model}.
#' @return 
#' A list containing the following elements:
#' \itemize{
#'   \item \code{lb}: lower bound of confidence
#'   \item \code{estimate}: point estimate
#'   \item \code{ub}: upper bound of confidence
#' }

compute_ci = function(x, ci_level = 0.05){
  
  ### check inputs
  if(ci_level < 0 | ci_level > 1)
    stop("F.5")
  
  ### compute confidence intervals using the inverse Hessian approach
  inv_fisher = MASS::ginv(x$hessian)
  sds = suppressWarnings(sqrt(diag(inv_fisher)))
  lower_limit = x$estimate + qnorm(p = (1 - ci_level) / 2) * sds
  upper_limit = x$estimate + qnorm(p = 1 - (1 - ci_level) / 2) * sds
 
  ### if negative variance, replace by NA
  lower_limit[diag(inv_fisher) < 0] = NA
  upper_limit[diag(inv_fisher) < 0] = NA
  
  ### create and return output
  out = lapply(list(lower_limit, x$estimate, upper_limit),
               parUncon2parCon, x$data$controls)
  if(any(is.na(out)))
    warning("F.6")
  names(out) = c("lb","estimate","ub")
  return(out)
}
