#' Computing confidence intervals
#'
#' @description
#' This function computes confidence intervals for the estimates of an
#' \code{fHMM_model} object using the inverse Fisher information.
#'
#' @param x
#' An object of class \code{fHMM_model}.
#'
#' @param alpha
#' The alpha level for the confidence interval, a numeric between 0 and 1. 
#' Per default, \code{alpha = 0.05}, which computes a 95% confidence interval.
#'
#' @return
#' A list containing the following elements:
#' \itemize{
#'   \item \code{lb}: lower bound of confidence
#'   \item \code{estimate}: point estimate
#'   \item \code{ub}: upper bound of confidence
#' }
#'
#' @keywords
#' internal
#'
#' @examples
#' data("dax_model_3t")
#' fHMM:::compute_ci(x = dax_model_3t, alpha = 0.05)
#' @importFrom stats qnorm

compute_ci <- function(x, alpha = 0.05) {

  ### check inputs
  if (!inherits(x,"fHMM_model")) {
    stop("'x' must be of class 'fHMM_model'.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("'alpha' must be a numeric between 0 and 1.", call. = FALSE)
  }

  ### compute confidence intervals using the inverse Hessian approach
  x$hessian[which(x$hessian %in% c(Inf, -Inf, NA, NaN))] <- 0
  inv_fisher <- MASS::ginv(x$hessian)
  sds <- suppressWarnings(sqrt(diag(inv_fisher)))
  z_alpha <- stats::qnorm(p = 1 - alpha / 2)
  lower_limit <- x$estimate - z_alpha * sds
  upper_limit <- x$estimate + z_alpha * sds

  ### if negative variance, replace by NA
  lower_limit[diag(inv_fisher) < 0] <- NA
  upper_limit[diag(inv_fisher) < 0] <- NA

  ### create and return output
  out <- lapply(
    list(lower_limit, x$estimate, upper_limit),
    parUncon2parCon, x$data$controls
  )
  if (any(is.na(out))) {
    warning(paste("Some confidence intervals could not be computed.",
    "The corresponding estimates may lie close to the boundaries of their parameter space,",
    "the confidence intervals may be unreliable and are therefore replaced by NA."),
    call. = FALSE)
  }
  names(out) <- c("lb", "estimate", "ub")
  return(out)
}
