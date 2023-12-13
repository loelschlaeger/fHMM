#' Compute confidence intervals
#'
#' @description
#' This helper function computes confidence intervals for the estimates of an
#' \code{\link{fHMM_model}} object using the inverse Fisher information.
#'
#' @param x
#' An object of class \code{\link{fHMM_model}}.
#'
#' @param alpha
#' A \code{numeric} between 0 and 1, the alpha level for the confidence interval.
#' By default, \code{alpha = 0.05}, which computes a 95% confidence interval.
#'
#' @return
#' A \code{list} containing the following elements:
#' \itemize{
#'   \item \code{lb}: lower bound of confidence
#'   \item \code{estimate}: point estimate
#'   \item \code{ub}: upper bound of confidence
#' }
#'
#' @keywords internal

compute_ci <- function(x, alpha = 0.05) {

  ### check inputs
  if (!inherits(x,"fHMM_model")) {
    stop("Input 'x' must be an 'fHMM_model' object.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1) {
    stop("Input 'alpha' must be a numeric between 0 and 1.", call. = FALSE)
  }

  ### compute confidence intervals using the inverse Hessian approach
  if(any(!is.finite(x$hessian))) {
    x$hessian[!is.finite(x$hessian)] <- 0
    warning(paste(
      "The Hessian matrix contains non-finite values.",
      "For confidence interval computation, they were replaced by zeros."),
      immediate. = TRUE, call. = FALSE)
  }
  inv_fisher <- MASS::ginv(x$hessian)
  sds <- suppressWarnings(sqrt(diag(inv_fisher)))
  z_alpha <- stats::qnorm(p = 1 - alpha / 2)
  lower_limit <- x$estimate - z_alpha * sds
  upper_limit <- x$estimate + z_alpha * sds

  ### if negative variance, replace by NA_real_
  lower_limit[diag(inv_fisher) < 0] <- NA_real_
  upper_limit[diag(inv_fisher) < 0] <- NA_real_

  ### create and return output
  out <- lapply(
    list(lower_limit, x$estimate, upper_limit),
    parUncon2parCon, x$data$controls
  )
  if (anyNA(unlist(out))) {
    warning("Some confidence intervals could not be computed.", call. = FALSE)
  }
  names(out) <- c("lb", "estimate", "ub")
  return(out)
}
