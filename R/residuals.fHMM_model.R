#' Residuals
#'
#' @description
#' This function extracts the computed (pseudo-) residuals of
#' an \code{fHMM_model} object.
#'
#' @param object
#' An object of class \code{fHMM_model}.
#' @param ...
#' Ignored.
#'
#' @return
#' A vector (or a matrix, in case of an hierarchical HMM) with (pseudo-)
#' residuals for each observation.
#'
#' @examples
#' data("dax_model_3t")
#' compute_residuals(dax_model_3t)
#' residuals(dax_model_3t)
#' 
#' @export

residuals.fHMM_model <- function(object, ...) {
  
  ### check input
  if (!inherits(object,"fHMM_model")) {
    stop("'object' must be of class 'fHMM_model'.", call. = FALSE)
  }
  if (is.null(object[["residuals"]])) {
    stop("No residuals contained in 'object'.",
         "Please call 'compute_residuals()' first. ", call. = FALSE)
  }
  
  ### extract residuals
  return(object[["residuals"]])
}