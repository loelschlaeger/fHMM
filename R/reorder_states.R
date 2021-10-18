#' Reordering of estimated states.
#' @description 
#' This function reorders the estimated states, which is useful for a comparison
#' to true parameters or the interpretation of states.
#' @param x
#' An object of class \code{fHMM_model}.
#' @param state_order
#' A vector or a matrix which determines the new ordering.
#' \itemize{
#'   \item If \code{x$data$controls$hierarchy = FALSE}, \code{state_order} must
#'         be a vector of length \code{x$data$controls$states} with integer
#'         values from \code{1} to \code{x$data$controls$states}. If the old 
#'         state number \code{x} should be the new state number \code{y}, put
#'         the value \code{x} at the position \code{y} of \code{state_order}.
#'         E.g. for a 2-state HMM, specifying \code{state_order = c(2,1)} 
#'         swaps the states.
#'   \item If \code{x$data$controls$hierarchy = TRUE}, \code{state_order} must
#'         be a matrix of dimension \code{x$data$controls$states[1]} x 
#'         \code{x$data$controls$states[2] + 1}. The first column orders the 
#'         coarse-scale states with the logic as described above. For each row,
#'         the second to last elements order the fine-scale states of the
#'         coarse-scale state specified by the first element. E.g. for an HHMM
#'         with 2 coarse- and 2 fine-scale states, specifying 
#'         \code{state_order = matrix(c(1,2,2,1,1,2),2,3)} swaps both
#'         coarse-scale states and the fine-scale states for coarse-scale state 
#'         2.
#' }
#' @return 
#' An object of class \code{fHMM_model}.
#' @export

reorder_states = function(x, state_order) {
  
  ### check inputs
  if(class(x) != "fHMM_model")
    stop("'x' is not of class 'fHMM_model'.")
  if(!x$data$controls$hierarchy){
    if(!(is.numeric(state_order) && 
         length(state_order) == x$data$controls$states &&
         all(state_order %in% 1:x$data$controls$states)))
      stop("<'state_order' missspecified>")
  } else {
    if(!(is.numeric(state_order) && is.matrix(state_order) &&
         all(dim(state_order) == x$data$controls$states + c(0,1)) &&
         all(state_order[1,] %in% 1:x$data$controls$states[1]) &&
         all(state_order[-1,] %in% 1:x$data$controls$states[2])))
      stop("<'state_order' missspecified>")
  }
  
  ### reorder states
  if(!x$data$controls$hierarchy){
    par = parUncon2par(x$estimate, x$data$controls)
    permut = diag(x$data$controls$states)[state_order,]
    par$Gamma = permut %*% par$Gamma %*% t(permut)
    par$mus = as.vector(permut %*% par$mus)
    par$sigmas = as.vector(permut %*% par$sigmas)
    par$dfs = as.vector(permut %*% par$dfs)
    parUncon = par2parUncon(par, x$data$controls)
    permut_all = diag(length(x$estimate))[match_all(x$estimate, parUncon),]
    x$estimate = as.vector(permut_all %*% parUncon)
    class(x$estimate) = "parUncon"
    x$gradient = as.vector(permut_all %*% x$gradient)
    x$hessian = permut_all %*% x$hessian %*% t(permut_all)
    x$nlm_output$estimate = x$estimate
    x$nlm_output$gradient = x$gradient
    if(!is.null(x$decode)) x = decode(x)
  } else {
    stop("not implemented yet.")
  }
  
  ### return reorderd 'fHMM_model'
  return(x)
  
}