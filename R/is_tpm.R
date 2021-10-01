#' Check if \code{x} is a transition probability matrix.
#' @param x
#' A matrix.
#' @return
#' A boolean.

is_tpm = function(x) {
  if(nrow(x) != ncol(x) || any(rowSums(x)!=1) || any(x<0)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}