#' Check if \code{x} is a ((non)-negative) ((non-)positive) (integer) numeric 
#' (vector).
#' @param x
#' An R object.
#' @param int
#' A boolean, if \code{TRUE} checks if \code{x} is an integer.
#' @param neg
#' A boolean, if \code{TRUE} checks if \code{x} is negative.
#' @param non_neg
#' A boolean, if \code{TRUE} checks if \code{x} is non-negative.
#' @param pos
#' A boolean, if \code{TRUE} checks if \code{x} is positive.
#' @param non-pos
#' A boolean, if \code{TRUE} checks if \code{x} is non-positive.
#' @return
#' A boolean.

is_number = function(x, int = FALSE, neg = FALSE, non_neg = FALSE, pos = FALSE,
                     non_pos = FALSE) {
  if(length(x) == 0)
    return(TRUE)
  for(i in 1:length(x)) {
    out = rep(TRUE,length(x))
    if(!is.numeric(x[i])){
      out[i] = FALSE
      break
    }
    if(int) if(x[i]%%1!=0){
      out[i] = FALSE
      break
    }
    if(neg) if(!x[i]<0){
      out[i] = FALSE
      break
    }
    if(non_neg) if(!x[i]>=0){
      out[i] = FALSE
      break
    }
    if(pos) if(!x[i]>0){
      out[i] = FALSE
      break
    }
    if(non_pos) if(!x[i]<=0){
      out[i] = FALSE
      break
    }
  }
  return(out)
}
