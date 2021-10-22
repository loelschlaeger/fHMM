#' Balancing visualization of multiple figures.
#' @description
#' This function finds a balanced setting for \code{par(mfrow)}.
#' @param n
#' The total number of figures.
#' @return
#' A vector of the form \code{c(nr,nc)}. If \code{par(mfrow = c(nr,nc))},
#' subsequent figures will be drawn in an \code{nr} x \code{nc} array on the
#' current device by rows.

set_mfrow = function(n){
  if(n==1)
    return(c(1,1))
  ran = 2:max(floor((n-1)/2),1)
  ran2 = pmax(ceiling(n/(ran)),1)
  rem = abs(n - ran2*ran)
  score = abs(sqrt(n)-(ran)) + abs(sqrt(n)-(ran2)) + rem
  nr = ran[which.min(score)]
  nc = ran2[which.min(score)]
  return(c(nr, nc))
}
