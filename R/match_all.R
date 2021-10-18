#' Match positions of two numeric vectors "as good as possible".
#' @description
#' This function matches the positions of two numeric vectors.
#' @param x
#' A numeric vector.
#' @param y
#' Another numeric vector of the same length as \code{x}.
#' @return
#' An integer vector of length \code{length(x)} with the positions of \code{y}
#' in \code{x}.

match_all = function(x, y) {
  stopifnot(length(x) == length(y))
  matches = numeric(length(x))
  distances = unique(sort(dist(c(x,y)))) + sqrt(.Machine$double.eps)
  for(d in distances)
    if(any(c(!is.na(x),!is.na(y))))
      for(i_x in 1:length(x)) for(i_y in 1:length(y))
          if(!is.na(x[i_x]) && !is.na(y[i_y]))
            if(isTRUE(all.equal(x[i_x],y[i_y],d))){
              matches[i_y] = i_x
              x[i_x] = NA
              y[i_y] = NA
            }
  return(matches)
}