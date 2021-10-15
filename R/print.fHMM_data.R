#' Print method for \code{fHMM_data}.
#' @description 
#' This function is the print method for an object of class \code{fHMM_data}.
#' @param x
#' An object of class \code{fHMM_data}.
#' @param ...
#' Ignored.
#' @return
#' Returns \code{x} invisibly.
#' @export

print.fHMM_data = function(x, ...) {
  cat("fHMM",ifelse(x$controls$simulated,"simulated","empirical"),"data:\n")
  if(x$controls$simulated){
    if(!x$controls[["hierarchy"]]){
      cat("* number of observations:", length(x$data), "\n")
    } else {
      cat("* number of observations:", length(x[["data"]][,1]), length(x[["data"]][,-1][!is.na(x[["data"]][,-1])]), "\n")
      cat("* fine-scale dimension:", if(!is.na(x$controls$horizon[2])) x$controls$horizon[2] else paste0("'",x$controls$period,"'"),"\n")
    }
  } else {
    cat("* data source:",basename(x$controls$data$file), "\n")
    cat("* date column:",x$controls$data$date_column, "\n")
    cat("* data column:",x$controls$data$data_column, "\n")
    cat("* log returns:",x$controls$data$logreturns, "\n")
    if(!x$controls[["hierarchy"]]){
      cat("* number of observations:", length(x$data), "\n")
    } else {
      cat("* number of observations:", length(x[["data"]][,1]), length(x[["data"]][,-1][!is.na(x[["data"]][,-1])]), "\n")
      cat("* fine-scale dimension:", if(!is.na(x$controls$horizon[2])) x$controls$horizon[2] else paste0("'",x$controls$period,"'"),"\n")
      cat("* coarse-scale merge:",deparse1(x$controls$data$merge, collapse = ""))
    }
  }
  return(invisible(x))
}
