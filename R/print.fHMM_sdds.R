#' Print method for \code{fHMM_sdds}.
#' @description 
#' This function is the print method for an object of class \code{fHMM_sdds}.
#' @param x
#' An object of class \code{fHMM_sdds}.
#' @param ...
#' Ignored.
#' @return
#' Returns \code{x} invisibly.
#' @export

print.fHMM_sdds = function(x, ...){
  for(sdd in x){
    cat(sdd$name)
    if(length(sdd$fixed)>0){
      cat("(")
      cat(paste(names(sdd$fixed),unlist(sdd$fixed), collapse = ", ", sep = " = "))
      cat(")")
    }
    cat(" ")
  }
  return(invisible(x))
}
