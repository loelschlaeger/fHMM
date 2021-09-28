#' Print method for \code{fHMM_controls}.
#' @description 
#' This function is the print method for an object of class \code{fHMM_controls}.
#' @param x
#' An object of class \code{fHMM_controls}.
#' @param ...
#' Ignored.
#' @return
#' Returns \code{x} invisibly.
#' No return value.
#' @export

print.fHMM_controls = function(x, ...) {
  cat("Controls:\n")
  cat("> path:", x[["path"]], "\n")
  cat("> model type:", x[["model"]], "\n")
  cat("> data type:", ifelse(x[["sim"]],"simulated","empirical"), "\n")
  if(x[["model"]] == "hmm") {
    cat("> number of states:", x[["states"]][1], "\n")
    cat("> SDDs:", paste0(x[["sdds"]][1],
                          ifelse(!is.na(x[["fixed_dfs"]][1]),
                                 paste0("(",x[["fixed_dfs"]][1],")"),"")),"\n")
  }
  if(x[["model"]] == "hhmm") {
    cat("> number of states:", x[["states"]][1], x[["states"]][2], "\n")
    cat("> SDDs:", paste0(x[["sdds"]][1],
                          ifelse(!is.na(x[["fixed_dfs"]][1]),
                                 paste0("(",x[["fixed_dfs"]][1],")"),"")),
        paste0(x[["sdds"]][2],
               ifelse(!is.na(x[["fixed_dfs"]][2]),
                      paste0("(",x[["fixed_dfs"]][2],")"),"")),"\n")
  }
  cat("> number of runs:", x[["fit"]][["runs"]], 
      ifelse(x[["fit"]][["at_true"]],"(initialised at true values)",""),"\n")
  if(!is.null(x[["fit"]][["seed"]])) 
    cat("> seed:",x[["fit"]][["seed"]],"\n")
}
