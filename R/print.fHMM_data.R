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
  ### print data characteristics
  if(controls[["sim"]]){
    if(controls[["model"]]=="hmm"){
      writeLines(sprintf("- %s %s","sample size:",length(data[["data"]])))
    }
    if(controls[["model"]]=="hhmm"){
      writeLines(sprintf("- %s %s / %s","sample size:",dim(data[["data"]])[1],length(data[["data"]][,-1][!is.na(data[["data"]][,-1])])))
    }
  } 
  if(!controls[["sim"]]){
    if(controls[["model"]]=="hmm"){
      writeLines(sprintf("- %s %s","data source:",controls[["data"]][["source"]][1]))
      writeLines(sprintf("- %s %s","data column:",controls[["data"]][["column"]][1]))
      writeLines(sprintf("- %s %s to %s","time horizon:",data[["dates"]][1],rev(data[["dates"]])[1]))
      writeLines(sprintf("- %s %s","data points:",length(data[["data"]])))
      writeLines(sprintf("- %s %s","log-returns:",controls[["data"]][["log_returns"]][1]))
    }
    if(controls[["model"]]=="hhmm"){
      writeLines(sprintf("- %s %s / %s","data source:",controls[["data"]][["source"]][1],controls[["data"]][["source"]][2]))
      writeLines(sprintf("- %s %s / %s","data column:",controls[["data"]][["column"]][1],controls[["data"]][["column"]][2]))
      writeLines(sprintf("- %s %s to %s","time horizon:",data[["dates"]][1],rev(data[["dates"]])[1]))
      writeLines(sprintf("- %s %s / %s","data points:",dim(data[["data"]])[1],length(data[["data"]][,-1][!is.na(data[["data"]][,-1])])))
      writeLines(sprintf("- %s %s / %s","log-returns:",controls[["data"]][["log_returns"]][1],controls[["data"]][["log_returns"]][2]))
      writeLines(sprintf("- %s %s","CS transformation:",gsub(" ", "", paste(deparse(controls[["data"]][["cs_transform"]]),collapse=""),fixed = TRUE)))
      if(is.numeric(controls[["horizon"]][2])) writeLines(sprintf("- %s %s","FS dimension:",controls[["horizon"]][2]))
      if(controls[["horizon"]][2]=="w") writeLines(sprintf("- %s %s","FS dimension:","weekly"))
      if(controls[["horizon"]][2]=="m") writeLines(sprintf("- %s %s","FS dimension:","monthly"))
      if(controls[["horizon"]][2]=="q") writeLines(sprintf("- %s %s","FS dimension:","quarterly"))
      if(controls[["horizon"]][2]=="y") writeLines(sprintf("- %s %s","FS dimension:","yearly"))
    }
  }
}