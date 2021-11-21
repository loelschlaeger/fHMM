#' Create labels for estimated parameters.
#' @description 
#' This function creates labels for estimated parameters.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @param expected_length
#' The expected output length.
#' @return 
#' A character vector of parameter labels.
#' @keywords 
#' internal

parameter_labels = function(controls, expected_length) {
  
  ### check input
  if(class(controls) != "fHMM_controls")
    stop("'controls' is not of class 'fHMM_controls'.")
  if(!is_number(expected_length, pos = TRUE))
    stop("'expected_length' must be a positive integer.")
  
  ### helper function for tpm labels
  tpm_labels = function(dim) outer(1:dim, 1:dim, paste, sep = ".") %>% 
    .[row(.) != col(.)]
  
  ### create parameter labels
  labels = paste0("Gamma_", tpm_labels(controls$states[1]))
  for(par in c("mu", "sigma", if(controls[["sdds"]][[1]]$name == "t") "df"))
    if(is.null(controls[["sdds"]][[1]]$pars[[par]]))
      labels = c(labels, paste0(par,"_",1:controls$states[1]))
  if(controls[["hierarchy"]]){
    for(i in 1:controls$states[1])
      labels = c(labels, paste0("Gamma*",i,"_", tpm_labels(controls$states[2])))
    for(par in c("mu", "sigma", if(controls[["sdds"]][[1]]$name == "t") "df"))
      if(is.null(controls[["sdds"]][[2]]$pars[[par]]))
        for(i in 1:controls$states[1])
          labels = c(labels, paste0(par,i,"_", 1:controls$states[2]))
  }
  
  ### check and return parameter labels
  if(length(labels) != expected_length)
    stop("Unexpected number of parameter labels.")
  return(labels)
  
}
