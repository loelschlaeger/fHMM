#' Plot method for \code{fHMM_model}.
#' @description 
#' This function is the plot method for an object of class \code{fHMM_model}.
#' @param x
#' An object of class \code{fHMM_model}.
#' @param type
#' A character, specifying the typ of plot and can be one of
#' \itemize{
#'   \item \code{"ll"} for a visualization of the likelihood values in the 
#'         different optimization runs,
#'   \item
#' }
#' @param events
#' ...
#' @param ...
#' Ignored.
#' @return
#' No return value.
#' @export

plot.fHMM_model = function(x, type = "", events = NULL) {
  
  ### check input
  
  ### create colors
  var_col = function(col,n){
    colorRampPalette(c("white",col,"black"))(n+2)[2:(n+1)]
  }
  base_col = function(n){
    colorRampPalette(c("darkgreen","green","yellow","orange","red","darkred"))(n)
  }
  col_alpha = function(col,alpha=0.6){
    adjustcolor(col,alpha)
  }
  colors = list()
  if(!x$data$controls[["hierarchy"]]){
    colors[["hmm"]] = col_alpha(base_col(controls[["states"]][1]))
  } else {
    colors[["hhmm_cs"]] = col_alpha(base_col(controls[["states"]][1]))
    for(s in seq_len(x$data$controls[["states"]][1])){
      colors[["hhmm_fs"]][[s]] = col_alpha(var_col(colors[["hhmm_cs"]][s],
                                                   x$data$controls[["states"]][2]))
    }
  }
  
  ### likelihood plot
  if(type == "ll"){
    plot_ll(lls = x$lls)  
  }
    
}