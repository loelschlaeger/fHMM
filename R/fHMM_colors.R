#' Set color scheme for visualizations in the fHMM package.
#' @description 
#' This function defines a color scheme for visualizations in the fHMM package.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @param colors
#' A character vector of color names or hexadecimal RGB triplets.
#' @return
#' An object of class \code{fHMM_colors}, which is...
#' @export

fHMM_colors = function(controls, colors = c("darkgreen", "green", "yellow",
                                            "orange", "red", "darkred")) {

  ### check inputs
  if(!class(controls) == "fHMM_controls")
    stop("'controls' must be of class 'fHMM_controls'.")
  if(!is.character(colors))
    stop("'colors' must be a character vector.")
  for(col in colors){
    out = tryCatch(is.matrix(col2rgb(col)), error = function(e) FALSE)
    if(out == FALSE)
      stop("'",col,"' in 'colors' is not a valid color representation.")
  }
  
  ### helper functions
  var_col = function(col,n) colorRampPalette(c("white",col,"black"))(n+2)[2:(n+1)]
  base_col = function(n) colorRampPalette(colors)(n)
  col_alpha = function(col,alpha=0.6) adjustcolor(col,alpha)
  
  ### create and return 'fHMM_colors' 
  if(!controls[["hierarchy"]]){
    out = col_alpha(base_col(controls[["states"]][1]))
  } else {
    out = matrix(NA, nrow = controls[["states"]][1], 
                 ncol = controls[["states"]][2] + 1)
    out[,1] = col_alpha(base_col(controls[["states"]][1]))
    for(s in seq_len(controls[["states"]][1]))
      out[s,-1] = col_alpha(var_col(out[s,1], controls[["states"]][2]))
  }
  class(out) = "fHMM_colors"
  return(out)
  
}