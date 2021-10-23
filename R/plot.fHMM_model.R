#' Plot method for \code{fHMM_model}.
#' @description 
#' This function is the plot method for an object of class \code{fHMM_model}.
#' @param x
#' An object of class \code{fHMM_model}.
#' @param plot_type
#' A character (vector), specifying the type of plot and can be one (or more) of
#' \itemize{
#'   \item \code{"ll"} for a visualization of the likelihood values in the 
#'         different optimization runs,
#'   \item \code{"sdd"} for a visualization of the estimated state-dependent
#'         distributions,
#'   \item \code{"pr"} for a visualization of the model's (pseudo-) residuals,
#'   \item \code{"ts"} for a visualization of the financial time series.
#' }
#' @param events
#' ...
#' @param colors
#' ...
#' @param ...
#' Ignored.
#' @return
#' No return value.
#' @export

plot.fHMM_model = function(x, plot_type = "ts", events = NULL, 
                           colors = NULL, ...) {
  
  ### check input
  if(!class(x) == "fHMM_model")
    stop("'x' is not of class 'fHMM_model'.")
  plot_type = intersect(plot_type, c("ll", "sdd", "pr", "ts"))
  if(!is.null(events))
    if(!is.list(events))
      stop("...")
  if(!is.null(colors))
    if(class(colors) != "fHMM_colors")
      stop("")
  
  ### create and check colors
  if(is.null(colors))
    colors = fHMM_colors(controls = x$data$controls)
  if(x$data$controls$hierarchy){
    if(length(colors) != x$data$controls$states)
      stop("...")
  } else {
    if(any(dim(colors) != x$data$controls$states + c(0,1)))
      stop("...")
  }
  
  ### visualizations
  if("ll" %in% plot_type) 
    plot_ll(lls = x$lls)  
  if("sdd" %in% plot_type)
    plot_sdd()
  if("pr" %in% plot_type){
    if(is.null(x$residuals)){
      warning("'residuals not available.'")
    } else {
      plot_pr(x$residuals)
    }
  }
  if("ts" %in% plot_type)
    plot_ts()
    
}