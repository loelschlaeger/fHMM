#' @title Visualization
#' @description Calls functions for visualization of model results.
#' @param data A list of processed data information.
#' @param fit A list of fitted model information.
#' @param decoding A vector (in case of a HMM) or a matrix (in case of a hierarchical HMM) of decoded states.
#' @param controls A list of controls.
#' @param events A list of (historical, financial) events.
#' @return No return value. Calls visualization functions \code{plot_sdd}, \code{plot_ts} and \code{pseudo_residuals}.

create_visuals = function(data,fit,decoding,controls,events){
  
  ### pre-checks
  if(is.na(controls[["controls_checked"]])){
    stop(sprintf("%s (%s)",exception("C.1")[2],exception("C.1")[1]),call.=FALSE)
  }
  if(controls[["sim"]] & !is.null(events)){
    events = NULL
    warning(sprintf("%s (%s)",exception("V.1")[2],exception("V.1")[1]),call.=FALSE)
  } else {
    if(length(events[["dates"]])!=length(events[["names"]])){
      stop(sprintf("%s (%s)",exception("V.2")[2],exception("V.2")[1]),call.=FALSE)
    }
  }
  
  ### save events
  if(!is.null(events)) check_saving(object = events, filetype = "rds", controls = controls)
  
  ### define colours
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
  if(controls[["model"]]=="hmm"){
    colors[["hmm"]] = col_alpha(base_col(controls[["states"]][1]))
  }
  if(controls[["model"]]=="hhmm"){
    colors[["hhmm_cs"]] = col_alpha(base_col(controls[["states"]][1]))
    for(s in seq_len(controls[["states"]][1])){
      colors[["hhmm_fs"]][[s]] = col_alpha(var_col(colors[["hhmm_cs"]][s],controls[["states"]][2]))
    }
  }
  
  ### create visualization of state dependent distributions
  plot_sdd(controls,data,fit,decoding,colors)
  
  ### create visualization of decoded time series
  plot_ts(controls,data,decoding,colors,events)
  
  ### compute, save, visualize and test pseudo-residuals
  pseudo_residuals(controls,data,fit,decoding)
  
  message("Model results visualized.")
}
