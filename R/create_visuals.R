#' Visualize model results
#'
#' @param data A list of processed data information
#' @param fit A fitted model
#' @param decoding A matrix of decoded states
#' @param controls A list of controls
#' @param events A list of events, default \code{NULL}
#' 
#' @return No return value, called for side effects

create_visuals = function(data,fit,decoding,controls,events=NULL){
  
  ### pre-checks
  if(is.null(controls[["controls_checked"]])){
    stop(sprintf("%s (%s)",exception("C.1")[2],exception("C.1")[1]),call.=FALSE)
  }
  if(controls[["sim"]] & !is.null(events)){
    events = NULL
    warning(sprintf("%s (%s)",exception("V.1")[2],exception("V.2")[1]),call.=FALSE)
  } 
  if(length(events[["dates"]])!=length(events[["names"]])){
    stop(sprintf("%s (%s)",exception("V.2")[2],exception("V.2")[1]),call.=FALSE)
  }
  
  ### save events
  if(!controls[["sim"]] & !is.null(events)){
    check_saving(object   = events,
                 filetype = "rds",
                 controls = controls)
  }
  
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
  if(controls[["model"]]=="HMM"){
    colors[["HMM"]] = col_alpha(base_col(controls[["states"]][1]))
  }
  if(controls[["model"]]=="HHMM"){
    colors[["HHMM_cs"]] = col_alpha(base_col(controls[["states"]][1]))
    for(s in seq_len(controls[["states"]][1])){
      colors[["HHMM_fs"]][[s]] = col_alpha(var_col(colors[["HHMM_cs"]][s],controls[["states"]][2]))
    }
  }
  
  ### create visualization of state dependent distributions
  plot_sdd(controls,data,fit,decoding,colors)
  
  ### create visualization of decoded time series
  plot_ts(controls,data,decoding,colors,events)
  
  ### compute, save, visualize and test pseudo-residuals
  pseudo_residuals(controls,data,fit,decoding)
  
  message("model results visualized")
}