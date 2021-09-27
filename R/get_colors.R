get_colors = function(controls) {
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
  return(colors)
}
