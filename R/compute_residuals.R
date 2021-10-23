#' Compute (pseudo-) residuals.
#' @description 
#' This function computes (pseudo-) residuals of the estimated model.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @return 
#' An object of class \code{RprobitB_model}.
#' @export

compute_residuals = function(x) {
  
  residuals = if(!x$data$controls$hierarchy){
    numeric(length(x$data$data))
  } else {
    matrix(NA)
  }
  
  T = ifelse(!x$data$controls$hierarchy, length(x$data$data), dim(x$data$data)[1]) 
  
  par = parUncon2par(x$estimate, x$data$controls)
  
  for(t in seq_len(T)){
    if(par$sdd[[1]]$name == "t"){
      Fxt = pt(q = (x$data$data[t] - par$mus[x$decoding[t]]) / par$sigmas[x$decoding[t]],
               df = par$dfs[x$decoding[t]])
    }
    if(par$sdd[[1]]$name == "gamma"){
      Fxt = pgamma(q = x$data$data[t],
                   shape = par$mus[x$decoding[t]]^2 / par$sigmas[x$decoding[t]]^2,
                   scale = par$sigmas[x$decoding[t]]^2 / par$mus[x$decoding[t]])
    }
    residuals[t] = qnorm(Fxt)
  }
  
  x$residuals = residuals
  
  return(x)
  
}