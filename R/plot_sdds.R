#' Visualization of estimated state-dependent distributions.
#' @description 
#' This function plots the estimated state-dependent distributions.
#' @param est
#' An object of class \code{fHMM_parameters} with estimated parameters.
#' @param true
#' Either \code{NULL} or an object of class \code{fHMM_parameters} with true
#' parameters.
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @param colors
#' An object of class \code{fHMM_colors}.
#' @return 
#' No return value. Draws a plot to the current device.

plot_sdds = function(est, true = NULL, controls, colors) {
  
  ### define densities
  density = function(name, x, sigma, mu, df){
    if(name == "t"){
      (1/sigma) * dt(x = (x - mu)/sigma, df = df)
    } else if(name == "gamma"){
      dgamma(x = x, shape = mu^2/sigma^2, scale = sigma^2/mu)
    } else {
      stop()
    }
  }
  
  ### compute densities
  xmin = -1
  xmax = 1
  length.out = 1e4
  while(TRUE){
    x = seq(from = xmin, to = xmax, length.out = length.out)
    f.x = list()
    for(s in 1:controls$states[1])
      f.x[[s]] = density(name = est$sdds[[1]]$name, x = x, mu = est$mus[s], 
                         sigma = est$sigmas[s], df = est$dfs[s])
    f.x_true = list()
    if(!is.null(true))
      for(s in 1:controls$states[1])
        f.x_true[[s]] = density(name = true$sdds[[1]]$name, x = x, 
                                mu = true$mus[s], sigma = true$sigmas[s], 
                                df = true$dfs[s])
    
    ### determine limits
    xmin_ind = min(sapply(c(f.x, f.x_true), function(x) min(which(x > 0.1))))
    xmax_ind = max(sapply(c(f.x, f.x_true), function(x) max(which(x > 0.1))))
    if(xmin_ind == 1){
      xmin = xmin * 2
    } else if(xmin_ind > 0.1*length.out){
      xmin = xmin / 1.5
    } else if(xmax_ind == length.out){
      xmax = xmax * 2
    } else if(xmax_ind < 0.9*length.out){
      xmax = xmax / 1.5
    } else {
      break
    }
  }
  ylim = round(c(0, max(sapply(f.x, max))),1)
  xlim = c(xmin, xmax)
  
  ### define x range and initialize plot
  plot(0, type = "n", xlim = xlim, ylim = ylim)
  
  ### plot densities
  for(s in 1:controls$states[1]){
    lines(x, f.x[[s]])
    lines(x, f.x_true[[s]], lty = 2)
    lines(x, f.x[[s]], col = colors[s], lty = 1, lwd = 2)
    if(!is.null(true))
      lines(x, f.x_true[[s]], lty = 2, col = colors[s], lwd = 2)
  }
  
}