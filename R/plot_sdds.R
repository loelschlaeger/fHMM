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

plot_sdds <- function(est, true = NULL, controls, colors) {
  
  ### check input
  stopifnot(class(est) == "fHMM_parameters")
  stopifnot(is.null(true) || class(true) == "fHMM_parameters")
  stopifnot(class(controls) == "fHMM_controls")
  stopifnot(class(colors) == "fHMM_colors")
  
  ### reset of 'par' settings
  oldpar <- par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(oldpar)))

  ### define densities
  density <- function(name, x, sigma, mu, df) {
    if (name == "t") {
      (1 / sigma) * dt(x = (x - mu) / sigma, df = df)
    } else if (name == "gamma") {
      dgamma(x = x, shape = mu^2 / sigma^2, scale = sigma^2 / mu)
    } else {
      stop()
    }
  }
  
  ### helper function to plot sdds
  helper_sdds = function(name, nstates, colors, main, est, true = NULL) {

    ### define x-limits
    xmin <- min(est$mus - 3 * est$sigmas)
    xmax <- max(est$mus + 3 * est$sigmas)
    if(!is.null(true)){
      xmin <- min(xmin, min(true$mus - 3 * true$sigmas))
      xmax <- max(xmax, max(true$mus + 3 * true$sigmas))
    }
    if (name == "gamma") {
      xmin <- 0.01
    }
  
    ### compute densities
    length.out <- 1e4
    x <- seq(from = xmin, to = xmax, length.out = length.out)
    f.x <- list()
    for (s in 1:nstates) {
      f.x[[s]] <- density(
        name = name, x = x, mu = est$mus[s], sigma = est$sigmas[s], 
        df = est$dfs[s]
      )
    }
    f.x_true <- list()
    if (!is.null(true)) {
      for (s in 1:nstates) {
        f.x_true[[s]] <- density(
          name = name, x = x, mu = true$mus[s], sigma = true$sigmas[s],
          df = true$dfs[s]
        )
      }
    }
    ylim <- max(round(c(0, max(sapply(f.x, max))), 1),100)
  
    ### define x range and initialize plot
    plot(0,
      type = "n", xlim = c(xmin, xmax),
      ylim = round(c(0, max(sapply(c(f.x, f.x_true), max))), 1),
      xlab = "", ylab = "", main = main
    )
  
    ### plot densities
    for (s in 1:nstates) {
      lines(x, f.x[[s]], col = colors[s], lty = 1, lwd = 2)
      if (!is.null(true)) {
        lines(x, f.x_true[[s]], lty = 2, col = colors[s], lwd = 2)
      }
    }
  
    ### add legend
    if (!is.null(true)) {
      legend("topright", c("estimated", "true"), lwd = 2, lty = 1:2)
    }
    
  }
  
  ### plot sdds
  if(controls$hierarchy){
    layout(matrix(c(rep(1, controls$states[1]), (1:controls$states[1])+1), 
                  nrow = 2, byrow = TRUE))
  }
  helper_sdds(name = est$sdds[[1]]$name, nstates = controls$states[1], 
              colors = colors, 
              main = "State-dependent distributions",
              est = list("mus" = est$mus, "sigmas" = est$sigmas, 
                         "dfs" = est$dfs), 
              true = if(!is.null(true)){
                list("mus" = true$mus, "sigmas" = true$sigmas, 
                     "dfs" = true$dfs)
                } else {
                  NULL
                  }
              )
  if(controls$hierarchy){
    for(s in 1:controls$states[1]){
      helper_sdds(name = est$sdds[[2]]$name, nstates = controls$states[2], 
                  colors = colors, 
                  main = paste("Coarse-scale state",s),
                  est = list("mus" = est$mus_star[[s]], 
                             "sigmas" = est$sigmas_star[[s]], 
                             "dfs" = est$dfs_star[[s]]), 
                  true = if(!is.null(true)){
                    list("mus" = true$mus_star[[s]], 
                         "sigmas" = true$sigmas_star[[s]], 
                         "dfs" = true$dfs_star[[s]])
                  } else {
                    NULL
                  }
                  )
    }
  }
}
