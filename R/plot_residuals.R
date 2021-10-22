

plot_pr = function(residuals) {
  
  ### reset of 'par' settings
  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow = set_mfrow(4))
  
  residuals = residuals[!is.na(residuals) & is.finite(residuals)]
  plot(residuals,
       ylim = c(floor(min(residuals)),ceiling(max(residuals))),
       main = "Residual plot",
       ylab = "Pseudo-residuals",
       las = 1,
       pch = 3)
  hist(residuals,
       freq = FALSE,
       breaks = 25,
       col = "lightgrey",
       xlim = c(floor(min(residuals)),ceiling(max(residuals))),
       main = "Histogram with normal density",
       xlab = "Pseudo-residuals",
       las = 1)
  x = seq(floor(min(residuals)),ceiling(max(residuals)),0.01)
  curve(dnorm(x),add=TRUE,lwd=2)
  qqnorm(residuals,
         ylim = c(floor(min(residuals)),ceiling(max(residuals))),
         xlim = c(floor(min(residuals)),ceiling(max(residuals))),
         main = "Normal Q-Q plot", 
         ylab = "Quantiles of pseudo-residuals", 
         xlab = "Normal quantiles",
         las = 1,
         pch = 20)
  abline(a=0,b=1)
  acf(residuals,
      main = "Autocorrelation plot",
      ylab = "Autocorrelation of pseudo-residuals",
      xlab = "Lag",
      las = 1)  
}