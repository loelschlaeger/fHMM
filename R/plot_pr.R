plot_residuals = function() {
  pseudos = pseudos[!is.na(pseudos) & is.finite(pseudos)]
  plot(pseudos,
       ylim=c(floor(min(pseudos)),ceiling(max(pseudos))),
       main="Residual plot",
       ylab=paste("Pseudo-residuals",label_add),
       las=1,
       pch=3)
  hist(pseudos,
       freq=FALSE,
       breaks=25,
       col="lightgrey",
       xlim=c(floor(min(pseudos)),ceiling(max(pseudos))),
       main="Histogram with N(0;1)-density",
       xlab=paste("Pseudo-residuals",label_add),
       las=1)
  x = seq(floor(min(pseudos)),ceiling(max(pseudos)),0.01)
  curve(dnorm(x),add=TRUE,lwd=2)
  qqnorm(pseudos,
         ylim=c(floor(min(pseudos)),ceiling(max(pseudos))),
         xlim=c(floor(min(pseudos)),ceiling(max(pseudos))),
         main="Normal Q-Q plot", 
         ylab=paste("Quantiles of",label_add,"pseudo-residuals"), 
         xlab="N(0;1)-quantiles",
         las=1,
         pch=20)
  abline(a=0,b=1)
  acf(pseudos,
      main="Autocorrelation plot",
      ylab=paste("Autocorrelation of",label_add,"pseudo-residuals"),
      xlab="Lag",
      las=1)  
}