### creates graphics of model results

visual = function(data,fit,decoding,controls,labels=NULL){
  if(is.null(controls[["controls_checked"]])) stop("'controls' invalid",call.=FALSE)
  if(controls[["sim"]] & !is.null(labels)) warning("Entries of 'labels' will be ignored since 'data' was simulated.",call.=FALSE)
  
  states = controls[["states"]]
  attach(fit[["thetaList"]])
  on.exit(detach(fit[["thetaList"]]))
  
  ### define colours
  var.col = function(col,n) colorRampPalette(c("white",col,"black"))(n+2)[2:(n+1)]
  base.col = function(n) colorRampPalette(c("darkgreen","green","yellow","orange","red","darkred"))(n)
  col.alpha = function(col,alpha=0.5) adjustcolor(col,alpha)
  colors = matrix(0,nrow=states[1],ncol=states[2]+1)
  colors[,1] = col.alpha(base.col(states[1]))
  for(s in seq_len(states[1])) colors[s,-1] = col.alpha(var.col(colors[s,1],states[2]))
  
  ### state dependent distributions
  if(controls[["model"]]=="HMM"){
    pdf(paste0("models/",controls[["model_name"]],"/sdd.pdf"), width=9, height=7)
    sdd = function(s,x) {(1/sigmas[s])*dt((x-mus[s])/sigmas[s],dfs[s])}
    lwd = 3
    xmin = -0.1; xmax = 0.1; x = seq(xmin,xmax,0.0001)
    ymin = 0; ymax = ceiling(max(sdd(1,x)))
    hist(data$observations,prob=TRUE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",xaxt="n",yaxt="n",xlab="",ylab="",main="")
    axis(1,seq(xmin,xmax,by=0.1)); axis(2,seq(ymin,ymax,by=20))
    title(main="State-dependent distributions",xlab="log-return",ylab="density")
    legend("topleft",legend=paste("state",seq_len(states[1])),col=colors[,1],lwd=lwd,pt.cex=1,cex=1.5)
    for(s in seq_len(states[1])){
      lines(x,sdd(s,x),col=colors[s,1],lwd=lwd)
    }
    dev.off()
  }
  
  if(controls[["model"]]=="HHMM"){
    pdf(paste0("models/",controls[["model_name"]],"/sdd.pdf"), width=9, height=7)
    sdd = function(s,x) {(1/sigmas[s])*dt((x-mus[s])/sigmas[s],dfs[s])}
    lwd = 3
    xmin = -0.1; xmax = 0.1; x = seq(xmin,xmax,0.0001)
    ymin = 0; ymax = ceiling(max(sdd(1,x)))
    hist(data$observations[,1],prob=TRUE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",xaxt="n",yaxt="n",xlab="",ylab="",main="")
    axis(1,seq(xmin,xmax,by=0.1)); axis(2,seq(ymin,ymax,by=40))
    title(main="State-dependent distributions",xlab="log-return",ylab="density")
    legend("topleft",legend=paste("coarse-scale state",seq_len(states[1])),col=colors[,1],lwd=lwd,pt.cex=1,cex=1.5)
    for(s in seq_len(states[1])){
      lines(x,sdd(s,x),col=colors[s,1],lwd=lwd)
    }
    
    sdd = function(cs,fs,x) {(1/sigmas_star[[cs]][fs])*dt((x-mus_star[[cs]][fs])/sigmas_star[[cs]][fs],dfs_star[[cs]][fs])}
    lwd = 3
    xmin = -0.1; xmax = 0.1; x = seq(xmin,xmax,0.0001)
    ymin = 0; ymax = ceiling(max(sdd(1,1,x)))
    for(cs in seq_len(states[1])){
      hist(as.vector(data$observations[,-1]),prob=TRUE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",xaxt="n",yaxt="n",xlab="",ylab="",main="")
      axis(1,seq(xmin,xmax,by=0.1)); axis(2,seq(ymin,ymax,by=20))
      title(main=paste("State-dependent distributions conditional on coarse-scale state",cs),xlab="log-return",ylab="density")
      legend("topleft",legend=paste("fine-scale state",seq_len(states[2])),col=colors[cs,-1],lwd=lwd,pt.cex=1,cex=1.5)
      for(fs in seq_len(states[2])){
        lines(x,sdd(cs,fs,x),col=colors[cs,fs+1],lwd=lwd)
      }
    }
    dev.off()
  }
  
  if(FALSE){
  ### decoded time series
  pdf(paste0("models/",controls$modelName,"_decoded_ts.pdf"), width=19, height=9)
  par(mfrow=c(1,1),las=1,mar=c(4,6,0.5,6))
  xmin = as.Date(controls$t_min)
  xmax = as.Date(controls$t_max)
  ymin = floor(min(close)/1000)*1000 - (ceiling(max(close)/1000)*1000-floor(min(close)/1000)*1000)/1.5
  ymax = max(close)#ceiling(max(close)/1000)*1000
  plot(date,close,type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="grey",xlab="",ylab="",xaxt="n",yaxt="n",cex.lab=2, cex.main=2)
  par(las=3)
  mtext("closing price", side=4, line=4, at=mean(close), cex=1.5)
  par(las=1)
  mtext("year",side=1,line=3,cex=1.5)
  markdates = seq(xmin,xmax,by="year")
  markdates = markdates[1:length(markdates)%%2==1]
  axis(1, markdates, format(markdates, "%Y"))
  axis(4, seq(floor(min(close)/1000)*1000,ceiling(max(close)/1000)*1000,length.out=5))
  legend("topleft",bty="n",pch=19,legend=c("coarse-scale state 1, fine-scale state 1",
                                           "coarse-scale state 1, fine-scale state 2",
                                           "coarse-scale state 2, fine-scale state 1",
                                           "coarse-scale state 2, fine-scale state 2",
                                           "coarse-scale state 3, fine-scale state 1",
                                           "coarse-scale state 3, fine-scale state 2"),
         col=as.vector(t(colours)),cex=1.25)
  for(st in ordering){
    points(date[cs_s==st&fs_s==which.max(pars$mus_star[[st]])],close[cs_s==st&fs_s==which.max(pars$mus_star[[st]])],col=colours[which(ordering==st),1],pch=20)
    points(date[cs_s==st&fs_s==which.min(pars$mus_star[[st]])],close[cs_s==st&fs_s==which.min(pars$mus_star[[st]])],col=colours[which(ordering==st),2],pch=20)
  }
  text(as.Date("2008-9-15"),max(close)*0.66,"bankruptcy of Lehman Brothers \nSeptember 15, 2008",pos=4,cex=1.5)
  abline(v=as.Date("2008-09-15"),lty=2,lwd=2)
  
  par(new=TRUE)
  par(mfrow=c(1,1),las=1)
  xmin = as.Date("2000-1-1")
  xmax = as.Date("2020-1-1")
  ymin = min(fs_obs)
  ymax = max(fs_obs) + 0.2
  plot(date,fs_obs,type="h",col="grey",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  par(las=3)
  mtext("log-return", side=2, line=4, at=0, cex=1.5)
  par(las=1)
  markdates = seq(xmin,xmax,by="year")
  markdates = markdates[1:length(markdates)%%2==1]
  axis(2, seq(-0.08,0.08,by=0.04))
  for(st in ordering){
    points(date[cs_s==st&fs_s==which.max(pars$mus_star[[st]])],fs_obs[cs_s==st&fs_s==which.max(pars$mus_star[[st]])],col=colours[which(ordering==st),1],pch=20)
    points(date[cs_s==st&fs_s==which.min(pars$mus_star[[st]])],fs_obs[cs_s==st&fs_s==which.min(pars$mus_star[[st]])],col=colours[which(ordering==st),2],pch=20)
  }
  dev.off()
  }
  
  ### pseudo-residuals
  if(controls[["model"]]=="HMM"){
    T = length(data$observations)
    pseudos = numeric(T)
    for(t in 1:T){
      pseudos[t] = qnorm(pt((data$observations[t]-mus[decoding[t]])/sigmas[decoding[t]],dfs[decoding[t]]))
    }
    pdf(paste0("models/",controls[["model_name"]],"/pseudos.pdf"), width=9, height=7)
      plot(pseudos,ylim=c(floor(min(pseudos)),ceiling(max(pseudos))),main="Index plot",ylab="PR")
      hist(pseudos,freq=FALSE,breaks=15,col="lightgrey",xlim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="Histogram w/ N(0;1)-density",xlab="PR"); x=seq(floor(min(pseudos)),ceiling(max(pseudos)),0.01); curve(dnorm(x),add=TRUE,lwd=2)
      qqnorm(pseudos[is.finite(pseudos)],ylim=c(floor(min(pseudos)),ceiling(max(pseudos))),xlim=c(floor(min(pseudos)),ceiling(max(pseudos))),main="normal Q-Q plot", ylab="PR quantiles", xlab="N(0;1) quantiles"); abline(a=0,b=1)
      acf(pseudos,lag.max = 10,main="", ylab="ACF PR", xlab="lag"); title("autocorrelation plot")
    dev.off()
  }
  if(controls[["model"]]=="HHMM"){
    T = dim(data$observations)[1]
    T_star = dim(data$observations)[2]-1
    pseudos_cs = numeric(T)
    pseudos_fs = numeric(T*T_star)
    for(t in 1:T){
      pseudos_cs[t] = qnorm(pt((data$observations[t,1]-mus[decoding[t,1]])/sigmas[decoding[t,1]],dfs[decoding[t,1]]))
    }
    decoding_cs = rep(decoding[,1],T_star)
    decoding_fs = as.vector(t(decoding[,-1]))
    for(t in 1:(T*T_star)){
      pseudos_fs[t] = qnorm(pt((as.vector(t(data$observations[,-1]))[t]-mus_star[[decoding_cs[t]]][decoding_fs[t]])/sigmas_star[[decoding_cs[t]]][decoding_fs[t]],dfs_star[[decoding_cs[t]]][decoding_fs[t]]))
    }
    pdf(paste0("models/",controls[["model_name"]],"/pseudos.pdf"), width=9, height=7)
      plot(pseudos_cs,ylim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="Index plot",ylab="PR CS")
      plot(pseudos_fs,ylim=c(floor(min(pseudos_fs)),ceiling(max(pseudos_fs))),main="Index plot",ylab="PR FS")
      hist(pseudos_cs,freq=FALSE,breaks=15,col="lightgrey",xlim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="Histogram w/ N(0;1)-density",xlab="PR CS"); x = seq(floor(min(pseudos_cs)),ceiling(max(pseudos_cs)),0.01); curve(dnorm(x),add=TRUE,lwd=2)
      hist(pseudos_fs,freq=FALSE,breaks=20,col="lightgrey",xlim=c(floor(min(pseudos_fs)),ceiling(max(pseudos_fs))),main="Histogram w/ N(0;1)-density",xlab="PR FS"); x = seq(floor(min(pseudos_fs)),ceiling(max(pseudos_fs)),0.01); curve(dnorm(x),add=TRUE,lwd=2)
      qqnorm(pseudos_cs[is.finite(pseudos_cs)],ylim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),xlim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="normal Q-Q plot", ylab="PR CS quantiles", xlab="N(0;1) quantiles"); abline(a=0,b=1)
      qqnorm(pseudos_fs[is.finite(pseudos_fs)],ylim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),xlim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="normal Q-Q plot", ylab="PR FS quantiles", xlab="N(0;1) quantiles"); abline(a=0,b=1)
      acf(pseudos_cs[is.finite(pseudos_cs)],lag.max = 10,main="", ylab="ACF PR CS", xlab="lag"); title("autocorrelation plot")
      acf(pseudos_fs[is.finite(pseudos_fs)],lag.max = 30,main="", ylab="ACF PR CS", xlab="lag"); title("autocorrelation plot")
    dev.off()
  }
  
}