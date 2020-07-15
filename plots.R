hhmm_visual = function(data,est,states,controls){
  
  # unzip data, estimates and states
  T = controls$T
  T_star = controls$T_star
  cs_obs = data$cs_obs
  fs_obs = data$fs_obs
  close = data$close
  date = data$date
  pars = est$thetaFull
  cs_s_s = states$cs_states
  cs_s = rep(states$cs_states,each=T_star)
  fs_s = states$fs_states
  
  # define colours
  alpha = 160
  c1.1 = rgb(0,200,0,alpha,maxColorValue = 255) #hell grün
  c1.2 = rgb(0,110,0,alpha,maxColorValue = 255) #dunkel grün
  c2.1 = rgb(255,240,00,alpha,maxColorValue = 255) #hell gelb
  c2.2 = rgb(255,180,00,alpha,maxColorValue = 255) #dunkel gelb
  c3.1 = rgb(255,0,0,alpha,maxColorValue = 255)	#hell rot
  c3.2 = rgb(140,0,0,alpha,maxColorValue = 255)	#dunkel rot
  colours = matrix(c(c1.1,c1.2,c2.1,c2.2,c3.1,c3.2),nrow=3,ncol=2,byrow=TRUE)
  ordering = order(pars$mus,decreasing=TRUE)
  
  # state dependent distributions
  pdf(paste0("models/",controls$modelName,"_state_dep_distr.pdf"), width=10, height=6)
  par(mfrow=c(1,3),las=1,mar=c(4,4,4,1))
  x = seq(-0.1,0.1,by=0.0001)
  xmin = -0.1
  xmax = 0.1
  ymin = 0
  ymax = 100
  lwd = 3
  for(st in ordering){
    sdd = function(n,x) {(1/pars$sigmas_star[[st]][n])*dt((x-pars$mus_star[[st]][n])/pars$sigmas_star[[st]][n],pars$dfs_star[[st]][n])}
    hist(fs_obs[cs_s==st],prob=TRUE,breaks=40,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",cex.main=1.5,
         main=paste("coarse-scale state",which(ordering==st)),xlab="",ylab="",xaxt="n",yaxt="n")
    axis(1,seq(xmin,xmax,by=0.1))
    axis(2,seq(0,80,by=20))
    title(ylab="density",line=3,cex.lab=1.2)
    title(xlab="log-return",line=2.5,cex.lab=1.2)
    legend("topleft",legend=c("fine-scale state 1","fine-scale state 2"),col=colours[which(ordering==st),],lwd=lwd,cex=1.25, pt.cex = 1)
    lines(x,sdd(which.max(pars$mus_star[[st]]),x),col=colours[which(ordering==st),1],lwd=lwd)
    lines(x,sdd(which.min(pars$mus_star[[st]]),x),col=colours[which(ordering==st),2],lwd=lwd)
  }
  dev.off()
  
  ## decoded time series
  pdf(paste0("models/",controls$modelName,"_decoded_ts.pdf"), width=15, height=9)
  par(mfrow=c(1,1),las=1,mar=c(4,6,0.5,6))
  xmin = as.Date(controls$t_min)
  xmax = as.Date(controls$t_max)
  ymin = floor(min(close)/1000)*1000 - (ceiling(max(close)/1000)*1000-floor(min(close)/1000)*1000)
  ymax = ceiling(max(close)/1000)*1000
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
  
  ## pseudo-residuals
  pseudos_cs = numeric(T)
  pseudos_fs = numeric(T*T_star)
  for(i in 1:T){
    pseudos_cs[i] = qnorm(pt((cs_obs[i]-pars$mus[cs_s_s[i]])/pars$sigmas[cs_s_s[i]],pars$dfs[cs_s_s[i]]))
  }
  for(i in 1:(T*T_star)){
    pseudos_fs[i] = qnorm(pt((fs_obs[i]-pars$mus_star[[cs_s[i]]][fs_s[i]])/pars$sigmas_star[[cs_s[i]]][fs_s[i]],pars$dfs_star[[cs_s[i]]][fs_s[i]]))
  }
  pdf(paste0("models/",controls$modelName,"_pseudos.pdf"), width=15, height=6)
  par(mfrow = c(2,4), mar=c(5, 5, 3, 3) + 0.1, las=1,cex.lab=1.5, cex.main=1.5)
  plot(pseudos_cs,ylim=c(-3,3),main="Index plot",ylab="PR CS")
  hist(pseudos_cs,freq=FALSE,breaks=15,col="lightgrey",ylim=c(0,0.5),xlim=c(-3,3),main="Histogram w/ N(0;1)-density",xlab="PR CS")
  x <- seq(-4,4,0.01)
  curve(dnorm(x),add=TRUE,lwd=2)
  qqnorm(pseudos_cs[is.finite(pseudos_cs)],ylim=c(-3,3),xlim=c(-3,3),main="Normal Q-Q plot")
  abline(a=0,b=1)
  acf(pseudos_cs,lag.max = 10,main="")
  title("Autocorrelation plot")
  
  plot(pseudos_fs,ylim=c(-4,4),main="",ylab="PR FS")
  hist(pseudos_fs,freq=FALSE,breaks=20,col="lightgrey",ylim=c(0,0.5),xlim=c(-4,4),main="",xlab="PR FS")
  x <- seq(-4,4,0.01)
  curve(dnorm(x),add=TRUE,lwd=2)
  qqnorm(pseudos_fs[is.finite(pseudos_fs)],ylim=c(-4,4),xlim=c(-4,4),main="")
  abline(a=0,b=1)
  acf(pseudos_fs[is.finite(pseudos_fs)],lag.max = 30,main="")
  dev.off()
  
}