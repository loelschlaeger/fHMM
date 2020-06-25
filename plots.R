hhmm_visual = function(data,est,states){
  
  # unzip data, estimates and states
  cs_obs = data$cs_obs
  fs_obs = data$fs_obs
  close = data$close
  date = data$date
  pars = est$thetaFull
  cs_s = states$cs_states
  fs_s = states$fs_states
  
  # define colours
  alpha = 160
  c1.1 = rgb(0,200,0,alpha,maxColorValue = 255) #hell grün
  c1.2 = rgb(0,110,0,alpha,maxColorValue = 255) #dunkel grün
  c2.1 = rgb(255,240,00,alpha,maxColorValue = 255) #hell gelb
  c2.2 = rgb(255,180,00,alpha,maxColorValue = 255) #dunkel gelb
  c3.1 = rgb(255,0,0,alpha,maxColorValue = 255)	#hell rot
  c3.2 = rgb(140,0,0,alpha,maxColorValue = 255)	#dunkel rot
  
  # state dependent distributions
  pdf(paste0("models/",controls$modelName,"_state_dep_distr.pdf"), width=10, height=6)
  par(mfrow=c(1,3),las=1,mar=c(4,4,4,1))
  x = seq(-0.8,0.8,by=0.0001)
  xmin = -0.06
  xmax = 0.06
  ymin = 0
  ymax = 85
  lwd = 3
  
  hist(fs_obs[cs_s==1],prob=TRUE,breaks=40,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",cex.main=1.5,main="coarse-scale state 1",xlab="",ylab="",xaxt="n",yaxt="n")
  axis(1,seq(-0.05,0.05,by=0.05))
  axis(2,seq(0,60,by=20))
  title(ylab="density",line=3,cex.lab=1.2)
  title(xlab="log-return",line=2.5,cex.lab=1.2)
  legend("topleft",legend=c("fine-scale state 1","fine-scale state 2"),col=c(c1.1,c1.2),lwd=lwd,cex=1.25, pt.cex = 1)
  x = seq(-0.06,0.06,by=0.0001)
  lines(x,(1/pars$sigmas_star[[1]][1])*dt((x-pars$mus_star[[1]][1])/pars$sigmas_star[[1]][1],pars$dfs_star[[1]][1]),col=c1.1,lwd=lwd)
  lines(x,(1/pars$sigmas_star[[1]][2])*dt((x-pars$mus_star[[1]][2])/pars$sigmas_star[[1]][2],pars$dfs_star[[1]][2]),col=c1.2,lwd=lwd)
  
  hist(fs_obs[cs_s==2],prob=TRUE,breaks=40,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",cex.main = 1.5,main="coarse-scale state 2",xlab="",ylab="",xaxt="n",yaxt="n")
  axis(1,seq(-0.05,0.05,by=0.05))
  axis(2,seq(0,60,by=20))
  title(ylab="density",line=3,cex.lab=1.2)
  title(xlab="log-return",line=2.5,cex.lab=1.2)
  legend("topleft",legend=c("fine-scale state 1","fine-scale state 2"),col=c(c2.1,c2.2),lwd=lwd,cex=1.25, pt.cex = 1)
  x = seq(-0.06,0.06,by=0.0001)
  lines(x,(1/pars$sigmas_star[[2]][1])*dt((x-pars$mus_star[[2]][1])/pars$sigmas_star[[2]][1],pars$dfs_star[[2]][1]),col=c2.1,lwd=lwd)
  lines(x,(1/pars$sigmas_star[[2]][2])*dt((x-pars$mus_star[[2]][2])/pars$sigmas_star[[2]][2],pars$dfs_star[[2]][2]),col=c2.2,lwd=lwd)
  
  hist(fs_obs[cs_s==3],prob=TRUE,breaks=20,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",cex.main = 1.5,main="coarse-scale state 3",xlab="",ylab="",xaxt="n",yaxt="n")
  axis(1,seq(-0.1,0.1,by=0.1))
  axis(2,seq(0,60,by=20))
  title(ylab="density",line=3,cex.lab=1.2)
  title(xlab="log-return",line=2.5,cex.lab=1.2)
  legend("topleft",legend=c("fine-scale state 1","fine-scale state 2"),col=c(c3.1,c3.2),lwd=lwd,cex=1.25, pt.cex = 1)
  x <- seq(-0.1,0.1,by=0.0001)
  lines(x,(1/pars$sigmas_star[[3]][1])*dt((x-pars$mus_star[[3]][1])/pars$sigmas_star[[3]][1],pars$dfs_star[[3]][1]),col=c3.1,lwd=lwd)
  lines(x,(1/pars$sigmas_star[[3]][2])*dt((x-pars$mus_star[[3]][2])/pars$sigmas_star[[3]][2],pars$dfs_star[[3]][2]),col=c3.2,lwd=lwd)
  dev.off()
  
  ## decoded time series
  pdf(paste0("models/",controls$modelName,"_decoded_ts.pdf"), width=15, height=9)
  par(mfrow=c(1,1),las=1,mar=c(4,6,0.5,6))
  xmin = as.Date(controls$t_min)
  xmax = as.Date(controls$t_max)
  ymin <- -11000
  ymax <- 14000
  plot(date,close,type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="grey",xlab="",ylab="",xaxt="n",yaxt="n",cex.lab=2, cex.main=2)
  par(las=3)
  mtext("closing price", side=4, line=4, at=8000, cex=1.5)
  par(las=1)
  mtext("year",side=1,line=3,cex=1.5)
  markdates <- seq(xmin,xmax,by="year")
  markdates <- markdates[1:length(markdates)%%2==1]
  axis(1, markdates, format(markdates, "%Y"))
  axis(4, seq(2000,14000,by=2000))
  legend("topleft",bty="n",pch=19,legend=c("coarse-scale state 1, fine-scale state 1","coarse-scale state 1, fine-scale state 2","coarse-scale state 2, fine-scale state 1","coarse-scale state 2, fine-scale state 2","coarse-scale state 3, fine-scale state 1","coarse-scale state 3, fine-scale state 2"),col=c(c1.1,c1.2,c2.1,c2.2,c3.1,c3.2),cex=1.25)
  
  points(date[cs_s==1&fs_s==2],close[cs_s==1&fs_s==2],col=c1.2,pch=20)
  points(date[cs_s==1&fs_s==1],close[cs_s==1&fs_s==1],col=c1.1,pch=20)
  
  points(date[cs_s==2&fs_s==2],close[cs_s==2&fs_s==2],col=c2.2,pch=20)
  points(date[cs_s==2&fs_s==1],close[cs_s==2&fs_s==1],col=c2.1,pch=20)
  
  points(date[cs_s==3&fs_s==1],close[cs_s==3&fs_s==1],col=c3.1,pch=20)
  points(date[cs_s==3&fs_s==2],close[cs_s==3&fs_s==2],col=c3.2,pch=20)
  
  par(new=TRUE)
  par(mfrow=c(1,1),las=1)
  xmin <- as.Date("2000-1-1")
  xmax <- as.Date("2020-1-1")
  ymin <- -0.08
  ymax <- 0.25
  plot(date,fs_obs,type="h",col="grey",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax))
  par(las=3)
  mtext("log-return", side=2, line=4, at=0, cex=1.5)
  par(las=1)
  markdates <- seq(xmin,xmax,by="year")
  markdates <- markdates[1:length(markdates)%%2==1]
  axis(2, seq(-0.08,0.08,by=0.04))
  
  points(date[cs_s==1&fs_s==2],fs_obs[cs_s==1&fs_s==2],col=c1.2,pch=20)
  points(date[cs_s==1&fs_s==1],fs_obs[cs_s==1&fs_s==1],col=c1.1,pch=20)
  
  points(date[cs_s==2&fs_s==2],fs_obs[cs_s==2&fs_s==2],col=c2.2,pch=20)
  points(date[cs_s==2&fs_s==1],fs_obs[cs_s==2&fs_s==1],col=c2.1,pch=20)
  
  points(date[cs_s==3&fs_s==1],fs_obs[cs_s==3&fs_s==1],col=c3.1,pch=20)
  points(date[cs_s==3&fs_s==2],fs_obs[cs_s==3&fs_s==2],col=c3.2,pch=20)
  dev.off()
  
  ## pseudo-residuals
  
}