### creates graphics of model results

visual = function(data,fit,decoding,controls,labels=NULL){
  
  ### pre-checks
  if(is.null(controls[["controls_checked"]])) stop("'controls' invalid",call.=FALSE)
  if(controls[["sim"]] & !is.null(labels)){
    warning("Entries of 'labels' will be ignored since 'data' was simulated.",call.=FALSE)
  } else if(!controls[["sim"]] & !is.null(labels) & length(labels[["dates"]])!=length(labels[["names"]])){
    stop("'dates' and 'names' in 'labels' must be of the same length.")
  }
  
  ### extract parameters
  states = controls[["states"]]
  if(controls[["model"]]=="HMM"){
    T = length(data$observations)
  }
  if(controls[["model"]]=="HHMM"){
    T = dim(data$observations)[1]
    T_star = dim(data$observations)[2]-1
    decoding_cs = rep(decoding[,1],T_star)
    decoding_fs = as.vector(t(decoding[,-1]))
    cs_observations = data[["observations"]][,1]
    fs_observations = as.vector(t(data[["observations"]][,-1]))
  }
  attach(fit[["thetaList"]])
  on.exit(detach(fit[["thetaList"]]))
  
  ### define colours
  var.col = function(col,n) colorRampPalette(c("white",col,"black"))(n+2)[2:(n+1)]
  base.col = function(n) colorRampPalette(c("darkgreen","green","yellow","orange","red","darkred"))(n)
  col.alpha = function(col,alpha=0.5) adjustcolor(col,alpha)
  colors = matrix(0,nrow=states[1],ncol=states[2]+1)
  colors[,1] = col.alpha(base.col(states[1]))
  for(s in seq_len(states[1])) colors[s,-1] = col.alpha(var.col(colors[s,1],states[2]))
  
  ### define layout
  legend_layout = list(cex=1.25,x="topleft",bg="white")
  plot_layout = list()
  
  ### state dependent distributions
  filename = paste0("models/",controls[["model_name"]],"/sdd.pdf")
  if(controls[["overwrite"]]==FALSE & file.exists(filename)){
    warning(paste0("Cannot create a plot of the state-dependent distributions because the path '",filename,"' already exists and you chose not to overwrite."),call.=FALSE)
  } else {
    if(controls[["model"]]=="HMM"){
      pdf(filename, width=9, height=7)
      sdd = function(s,x) {(1/sigmas[s])*dt((x-mus[s])/sigmas[s],dfs[s])}
      lwd = 3
      xmin = -0.1; xmax = 0.1; x = seq(xmin,xmax,0.0001)
      ymin = 0; ymax = ceiling(max(sdd(1,x)))
      hist(data$observations,prob=TRUE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",xaxt="n",yaxt="n",xlab="",ylab="",main="")
      axis(1,seq(xmin,xmax,by=0.1)); axis(2,seq(ymin,ymax,by=20))
      title(main="State-dependent distributions",xlab="log-return",ylab="density")
      do.call(legend,c(list(legend=paste("state",seq_len(states[1])),col=colors[,1],lwd=3),legend_layout))
      for(s in seq_len(states[1])){
        lines(x,sdd(s,x),col=colors[s,1],lwd=lwd)
      }
      invisible(dev.off())
    }
    if(controls[["model"]]=="HHMM"){
      pdf(filename, width=9, height=7)
      sdd = function(s,x) {(1/sigmas[s])*dt((x-mus[s])/sigmas[s],dfs[s])}
      lwd = 3
      xmin = -0.1; xmax = 0.1; x = seq(xmin,xmax,0.0001)
      ymin = 0; ymax = ceiling(max(sdd(1,x)))
      hist(cs_observations,prob=TRUE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",xaxt="n",yaxt="n",xlab="",ylab="",main="")
      axis(1,seq(xmin,xmax,by=0.1)); axis(2,seq(ymin,ymax,by=40))
      title(main="State-dependent distributions",xlab="log-return",ylab="density")
      do.call(legend,c(list(legend=paste("coarse-scale state",seq_len(states[1])),col=colors[,1],lwd=3),legend_layout))
      for(s in seq_len(states[1])){
        lines(x,sdd(s,x),col=colors[s,1],lwd=lwd)
      }
      sdd = function(cs,fs,x) 1/sigmas_star[[cs]][fs]*dt((x-mus_star[[cs]][fs])/sigmas_star[[cs]][fs],dfs_star[[cs]][fs])
      lwd = 3
      xmin = -0.1; xmax = 0.1; x = seq(xmin,xmax,0.0001)
      ymin = 0; ymax = ceiling(max(sdd(1,1,x)))
      for(cs in seq_len(states[1])){
        hist(fs_observations,prob=TRUE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",xaxt="n",yaxt="n",xlab="",ylab="",main="")
        axis(1,seq(xmin,xmax,by=0.1)); axis(2,seq(ymin,ymax,by=20))
        title(main=paste("State-dependent distributions conditional on coarse-scale state",cs),xlab="log-return",ylab="density")
        do.call(legend,c(list(legend=paste("fine-scale state",seq_len(states[2])),col=colors[cs,-1],lwd=3),legend_layout))
        for(fs in seq_len(states[2])){
          lines(x,sdd(cs,fs,x),col=colors[cs,fs+1],lwd=lwd)
        }
      }
      invisible(dev.off())
    }
  }
  
  ### decoded time series
  filename = paste0("models/",controls[["model_name"]],"/ts.pdf")
  if(controls[["overwrite"]]==FALSE & file.exists(filename)){
    warning(paste0("Cannot create a plot of the decoded time series because the path '",filename,"' already exists and you chose not to overwrite."),call.=FALSE)
  } else {
    pdf(filename, width=19, height=9)
    par(mfrow=c(1,1),las=1,mar=c(4,6,0.5,6))
    if(controls[["sim"]]){
      xmin = 1
      xmax = ifelse(controls[["model"]]=="HMM",length(data$observations),dim(data$observations)[1]*(dim(data$observations)[2]-1))
    }
    if(!controls[["sim"]]){
      xmin = as.Date(head(data$dates,n=1))
      xmax = as.Date(tail(data$dates,n=1)) 
    }
    ymax = max(data$closes); ymin = -ymax
    plot(data$dates,data$closes,type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="grey",xlab="",ylab="",xaxt="n",yaxt="n",cex.lab=2, cex.main=2)
    par(las=3)
    mtext("closing price", side=4, line=4, at=median(data$closes), cex=1.5)
    par(las=1)
    mtext("year",side=1,line=3,cex=1.5)
    markdates = seq(xmin,xmax,by="year"); markdates = markdates[1:length(markdates)%%2==1]
    axis(1, markdates, format(markdates, "%Y"))
    axis(4, round(seq(min(data$closes),max(data$closes),length.out=5),digits=-1))
    if(controls[["model"]]=="HMM"){
      for(s in seq_len(states[1])){
        points(data$dates[decoding==s],data$closes[decoding==s],col=colors[s,1],pch=20)
      }
      do.call(legend,c(list(legend=paste("state",seq_len(states[1])),col=colors[,1],pch=19),legend_layout))
    }
    if(controls[["model"]]=="HHMM"){
      for(cs in seq_len(states[1])){
        for(fs in seq_len(states[2])){
          points(data$dates[decoding_cs==cs&decoding_fs==fs],data$closes[decoding_cs==cs&decoding_fs==fs],col=colors[cs,fs+1],pch=20)
        }
      }
      eg = expand.grid(seq_len(states[2]),seq_len(states[1]))
      do.call(legend,c(list(legend=paste0("coarse-scale state ",eg[,2],", fine-scale state ",eg[,1]),col=as.vector(t(colors[,-1])),pch=19),legend_layout))
    }
    par(new=TRUE)
    par(mfrow=c(1,1),las=1)
    if(controls[["model"]]=="HMM"){
      ymin = min(data$observations); ymax = max(data$observations)
      plot(data$dates,data$observations,type="h",col="grey",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax*3))
    }
    if(controls[["model"]]=="HHMM"){
      ymin = min(fs_observations); ymax = max(fs_observations)
      plot(data$dates,fs_observations,type="h",col="grey",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax*3))
    }
    par(las=3)
    mtext("log-return", side=2, line=4, at=0, cex=1.5)
    par(las=1)
    axis(2, round(seq(ymin,ymax,length.out=5),2))
    if(controls[["model"]]=="HMM"){
      for(s in seq_len(states[1])){
        points(data$dates[decoding==s],data$observations[decoding==s],col=colors[s,1],pch=20)
      }
    }
    if(controls[["model"]]=="HHMM"){
      for(cs in seq_len(states[1])){
        for(fs in seq_len(states[2])){
          points(data$dates[decoding_cs==cs&decoding_fs==fs],fs_observations[decoding_cs==cs&decoding_fs==fs],col=colors[cs,fs+1],pch=20)
        }
      }
    }
    if(!controls[["sim"]] & !is.null(labels)){
      no_labels = length(labels[["dates"]])
      for(l in seq_len(no_labels)){
        abline(v=as.Date(labels[["dates"]][l]),lty=2,lwd=2); text(x=as.Date(labels[["dates"]][l]),y=ymin,l,pos=4,cex=1.5)
      }
      do.call(legend,c(list(x="topright",legend=paste0(seq_len(no_labels),": ",labels[["names"]])),legend_layout[names(legend_layout)!="x"]))
    }
    invisible(dev.off())
  }
  
  ### pseudo-residuals
  filename = paste0("models/",controls[["model_name"]],"/pseudos.pdf")
  if(controls[["overwrite"]]==FALSE & file.exists(filename)){
    warning(paste0("Cannot create a plot of the pseudo-residuals because the path '",filename,"' already exists and you chose not to overwrite."),call.=FALSE)
  } else {
    if(controls[["model"]]=="HMM"){
      pseudos = numeric(T)
      for(t in 1:T){
        pseudos[t] = qnorm(pt((data$observations[t]-mus[decoding[t]])/sigmas[decoding[t]],dfs[decoding[t]]))
      }
      pdf(filename, width=9, height=7)
        plot(pseudos,ylim=c(floor(min(pseudos)),ceiling(max(pseudos))),main="Index plot",ylab="PR")
        hist(pseudos,freq=FALSE,breaks=15,col="lightgrey",xlim=c(floor(min(pseudos)),ceiling(max(pseudos))),main="Histogram w/ N(0;1)-density",xlab="PR"); x=seq(floor(min(pseudos)),ceiling(max(pseudos)),0.01); curve(dnorm(x),add=TRUE,lwd=2)
        qqnorm(pseudos[is.finite(pseudos)],ylim=c(floor(min(pseudos)),ceiling(max(pseudos))),xlim=c(floor(min(pseudos)),ceiling(max(pseudos))),main="normal Q-Q plot", ylab="PR quantiles", xlab="N(0;1) quantiles"); abline(a=0,b=1)
        acf(pseudos,lag.max = 10,main="", ylab="ACF PR", xlab="lag"); title("autocorrelation plot")
      invisible(dev.off())
    }
    if(controls[["model"]]=="HHMM"){
      pseudos_cs = numeric(T)
      pseudos_fs = numeric(T*T_star)
      for(t in 1:T){
        pseudos_cs[t] = qnorm(pt((data$observations[t,1]-mus[decoding[t,1]])/sigmas[decoding[t,1]],dfs[decoding[t,1]]))
      }
      for(t in 1:(T*T_star)){
        pseudos_fs[t] = qnorm(pt((as.vector(t(data$observations[,-1]))[t]-mus_star[[decoding_cs[t]]][decoding_fs[t]])/sigmas_star[[decoding_cs[t]]][decoding_fs[t]],dfs_star[[decoding_cs[t]]][decoding_fs[t]]))
      }
      pdf(filename, width=9, height=7)
        plot(pseudos_cs,ylim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="Index plot",ylab="PR CS")
        plot(pseudos_fs,ylim=c(floor(min(pseudos_fs)),ceiling(max(pseudos_fs))),main="Index plot",ylab="PR FS")
        hist(pseudos_cs,freq=FALSE,breaks=15,col="lightgrey",xlim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="Histogram w/ N(0;1)-density",xlab="PR CS"); x = seq(floor(min(pseudos_cs)),ceiling(max(pseudos_cs)),0.01); curve(dnorm(x),add=TRUE,lwd=2)
        hist(pseudos_fs,freq=FALSE,breaks=20,col="lightgrey",xlim=c(floor(min(pseudos_fs)),ceiling(max(pseudos_fs))),main="Histogram w/ N(0;1)-density",xlab="PR FS"); x = seq(floor(min(pseudos_fs)),ceiling(max(pseudos_fs)),0.01); curve(dnorm(x),add=TRUE,lwd=2)
        qqnorm(pseudos_cs[is.finite(pseudos_cs)],ylim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),xlim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="normal Q-Q plot", ylab="PR CS quantiles", xlab="N(0;1) quantiles"); abline(a=0,b=1)
        qqnorm(pseudos_fs[is.finite(pseudos_fs)],ylim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),xlim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="normal Q-Q plot", ylab="PR FS quantiles", xlab="N(0;1) quantiles"); abline(a=0,b=1)
        acf(pseudos_cs[is.finite(pseudos_cs)],lag.max = 10,main="", ylab="ACF PR CS", xlab="lag"); title("autocorrelation plot")
        acf(pseudos_fs[is.finite(pseudos_fs)],lag.max = 30,main="", ylab="ACF PR CS", xlab="lag"); title("autocorrelation plot")
      invisible(dev.off())
    }
  }
    
  writeLines("Done with visualization.")
}
