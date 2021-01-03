### creates graphics of model results

visual = function(data,fit,decoding,controls,labels=NULL){
  
  ### pre-checks
  if(is.null(controls[["controls_checked"]])) stop("'controls' invalid",call.=FALSE)
  if(controls[["sim"]] & !is.null(labels)){
    warning("Object 'labels' is ignored because 'data' is simulated.",call.=FALSE)
  } else if(!controls[["sim"]] & !is.null(labels) & length(labels[["dates"]])!=length(labels[["names"]])){
    stop("'dates' and 'names' in 'labels' must be of the same length.")
  }
  
  ### extract parameters
  states = controls[["states"]]
  if(controls[["model"]]=="HMM"){
    T = length(data[["logReturns"]])
  }
  if(controls[["model"]]=="HHMM"){
    T = dim(data[["logReturns"]])[1]
    T_star = dim(data[["logReturns"]])[2]-1
    decoding_cs = rep(decoding[,1],T_star)
    decoding_fs = as.vector(t(decoding[,-1]))
    cs_logReturns = data[["logReturns"]][,1]
    fs_logReturns = as.vector(t(data[["logReturns"]][,-1]))
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
  
  ### define legend layout
  legend_layout = list(cex=1.25,x="topleft",bg="white")
  
  ### rounding functions
  floor_dec = function(x,n) round(x-5*10^(-n-1),n)
  ceiling_dec = function(x,n) round(x+5*10^(-n-1),n)
  
  ### state dependent distributions
  filename = paste0("models/",controls[["id"]],"/sdd.pdf")
  if(controls[["overwrite"]]==FALSE & file.exists(filename)){
    warning(paste0("Cannot create a plot of the state-dependent distributions because the path '",filename,"' already exists and you chose not to overwrite."),call.=FALSE)
  } else {
    if(controls[["model"]]=="HMM"){
      pdf(filename, width=9, height=7)
      sdd = function(s,x) {(1/sigmas[s])*dt((x-mus[s])/sigmas[s],dfs[s])}
      lwd = 3
      xmin = floor_dec(min(data[["logReturns"]]),1); xmax = ceiling_dec(max(data[["logReturns"]]),1); x = seq(xmin,xmax,0.001)
      ymin = 0; ymax = ceiling(max(sapply(x,sdd,s=seq_len(states[1]))))
      hist(data[["logReturns"]],prob=TRUE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",xaxt="n",yaxt="n",xlab="",ylab="",main="")
      axis(1,trunc(seq(xmin,xmax,by=0.1)*10)/10); axis(2,seq(ymin,ymax,by=20),las=1)
      title(main="State-dependent distributions",xlab="Log-return",ylab="Density")
      do.call(legend,c(list(legend=paste("State",seq_len(states[1])),col=colors[,1],lwd=3),legend_layout))
      for(s in seq_len(states[1])){
        lines(x[sdd(s,x)>0.001],sdd(s,x)[sdd(s,x)>0.001],col=colors[s,1],lwd=lwd)
      }
      invisible(dev.off())
    }
    if(controls[["model"]]=="HHMM"){
      pdf(filename, width=9, height=7)
      sdd = function(s,x) {(1/sigmas[s])*dt((x-mus[s])/sigmas[s],dfs[s])}
      lwd = 3
      xmin = min(-0.1,floor_dec(min(cs_logReturns),1)); xmax = max(0.1,ceiling_dec(max(cs_logReturns),1)); x = seq(xmin,xmax,0.001)
      ymin = 0; ymax = ceiling(max(sapply(x,sdd,s=seq_len(states[1]))))
      hist(cs_logReturns,prob=TRUE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",xaxt="n",yaxt="n",xlab="",ylab="",main="")
      axis(1,seq(xmin,xmax,by=0.1)); axis(2,seq(ymin,ymax,by=40),las=1)
      title(main="State-dependent distributions",xlab="Log-return",ylab="Density")
      do.call(legend,c(list(legend=paste("Coarse-scale state",seq_len(states[1])),col=colors[,1],lwd=3),legend_layout))
      for(s in seq_len(states[1])){
        lines(x[sdd(s,x)>0.001],sdd(s,x)[sdd(s,x)>0.001],col=colors[s,1],lwd=lwd)
      }
      sdd = function(cs,fs,x) {1/sigmas_star[[cs]][fs]*dt((x-mus_star[[cs]][fs])/sigmas_star[[cs]][fs],dfs_star[[cs]][fs])}
      lwd = 3
      xmin = floor_dec(min(fs_logReturns),1); xmax = ceiling_dec(max(fs_logReturns),1); x = seq(xmin,xmax,0.001)
      ymin = 0; ymax = 0
      for(cs in seq_len(states[1])){
        for(fs in seq_len(states[2])){
          temp = max(sdd(cs,fs,x))
          if(temp>ymax) ymax = ceiling(temp)
        }
      }
      for(cs in seq_len(states[1])){
        hist(fs_logReturns,prob=TRUE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",xaxt="n",yaxt="n",xlab="",ylab="",main="")
        axis(1,seq(xmin,xmax,by=0.1)); axis(2,seq(ymin,ymax,by=20),las=1)
        title(main=paste("State-dependent distributions conditional on coarse-scale state",cs),xlab="Log-return",ylab="Density")
        do.call(legend,c(list(legend=paste("Fine-scale state",seq_len(states[2])),col=colors[cs,-1],lwd=3),legend_layout))
        for(fs in seq_len(states[2])){
          lines(x[sdd(cs,fs,x)>0.001],sdd(cs,fs,x)[sdd(cs,fs,x)>0.001],col=colors[cs,fs+1],lwd=lwd)
        }
      }
      invisible(dev.off())
    }
  }
  
  ### decoded time series
  filename = paste0("models/",controls[["id"]],"/ts.pdf")
  if(controls[["overwrite"]]==FALSE & file.exists(filename)){
    warning(paste0("Cannot create a plot of the decoded time series because the path '",filename,"' already exists and you chose not to overwrite."),call.=FALSE)
  } else {
    pdf(filename, width=19, height=9)
    par(las=1,mar=c(6,5,0.5,5),bty="n")
    if(!controls[["sim"]]){
      xmin = as.Date(format(as.Date(head(data$dates,n=1)),"%Y-01-01")); 
      xmin=as.Date("2000-01-01")
      xmax = as.Date(paste0(as.numeric(format(tail(data$dates,n=1),"%Y"))+1,"-01-01"))
      ymax = max(data[["dataRaw"]]); ymin = -ymax
      plot(data$dates,data[["dataRaw"]],type="l",xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="grey",xlab="",ylab="",xaxt="n",yaxt="n",cex.lab=2, cex.main=2)
      par(las=3)
      if(controls[["model"]]=="HMM") data_lab = controls[["data_col"]][1]
      if(controls[["model"]]=="HHMM") data_lab = controls[["data_col"]][2]
      mtext(data_lab,side=4,line=3.5,at=mean(data[["dataRaw"]]),cex=1.25)
      par(las=1)
      mtext("Year",side=1,line=2.5,cex=1.25)
      markdates = seq(xmin,xmax,by="year"); markdates = markdates[1:length(markdates)%%2==1]
      axis(1, markdates, format(markdates, "%Y"))
      axis(4, round(seq(min(data[["dataRaw"]]),max(data[["dataRaw"]]),length.out=3),digits=-2))
      if(controls[["model"]]=="HMM"){
        for(s in seq_len(states[1])){
          points(data$dates[decoding==s],data[["dataRaw"]][decoding==s],col=colors[s,1],pch=20)
        }
      }
      if(controls[["model"]]=="HHMM"){
        for(cs in seq_len(states[1])){
          for(fs in seq_len(states[2])){
            points(data$dates[decoding_cs==cs&decoding_fs==fs],data[["dataRaw"]][decoding_cs==cs&decoding_fs==fs],col=colors[cs,fs+1],pch=20)
          }
        }
      }
      par(new=TRUE,las=1)
      x_values = data$dates
      ymax_factor = 3
    }
    if(controls[["sim"]]){
      xmin = 1
      if(controls[["model"]]=="HMM") xmax = length(data[["logReturns"]])
      if(controls[["model"]]=="HHMM") xmax = length(fs_logReturns)
      x_values = seq_len(xmax)
      ymax_factor = 1.5
    }
      if(controls[["model"]]=="HMM"){
        ymin = min(data[["logReturns"]]); ymax = max(data[["logReturns"]])
        plot(x_values,data[["logReturns"]],type="h",col="grey",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax*ymax_factor))
      }
      if(controls[["model"]]=="HHMM"){
        ymin = min(fs_logReturns); ymax = max(fs_logReturns)
        plot(x_values,fs_logReturns,type="h",col="grey",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax*ymax_factor))
      }
      par(las=3)
      mtext("Log-return",side=2,line=3.5,at=0,cex=1.25)
      par(las=1)
      axis(2, round(c(ymin,0,ymax),2))
      if(controls[["model"]]=="HMM"){
        for(s in seq_len(states[1])){
          points(x_values[decoding==s],data[["logReturns"]][decoding==s],col=colors[s,1],pch=20)
        }
      }
      if(controls[["model"]]=="HHMM"){
        for(cs in seq_len(states[1])){
          for(fs in seq_len(states[2])){
            points(x_values[decoding_cs==cs&decoding_fs==fs],fs_logReturns[decoding_cs==cs&decoding_fs==fs],col=colors[cs,fs+1],pch=20)
          }
        }
      }
      abline(h=0)
      if(!controls[["sim"]] & !is.null(labels)){
        for(l in seq_len(length(labels[["dates"]]))){
          if(labels[["dates"]][l]<=xmax){
            abline(v=as.Date(labels[["dates"]][l]))
            text(x=as.Date(labels[["dates"]][l]),y=ymin,labels=l,pos=2,cex=1.25)
          }
        }
        names_trunc = labels[["names"]][labels[["dates"]]<=xmax]
        mtext(paste0(seq_len(length(names_trunc)),": ",names_trunc,collapse = "   "),side=1,line=4,cex=1.25)
      }
      if(controls[["model"]]=="HMM"){
        do.call(legend,c(list(legend=paste("State",seq_len(states[1])),col=colors[,1],pch=19),legend_layout))
      }
      if(controls[["model"]]=="HHMM"){
        eg = expand.grid(seq_len(states[2]),seq_len(states[1]))
        do.call(legend,c(list(legend=paste0("Coarse-scale state ",eg[,2],", fine-scale state ",eg[,1]),col=as.vector(t(colors[,-1])),pch=19),legend_layout))
      }
      invisible(dev.off())
  }
  
  ### pseudo-residuals
  filename = paste0("models/",controls[["id"]],"/pseudos.pdf")
  if(controls[["overwrite"]]==FALSE & file.exists(filename)){
    warning(paste0("Cannot create a plot of the pseudo-residuals because the path '",filename,"' already exists and you chose not to overwrite."),call.=FALSE)
  } else {
    if(controls[["model"]]=="HMM"){
      pseudos = numeric(T)
      for(t in 1:T){
        pseudos[t] = qnorm(pt((data[["logReturns"]][t]-mus[decoding[t]])/sigmas[decoding[t]],dfs[decoding[t]]))
      }
      pdf(filename, width=9, height=7)
        plot(pseudos,ylim=c(floor(min(pseudos)),ceiling(max(pseudos))),main="Residual plot",ylab="Pseudo-residuals",las=1,pch=3)
        hist(pseudos,freq=FALSE,breaks=15,col="lightgrey",xlim=c(floor(min(pseudos)),ceiling(max(pseudos))),main="Histogram w/ N(0;1)-density",xlab="Pseudo-residuals",las=1); x=seq(floor(min(pseudos)),ceiling(max(pseudos)),0.01); curve(dnorm(x),add=TRUE,lwd=2)
        qqnorm(pseudos[is.finite(pseudos)],ylim=c(floor(min(pseudos)),ceiling(max(pseudos))),xlim=c(floor(min(pseudos)),ceiling(max(pseudos))),main="Normal Q-Q plot", ylab="Quantiles of pseudo-residuals", xlab="N(0;1)-quantiles",las=1,pch=20); abline(a=0,b=1)
        acf(pseudos,lag.max = 10,main="", ylab="Autocorrelation of pseudo-residuals", xlab="Lag",las=1); title("Autocorrelation plot")
      invisible(dev.off())
    }
    if(controls[["model"]]=="HHMM"){
      pseudos_cs = numeric(T)
      pseudos_fs = numeric(T*T_star)
      for(t in 1:T){
        pseudos_cs[t] = qnorm(pt((data[["logReturns"]][t,1]-mus[decoding[t,1]])/sigmas[decoding[t,1]],dfs[decoding[t,1]]))
      }
      for(t in 1:(T*T_star)){
        pseudos_fs[t] = qnorm(pt((as.vector(t(data[["logReturns"]][,-1]))[t]-mus_star[[decoding_cs[t]]][decoding_fs[t]])/sigmas_star[[decoding_cs[t]]][decoding_fs[t]],dfs_star[[decoding_cs[t]]][decoding_fs[t]]))
      }
      pseudos_cs = pseudos_cs[is.finite(pseudos_cs)]
      pseudos_fs = pseudos_fs[is.finite(pseudos_fs)]
      pdf(filename, width=9, height=7)
        plot(pseudos_cs,ylim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="Residual plot",ylab="Coarse-scale pseudo-residuals",las=1,pch=3)
        plot(pseudos_fs,ylim=c(floor(min(pseudos_fs)),ceiling(max(pseudos_fs))),main="Residual plot",ylab="Fine-scale pseudo-residuals",las=1,pch=3)
        hist(pseudos_cs,freq=FALSE,breaks=15,col="lightgrey",xlim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="Histogram w/ N(0;1)-density",xlab="Coarse-scale pseudo-residuals",las=1); x = seq(floor(min(pseudos_cs)),ceiling(max(pseudos_cs)),0.01); curve(dnorm(x),add=TRUE,lwd=2)
        hist(pseudos_fs,freq=FALSE,breaks=20,col="lightgrey",xlim=c(floor(min(pseudos_fs)),ceiling(max(pseudos_fs))),main="Histogram w/ N(0;1)-density",xlab="Fine-scale pseudo-residuals of fine scale",las=1); x = seq(floor(min(pseudos_fs)),ceiling(max(pseudos_fs)),0.01); curve(dnorm(x),add=TRUE,lwd=2)
        qqnorm(pseudos_cs[is.finite(pseudos_cs)],ylim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),xlim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="Normal Q-Q plot", ylab="Quantiles of coarse-scale pseudo-residuals", xlab="N(0;1)-quantiles",las=1,pch=20); abline(a=0,b=1)
        qqnorm(pseudos_fs[is.finite(pseudos_fs)],ylim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),xlim=c(floor(min(pseudos_cs)),ceiling(max(pseudos_cs))),main="Normal Q-Q plot", ylab="Quantiles of fine-scale pseudo-residuals", xlab="N(0;1)-quantiles",las=1,pch=20); abline(a=0,b=1)
        acf(pseudos_cs[is.finite(pseudos_cs)],lag.max = 10,main="", ylab="Autocorrelation of coarse-scale pseudo-residuals", xlab="Lag",las=1); title("Autocorrelation plot")
        acf(pseudos_fs[is.finite(pseudos_fs)],lag.max = 30,main="", ylab="Autocorrelation of fine-scale pseudo-residuals", xlab="Lag",las=1); title("Autocorrelation plot")
      invisible(dev.off())
    }
  }
    
  writeLines("Visualization successful.")
}

### create visualization of LLs
plot_ll = function(llks,controls){
  filename = paste0("models/",controls[["id"]],"/lls.pdf")
  if(controls[["overwrite"]]==FALSE & file.exists(filename)){
    warning(paste0("Cannot create a plot of the log-likelihoods because the path '",filename,"' already exists and you chose not to overwrite."),call.=FALSE)
  } else {
    pdf(filename, width=9, height=7)
      if(length(llks)<=5){
        plot(llks,xaxt="n",yaxt="n",xlab="Estimation run",ylab="",main="Log-likelihoods",pch=16,ylim=c(floor(min(llks,na.rm=TRUE)),ceiling(max(llks,na.rm=TRUE))))
        axis(1,las=1,at=seq_len(length(llks)),labels=seq_len(length(llks)))      
      } else {
        plot(llks,yaxt="n",xlab="Estimation run",ylab="",main="Log-likelihoods",pch=16,ylim=c(floor(min(llks,na.rm=TRUE)),ceiling(max(llks,na.rm=TRUE))))
        axis(2,las=1,at=unique(round(llks[!is.na(llks)])),labels=unique(round(llks[!is.na(llks)])))
      }
      points(x=which.max(llks),y=llks[which.max(llks)],pch=16,cex=1.25,col="red")
      axis(2,las=1,at=unique(round(llks[!is.na(llks)])),labels=unique(round(llks[!is.na(llks)])))
    invisible(dev.off())
  }
}
