### creates graphics of model results
create_visuals = function(data,fit,decoding,controls,events=NULL){
  
  ### pre-checks
  if(is.null(controls[["controls_checked"]])) stop("'controls' invalid",call.=FALSE)
  if(controls[["sim"]] & !is.null(events)){
    events = NULL
    warning("'events' is ignored because 'data' is simulated.",call.=FALSE)
  } 
  if(!controls[["sim"]] & !is.null(events) & length(events[["dates"]])!=length(events[["names"]])){
    stop("'dates' and 'names' in 'events' must be of the same length.")
  }
  
  ### save events
  if(!controls[["sim"]] & !is.null(events)){
    check_saving(object   = events,
                 filetype = "rds",
                 controls = controls)
  }
  
  ### define colours
  var_col = function(col,n){
    colorRampPalette(c("white",col,"black"))(n+2)[2:(n+1)]
  }
  base_col = function(n){
    colorRampPalette(c("darkgreen","green","yellow","orange","red","darkred"))(n)
  }
  col_alpha = function(col,alpha=0.6){
    adjustcolor(col,alpha)
  }
  colors = list()
  if(controls[["model"]]=="HMM"){
    colors[["HMM"]] = col_alpha(base_col(controls[["states"]][1]))
  }
  if(controls[["model"]]=="HHMM"){
    colors[["HHMM_cs"]] = col_alpha(base_col(controls[["states"]][1]))
    for(s in seq_len(controls[["states"]][1])){
      colors[["HHMM_fs"]][[s]] = col_alpha(var_col(colors[["HHMM_cs"]][s],controls[["states"]][2]))
    }
  }
  
  ### create visualization of state dependent distributions
  plot_sdds(controls,data,fit,decoding,colors)
  
  ### create visualization of decoded time series
  plot_ts(controls,data,decoding,colors,events)
  
  ### compute, save, visualize and test pseudo-residuals
  plot_prs(controls,data,fit,decoding)
}

### create visualization of state dependent distributions
plot_sdds = function(controls,data,fit,decoding,colors){
  
  if(check_saving(name     = "sdds",
                  filetype = "pdf",
                  controls = controls)){
    
    filename = paste0("models/",controls[["id"]],"/sdds.pdf")
    
    create_sdds_plot = function(nostates,
                                mus,
                                sigmas,
                                dfs,
                                sdd,
                                x_range,
                                c_xlim = FALSE,
                                xlim = NULL,
                                colors = NULL,
                                llabel = NULL,
                                ltitle = NULL,
                                sdd_true_parm = NULL){
      lwd = 3
      x = seq(x_range[1]*ifelse(x_range[1]<0,1.5,0.5),x_range[2]*ifelse(x_range[2]>0,1.5,0.5),length.out=10000)
      if(sdd=="gamma") x = x[x>0.001]
      
      sdd_density_values = list()
      sdd_true_density_values = list()
      sdd_density = function(mus,sigmas,dfs,state,sdd,x){
        if(sdd=="t"){
          return((1/sigmas[s])*dt((x-mus[s])/sigmas[s],dfs[s]))
        }
        if(sdd=="gamma"){
          return(dgamma(x,shape=mus[s]^2/sigmas[s]^2,scale=sigmas[s]^2/mus[s]))
        }
      }
      for(s in seq_len(nostates)){
        sdd_density_values[[s]] = sdd_density(mus,sigmas,dfs,s,sdd,x)
        if(!is.null(sdd_true_parm)){
          sdd_true_density_values[[s]] = sdd_density(sdd_true_parm[["mus"]],sdd_true_parm[["sigmas"]],sdd_true_parm[["dfs"]],s,sdd,x)
        }
      }
      trunc_distr = 0.01 * min(sapply(c(sdd_density_values,sdd_true_density_values),max))
      
      ymin = 0
      ymax = max(c(unlist(sdd_density_values),unlist(sdd_true_density_values)))
      if(c_xlim){
        xmin = max(-1,min(rep(x,nostates)[unlist(sdd_density_values)>trunc_distr]))
        xmin = round(xmin,digits=5)
        xmax = min(100,max(rep(x,nostates)[unlist(sdd_density_values)>trunc_distr]))
        xmax = round(xmax,digits=5)
        return(c(xmin,xmax))
      }
      if(!is.null(xlim)){
        xmin = xlim[1]
        xmax = xlim[2]
      } else {
        xmin = max(-1,min(rep(x,nostates)[unlist(sdd_density_values)>trunc_distr]))
        xmin = round(xmin,digits=5)
        xmax = min(100,max(rep(x,nostates)[unlist(sdd_density_values)>trunc_distr]))
        xmax = round(xmax,digits=5)
      }
      
      hist(0,prob=TRUE,xlim=c(xmin,xmax),ylim=c(ymin,ymax),col="white",border="white",xaxt="n",yaxt="n",xlab="",ylab="",main="")
      if(xmin<0 & 0<xmax){
        axis(1,c(xmin,0,xmax),labels=sprintf("%.1g",c(xmin,0,xmax)))
      }
      if(0<=xmin || 0>=xmax){
        axis(1,c(xmin,xmax),labels=sprintf("%.1g",c(xmin,xmax)))
      }
      axis(2,c(ymin,sapply(c(sdd_density_values,sdd_true_density_values),max)),labels=sprintf("%.2g",c(ymin,sapply(c(sdd_density_values,sdd_true_density_values),max))),las=1)
      title(main = paste0("Density of state-dependent ",sdd,"-distributions"),
            xlab = "Log-return",
            ylab = "")
      for(s in seq_len(nostates)){
        lines(x[sdd_density_values[[s]]>trunc_distr],sdd_density_values[[s]][sdd_density_values[[s]]>trunc_distr],col=colors[s],lwd=lwd)
        if(!is.null(sdd_true_parm)){
          lines(x[sdd_true_density_values[[s]]>trunc_distr],sdd_true_density_values[[s]][sdd_true_density_values[[s]]>trunc_distr],col=colors[s],lwd=lwd,lty=2)
        }
      }
      
      legend(legend=paste(llabel,seq_len(nostates)),col=colors,lwd=lwd,title=ltitle,cex=1.25,x="topleft",bg=rgb(1,1,1,0.5))
      if(!is.null(sdd_true_parm)){
        legend(legend=c("estimated","true"),col="grey",lwd=lwd,lty=c(1,2),cex=1.25,x="topright",bg=rgb(1,1,1,0.5))
      }
    }
    
    if(controls[["model"]]=="HMM"){
      pdf(file = filename, width=8, height=8)
        if(controls[["sim"]]){
          sdd_true_parm = list("mus"    = data[["thetaList0"]][["mus"]],
                               "sigmas" = data[["thetaList0"]][["sigmas"]],
                               "dfs"    = data[["thetaList0"]][["dfs"]])
        } else {
          sdd_true_parm = NULL
        }
        create_sdds_plot(nostates      = controls[["states"]][1],
                         mus           = fit[["thetaList"]][["mus"]],
                         sigmas        = fit[["thetaList"]][["sigmas"]],
                         dfs           = fit[["thetaList"]][["dfs"]],
                         sdd           = controls[["sdds"]][1],
                         x_range       = c(min(data[["logReturns"]]),max(data[["logReturns"]])),
                         colors        = colors[["HMM"]],
                         llabel        = "State",
                         sdd_true_parm = sdd_true_parm)
      invisible(dev.off())
    }
    
    if(controls[["model"]]=="HHMM"){
      pdf(file = filename, width=8, height=8)
        if(controls[["sim"]]){
          sdd_true_parm = list("mus"    = data[["thetaList0"]][["mus"]],
                               "sigmas" = data[["thetaList0"]][["sigmas"]],
                               "dfs"    = data[["thetaList0"]][["dfs"]])
        } else {
          sdd_true_parm = NULL
        } 
        create_sdds_plot(nostates      = controls[["states"]][1],
                         mus           = fit[["thetaList"]][["mus"]],
                         sigmas        = fit[["thetaList"]][["sigmas"]],
                         dfs           = fit[["thetaList"]][["dfs"]],
                         sdd           = controls[["sdds"]][1],
                         x_range       = c(min(data[["logReturns"]][,1]),max(data[["logReturns"]][,1])),
                         colors        = colors[["HHMM_cs"]],
                         llabel        = "Coarse-scale state",
                         sdd_true_parm = sdd_true_parm)
        xlims = matrix(0,nrow=2,ncol=controls[["states"]][1])
        for(cs in seq_len(controls[["states"]][1])){
          xlims[,cs] = create_sdds_plot(nostates = controls[["states"]][2],
                                        mus      = fit[["thetaList"]][["mus_star"]][[cs]],
                                        sigmas   = fit[["thetaList"]][["sigmas_star"]][[cs]],
                                        dfs      = fit[["thetaList"]][["dfs_star"]][[cs]],
                                        sdd      = controls[["sdds"]][2],
                                        x_range  = c(min(data[["logReturns"]][,-1]),max(data[["logReturns"]][,-1])),
                                        c_xlim   = TRUE) 
        }
        for(cs in seq_len(controls[["states"]][1])){
          if(controls[["sim"]]){
            sdd_true_parm = list("mus"    = data[["thetaList0"]][["mus_star"]][[cs]],
                                 "sigmas" = data[["thetaList0"]][["sigmas_star"]][[cs]],
                                 "dfs"    = data[["thetaList0"]][["dfs_star"]][[cs]])
          } else {
            sdd_true_parm = NULL
          } 
          create_sdds_plot(nostates      = controls[["states"]][2],
                           mus           = fit[["thetaList"]][["mus_star"]][[cs]],
                           sigmas        = fit[["thetaList"]][["sigmas_star"]][[cs]],
                           dfs           = fit[["thetaList"]][["dfs_star"]][[cs]],
                           sdd           = controls[["sdds"]][2],
                           x_range       = c(min(data[["logReturns"]][,-1]),max(data[["logReturns"]][,-1])),
                           c_xlim        = FALSE,
                           xlim          = c(min(xlims[1,]),max(xlims[2,])),
                           colors        = colors[["HHMM_fs"]][[cs]],
                           llabel        = "Fine-scale state",
                           ltitle        = paste("Coarse-scale state",cs),
                           sdd_true_parm = sdd_true_parm)
        }
      invisible(dev.off())
    }
    message("SDDs visualized.")
  }
}

### create visualization of decoded time series
plot_ts = function(controls,data,decoding,colors,events){
  
  ### extract parameters
  if(controls[["model"]]=="HMM"){
    T = length(data[["logReturns"]])
  }
  if(controls[["model"]]=="HHMM"){
    T = dim(data[["logReturns"]])[1]
    decoding_cs = rep(decoding[,1],times = data[["T_star"]])
    decoding_fs = as.vector(t(decoding[,-1]))[!is.na(as.vector(t(decoding[,-1])))]
    cs_logReturns = data[["logReturns"]][,1]
    fs_logReturns = as.vector(t(data[["logReturns"]][,-1]))[!is.na(as.vector(t(data[["logReturns"]][,-1])))]
  }
  
  if(check_saving(name     = "ts",
                  filetype = "pdf",
                  controls = controls)){
    pdf(file = paste0("models/",controls[["id"]],"/ts.pdf"), width=20, height=10)
      par(las=1,mar=c(6,5,0.5,5),bty="n")
      if(!controls[["sim"]]){
        xmin = as.Date(format(as.Date(head(data[["dates"]],n=1)),"%Y-01-01")); 
        xmax = as.Date(paste0(as.numeric(format(tail(data[["dates"]],n=1),"%Y"))+1,"-01-01"))
        ymax = ceiling(max(data[["dataRaw"]]))
        ymin = -ymax
        plot(data[["dates"]],data[["dataRaw"]],
             type="l",
             xlim=c(xmin,xmax),ylim=c(1.2*ymin,1.2*ymax),
             col="lightgrey",xlab="",ylab="",
             xaxt="n",yaxt="n",
             cex.lab=2, cex.main=2)
        if(controls[["model"]]=="HMM"){
          data_lab = controls[["data_col"]][1]
        }
        if(controls[["model"]]=="HHMM"){
          data_lab = controls[["data_col"]][2]
        }
        mtext("Year",side=1,line=2.5,cex=1.25)
        markdates = seq(xmin,xmax,by="year")
        markdates = markdates[1:length(markdates)%%2==1]
        axis(1, markdates, format(markdates, "%Y"))
        y_ticks = signif(seq(floor(min(data[["dataRaw"]])),ymax,length.out=3),digits=3)
        axis(4, y_ticks)
        mtext(data_lab,side=4,line=3.5,at=mean(y_ticks),cex=1.25,las=3)
        if(controls[["model"]]=="HMM"){
          for(s in seq_len(controls[["states"]][1])){
            points(data[["dates"]][decoding==s],data[["dataRaw"]][decoding==s],col=colors[["HMM"]][s],pch=20)
          }
        }
        if(controls[["model"]]=="HHMM"){
          for(cs in seq_len(controls[["states"]][1])){
            for(fs in seq_len(controls[["states"]][2])){
              points(data[["dates"]][decoding_cs==cs&decoding_fs==fs],data[["dataRaw"]][decoding_cs==cs&decoding_fs==fs],col=colors[["HHMM_fs"]][[cs]][fs],pch=20)
            }
          }
        }
        par(new=TRUE,las=1)
        x_values = data[["dates"]]
        ymax_factor = 3
      }
      if(controls[["sim"]]){
        xmin = 1
        if(controls[["model"]]=="HMM"){
          xmax = length(data[["logReturns"]])
        }
        if(controls[["model"]]=="HHMM"){
          xmax = length(fs_logReturns)
        }
        x_values = seq_len(xmax)
        ymax_factor = 1
      }
      if(controls[["model"]]=="HMM"){
        ymin = min(data[["logReturns"]])
        ymax = max(data[["logReturns"]])
        plot(x_values,data[["logReturns"]],type="h",col="lightgrey",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax*ymax_factor))
      }
      if(controls[["model"]]=="HHMM"){
        ymin = min(fs_logReturns)
        ymax = max(fs_logReturns)
        plot(x_values,fs_logReturns,type="h",col="lightgrey",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax*ymax_factor))
      }
      if(!controls[["sim"]]){
        mtext("Log-return",side=2,line=3.5,at=0,cex=1.25,las=3)
      }
      if(controls[["sim"]]){
        mtext("Index",side=1,line=2.5,cex=1.25)
        if(controls[["model"]]=="HMM"){
          mtext("Simulated observation",side=2,line=3.5,cex=1.25,las=3,at=mean(c(ymin,ymax)))
        }
        if(controls[["model"]]=="HHMM"){
          mtext("Simulated fine-scale observation",side=2,line=3.5,cex=1.25,las=3,at=mean(c(ymin,ymax)))
        }
        axis(1, c(xmin,xmax))
      }
      if(ymin<0 & 0<ymax){
        axis(2,c(ymin,0,ymax),labels=sprintf("%.2g",c(ymin,0,ymax)))
      }
      if(0<=ymin || 0>=ymax){
        axis(2,c(ymin,ymax),labels=sprintf("%.2g",c(ymin,ymax)))
      }
      if(controls[["model"]]=="HMM"){
        for(s in seq_len(controls[["states"]][1])){
          points(x_values[decoding==s],data[["logReturns"]][decoding==s],col=colors[["HMM"]][s],pch=20)
        }
      }
      if(controls[["model"]]=="HHMM"){
        for(cs in seq_len(controls[["states"]][1])){
          for(fs in seq_len(controls[["states"]][2])){
            points(x_values[decoding_cs==cs&decoding_fs==fs],fs_logReturns[decoding_cs==cs&decoding_fs==fs],col=colors[["HHMM_fs"]][[cs]][fs],pch=20)
          }
        }
      }
      if(!controls[["sim"]] & !is.null(events)){
        for(l in seq_len(length(events[["dates"]]))){
          if(events[["dates"]][l]<=xmax){
            abline(v=as.Date(events[["dates"]][l]))
            text(x=as.Date(events[["dates"]][l]),y=ymin,labels=l,pos=2,cex=1.25)
          }
        }
        names_trunc = events[["names"]][events[["dates"]]<=xmax]
        mtext(paste0(seq_len(length(names_trunc)),": ",names_trunc,collapse = "   "),side=1,line=4,cex=1.25)
      }
      if(controls[["model"]]=="HMM"){
        legend(legend=paste("State",seq_len(controls[["states"]][1])),col=colors[["HMM"]],pch=20,cex=1.25,x="topleft",bg=rgb(1,1,1,0.5))
      }
      if(controls[["model"]]=="HHMM"){
        eg = expand.grid(seq_len(controls[["states"]][2]),seq_len(controls[["states"]][1]))
        legend(legend=c(paste("Coarse-scale state",seq_len(controls[["states"]][1])),paste0("Fine-scale state ",eg[,1]," in coarse-scale state ",eg[,2])),
               col=c(colors[["HHMM_cs"]],as.vector(unlist(colors[["HHMM_fs"]]))),pt.lwd=c(rep(3,controls[["states"]][1]),rep(1,dim(eg)[1])),pch=c(rep(1,controls[["states"]][1]),rep(20,dim(eg)[1])),pt.cex=c(rep(3,controls[["states"]][1]),rep(2,dim(eg)[1])),cex=1.25,bg=rgb(1,1,1,0.5),x="topleft")
      }
      if(controls[["model"]]=="HHMM"){
        par(new=TRUE)
        ymin = min(cs_logReturns)
        ymax = max(cs_logReturns)
        x_values_cs = x_values[round(seq(1,length(x_values),length.out=T))]
        plot(x_values_cs,cs_logReturns,type="c",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax*ymax_factor*1.5))
        for(cs in seq_len(controls[["states"]][1])){
          points(x_values_cs[decoding[,1]==cs],cs_logReturns[decoding[,1]==cs],col=colors[["HHMM_cs"]][[cs]],pch=1,cex=3,lwd=2)
        }
        if(ymin<0 & 0<ymax){
          axis(4,c(ymin,0,ymax),labels=sprintf("%.2g",c(ymin,0,ymax)))
        }
        if(0<=ymin || 0>=ymin){
          axis(4,c(ymin,ymax),labels=sprintf("%.2g",c(ymin,ymax)))
        }
        if(controls[["sim"]]){
          mtext("Simulated coarse-scale observation",side=4,line=3.5,at=mean(c(ymin,ymax)),cex=1.25,las=3)
        }
        if(!controls[["sim"]]){
          mtext("Coarse-scale observation",side=4,line=3.5,at=mean(c(ymin,ymax)),cex=1.25,las=3)
        }
      } 
    invisible(dev.off())
    message("Time series visualized.")
  }
}

### compute, save and visualize pseudo-residuals
plot_prs = function(controls,data,fit,decoding){
  
  ### extract parameters
  if(controls[["model"]]=="HMM"){
    T = length(data[["logReturns"]])
  }
  if(controls[["model"]]=="HHMM"){
    T = dim(data[["logReturns"]])[1]
    decoding_cs = rep(decoding[,1],times = data[["T_star"]])
    decoding_fs = as.vector(t(decoding[,-1]))[!is.na(as.vector(t(decoding[,-1])))]
    cs_logReturns = data[["logReturns"]][,1]
    fs_logReturns = as.vector(t(data[["logReturns"]][,-1]))[!is.na(as.vector(t(data[["logReturns"]][,-1])))]
  }
  
  compute_prs = function(no_prs,data,decoding,mus,sigmas,dfs,sdd){
    pseudos = numeric(no_prs)
    for(t in seq_len(no_prs)){
      if(sdd=="t"){
        Fxt = pt((data[t]-mus[decoding[t]])/sigmas[decoding[t]],dfs[decoding[t]])
      }
      if(sdd=="gamma"){
        Fxt = pgamma(data[t],shape=mus[decoding[t]]^2/sigmas[decoding[t]]^2,scale=sigmas[decoding[t]]^2/mus[decoding[t]])
      }
      pseudos[t] = qnorm(Fxt)
    }
    return(pseudos)
  }
  
  create_prs_plots = function(pseudos,label_add=""){
    pseudos = pseudos[!is.na(pseudos) & is.finite(pseudos)]
    jbtest = tseries::jarque.bera.test(pseudos)[["p.value"]]
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
    legend("topleft",paste("P-value of Jarque-Bera test:",sprintf("%.2g",jbtest)),bty="n",bg=rgb(1,1,1,0.5))
    abline(a=0,b=1)
    acf(pseudos,
        main="Autocorrelation plot",
        ylab=paste("Autocorrelation of",label_add,"pseudo-residuals"),
        xlab="Lag",
        las=1) 
  }
  
  if(check_saving(name     = "prs",
                  filetype = "pdf",
                  controls = controls)){
    filename = paste0("models/",controls[["id"]],"/prs.pdf")
    if(controls[["model"]]=="HMM"){
      pseudos = compute_prs(no_prs   = T,
                            data     = data[["logReturns"]],
                            decoding = decoding,
                            mus      = fit[["thetaList"]][["mus"]],
                            sigmas   = fit[["thetaList"]][["sigmas"]],
                            dfs      = fit[["thetaList"]][["dfs"]],
                            sdd      = controls[["sdds"]][1])
      check_saving(object   = pseudos,
                   filetype = "rds",
                   controls = controls)
      pdf(filename, width=8, height=8)
        create_prs_plots(pseudos)
      invisible(dev.off())
    }
    if(controls[["model"]]=="HHMM"){
      pseudos_cs = compute_prs(no_prs   = T,
                               data     = data[["logReturns"]][,1],
                               decoding = decoding[,1],
                               mus      = fit[["thetaList"]][["mus"]],
                               sigmas   = fit[["thetaList"]][["sigmas"]],
                               dfs      = fit[["thetaList"]][["dfs"]],
                               sdd      = controls[["sdds"]][1])
      pseudos_fs = numeric(sum(data[["T_star"]]))
      for(t in seq_len(sum(data[["T_star"]]))){
        pseudos_fs[t] = compute_prs(no_prs   = 1,
                                    data     = fs_logReturns[t],
                                    decoding = decoding_fs[t],
                                    mus      = fit[["thetaList"]][["mus_star"]][[decoding_cs[t]]],
                                    sigmas   = fit[["thetaList"]][["sigmas_star"]][[decoding_cs[t]]],
                                    dfs      = fit[["thetaList"]][["dfs_star"]][[decoding_cs[t]]],
                                    sdd      = controls[["sdds"]][2])
      }
      pseudos = list("pseudos_cs" = pseudos_cs, "pseudos_fs" = pseudos_fs)
      check_saving(object   = pseudos,
                   filetype = "rds",
                   controls = controls)
      pdf(filename, width=8, height=8)
        create_prs_plots(pseudos_cs,label_add="coarse-scale")
        create_prs_plots(pseudos_fs,label_add="fine-scale")
      invisible(dev.off())
    }
    message("Pseudo-residuals visualized.")
  }
}

### create visualization of LLs
plot_ll = function(llks,controls){
  if(check_saving(name     = "lls",
                  filetype = "pdf",
                  controls = controls)){
    llks[which(llks< -1e100)] = NA
    if(length(llks[!is.na(llks)])==0){
      stop("Failed to create 'lls.pdf'.",call.=FALSE)
    } else {
      pdf(file = paste0("models/",controls[["id"]],"/lls.pdf"), width=8, height=8)
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
}

