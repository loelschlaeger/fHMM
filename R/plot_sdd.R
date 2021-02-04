#' Visualize estimated state-dependent distributions
#'
#' @param controls A list of controls
#' @param data A list of processed data information
#' @param fit A fitted model
#' @param decoding A matrix of decoded states
#' @param colors A matrix of colors for different states

plot_sdd = function(controls,data,fit,decoding,colors){
  
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
                                      x_range  = c(min(data[["logReturns"]][,-1],na.rm=TRUE),max(data[["logReturns"]][,-1],na.rm=TRUE)),
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
                         x_range       = c(min(data[["logReturns"]][,-1],na.rm=TRUE),max(data[["logReturns"]][,-1],na.rm=TRUE)),
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