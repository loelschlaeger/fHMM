#' Visualize decoded time-series
#' @param controls A list of controls.
#' @param data A list of processed data information.
#' @param decoding A matrix of decoded states.
#' @param colors A matrix of colors for different states.
#' @param events A list of events.
#' @return No return value, creates graphic in \code{controls[["path"]]}/models/\code{controls[["id"]]}
plot_ts = function(controls,data,decoding,colors,events){
  ### extract parameters
  if(controls[["model"]]=="hmm"){
    T = length(data[["data"]])
  }
  if(controls[["model"]]=="hhmm"){
    T = dim(data[["data"]])[1]
    decoding_cs = rep(decoding[,1], times = data[["T_star"]])
    decoding_fs = as.vector(t(decoding[,-1]))[!is.na(as.vector(t(decoding[,-1])))]
    cs_data = data[["data"]][,1]
    fs_data = as.vector(t(data[["data"]][,-1]))[!is.na(as.vector(t(data[["data"]][,-1])))]
  }
  if(check_saving(name = "decoded_time_series", filetype = "pdf", controls = controls)){
    pdf(file = paste0(controls[["path"]],"/models/",controls[["id"]],"/decoded_time_series.pdf"), width=20, height=10)
      par(las=1,mar=c(6,5,0.5,5),bty="n")
      if(!controls[["sim"]]){
        xmin = as.Date(format(as.Date(head(data[["dates"]],n=1)),"%Y-01-01")); 
        xmax = as.Date(paste0(as.numeric(format(tail(data[["dates"]],n=1),"%Y"))+1,"-01-01"))
        if(controls[["model"]]=="hmm"){
          ydata = data[["data_raw"]]
          ymax = ceiling(max(ydata))
        }
        if(controls[["model"]]=="hhmm"){
          ydata = data[["data_fs_raw"]]
          ymax = ceiling(max(ydata))
        }
        ymin = -ymax
        plot(data[["dates"]],ydata,
             type="l",
             xlim=c(xmin,xmax),ylim=c(1.2*ymin,1.2*ymax),
             col="lightgrey",xlab="",ylab="",
             xaxt="n",yaxt="n",
             cex.lab=2, cex.main=2)
        if(controls[["model"]]=="hmm"){
          data_lab = controls[["data"]][["column"]][1]
        }
        if(controls[["model"]]=="hhmm"){
          data_lab = controls[["data"]][["column"]][2]
        }
        mtext("Year",side=1,line=2.5,cex=1.25)
        markdates = seq(xmin,xmax,by="year")
        markdates = markdates[1:length(markdates)%%2==1]
        axis(1, markdates, format(markdates, "%Y"))
        y_ticks = signif(seq(floor(min(ydata)),ymax,length.out=3),digits=3)
        axis(4, y_ticks)
        mtext(data_lab,side=4,line=3.5,at=mean(y_ticks),cex=1.25,las=3)
        if(controls[["model"]]=="hmm"){
          for(s in seq_len(controls[["states"]][1])){
            points(data[["dates"]][decoding==s],ydata[decoding==s],col=colors[["hmm"]][s],pch=20)
          }
        }
        if(controls[["model"]]=="hhmm"){
          for(cs in seq_len(controls[["states"]][1])){
            for(fs in seq_len(controls[["states"]][2])){
              points(data[["dates"]][decoding_cs==cs&decoding_fs==fs],ydata[decoding_cs==cs&decoding_fs==fs],col=colors[["hhmm_fs"]][[cs]][fs],pch=20)
            }
          }
        }
        par(new=TRUE,las=1)
        x_values = data[["dates"]]
        ymax_factor = 3
      }
      if(controls[["sim"]]){
        xmin = 1
        if(controls[["model"]]=="hmm"){
          xmax = length(data[["data"]])
        }
        if(controls[["model"]]=="hhmm"){
          xmax = length(fs_data)
        }
        x_values = seq_len(xmax)
        ymax_factor = 1
      }
      if(controls[["model"]]=="hmm"){
        ymin = min(data[["data"]])
        ymax = max(data[["data"]])
        plot(x_values,data[["data"]],type="h",col="lightgrey",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax*ymax_factor))
      }
      if(controls[["model"]]=="hhmm"){
        ymin = min(fs_data)
        ymax = max(fs_data)
        plot(x_values,fs_data,type="h",col="lightgrey",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax*ymax_factor))
      }
      if(!controls[["sim"]]){
        mtext("Fine-scale data",side=2,line=3.5,at=0,cex=1.25,las=3)
      }
      if(controls[["sim"]]){
        mtext("Index",side=1,line=2.5,cex=1.25)
        if(controls[["model"]]=="hmm"){
          mtext("Simulated data",side=2,line=3.5,cex=1.25,las=3,at=mean(c(ymin,ymax)))
        }
        if(controls[["model"]]=="hhmm"){
          mtext("Simulated fine-scale data",side=2,line=3.5,cex=1.25,las=3,at=mean(c(ymin,ymax)))
        }
        axis(1, c(xmin,xmax))
      }
      if(ymin<0 & 0<ymax){
        axis(2,c(ymin,0,ymax),labels=sprintf("%.2g",c(ymin,0,ymax)))
      }
      if(0<=ymin || 0>=ymax){
        axis(2,c(ymin,ymax),labels=sprintf("%.2g",c(ymin,ymax)))
      }
      if(controls[["model"]]=="hmm"){
        for(s in seq_len(controls[["states"]][1])){
          points(x_values[decoding==s],data[["data"]][decoding==s],col=colors[["hmm"]][s],pch=20)
        }
      }
      if(controls[["model"]]=="hhmm"){
        for(cs in seq_len(controls[["states"]][1])){
          for(fs in seq_len(controls[["states"]][2])){
            points(x_values[decoding_cs==cs&decoding_fs==fs],fs_data[decoding_cs==cs&decoding_fs==fs],col=colors[["hhmm_fs"]][[cs]][fs],pch=20)
          }
        }
      }
      if(!controls[["sim"]] & !is.null(events)){
        events[["names"]] = events[["names"]][events[["dates"]] > xmin & events[["dates"]] < xmax]
        events[["dates"]] = events[["dates"]][events[["dates"]] > xmin & events[["dates"]] < xmax]
        for(l in seq_len(length(events[["dates"]]))){
          if(events[["dates"]][l] > xmin & events[["dates"]][l] < xmax){
            abline(v=as.Date(events[["dates"]][l]))
            text(x=as.Date(events[["dates"]][l]),y=ymin,labels=l,pos=2,cex=1.25)
          }
        }
        mtext(paste0(seq_len(length(events[["names"]])),": ",events[["names"]],collapse = "   "),side=1,line=4,cex=1.25)
      }
      if(controls[["model"]]=="hmm"){
        legend(legend=paste("State",seq_len(controls[["states"]][1])),col=colors[["hmm"]],pch=20,cex=1.25,x="topleft",bg=rgb(1,1,1,0.5))
      }
      if(controls[["model"]]=="hhmm"){
        eg = expand.grid(seq_len(controls[["states"]][2]),seq_len(controls[["states"]][1]))
        legend(legend=c(paste("Coarse-scale state",seq_len(controls[["states"]][1])),paste0("Fine-scale state ",eg[,1]," in coarse-scale state ",eg[,2])),
               col=c(colors[["hhmm_cs"]],as.vector(unlist(colors[["hhmm_fs"]]))),pt.lwd=c(rep(3,controls[["states"]][1]),rep(1,dim(eg)[1])),pch=c(rep(1,controls[["states"]][1]),rep(20,dim(eg)[1])),pt.cex=c(rep(3,controls[["states"]][1]),rep(2,dim(eg)[1])),cex=1.25,bg=rgb(1,1,1,0.5),x="topleft")
      }
      if(controls[["model"]]=="hhmm"){
        par(new=TRUE)
        ymin = min(cs_data)
        ymax = max(cs_data)
        x_values_cs = x_values[round(seq(1,length(x_values),length.out=T))]
        plot(x_values_cs,cs_data,type="c",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax*ymax_factor*1.5))
        for(cs in seq_len(controls[["states"]][1])){
          points(x_values_cs[decoding[,1]==cs],cs_data[decoding[,1]==cs],col=colors[["hhmm_cs"]][[cs]],pch=1,cex=3,lwd=2)
        }
        if(ymin<0 & 0<ymax){
          axis(4,c(ymin,0,ymax),labels=sprintf("%.2g",c(ymin,0,ymax)))
        }
        if(0<=ymin || 0>=ymin){
          axis(4,c(ymin,ymax),labels=sprintf("%.2g",c(ymin,ymax)))
        }
        if(controls[["sim"]]){
          mtext("Simulated coarse-scale data",side=4,line=3.5,at=mean(c(ymin,ymax)),cex=1.25,las=3)
        }
        if(!controls[["sim"]]){
          mtext("Coarse-scale data",side=4,line=3.5,at=mean(c(ymin,ymax)),cex=1.25,las=3)
        }
      } 
    invisible(dev.off())
  }
}