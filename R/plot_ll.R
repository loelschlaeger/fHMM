#' Plot log-likelihood values of estimation runs
#'
#' @param llks A vector of log-likelihood values
#' @param controls A list of controls
#' 
#' @return No return value, called for side effects

plot_ll = function(llks,controls){
  if(check_saving(name = "log_likelihoods", filetype = "pdf", controls = controls)){
    pdf(file = paste0("models/",controls[["id"]],"/log_likelihoods.pdf"), width=8, height=8)
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