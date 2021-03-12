#' @title Visualization of log-likelihood values
#' @description Plots log-likelihood values of the different estimation runs.
#' @param lls A vector of log-likelihood values.
#' @param controls A list of controls.
#' @return No return value. Creates file "log_likelihoods.pdf" in "\code{controls[["path"]]}/models/\code{controls[["id"]]}".

plot_ll = function(lls,controls){
  if(check_saving(name = "log_likelihoods", filetype = "pdf", controls = controls)){
    pdf(file = paste0(controls[["path"]],"/models/",controls[["id"]],"/log_likelihoods.pdf"), width=8, height=8)
      if(length(lls)<=5){
        plot(lls,xaxt="n",yaxt="n",xlab="Estimation run",ylab="",main="Log-likelihoods",pch=16,ylim=c(floor(min(lls,na.rm=TRUE)),ceiling(max(lls,na.rm=TRUE))))
        axis(1,las=1,at=seq_len(length(lls)),labels=seq_len(length(lls)))      
      } else {
        plot(lls,yaxt="n",xlab="Estimation run",ylab="",main="Log-likelihoods",pch=16,ylim=c(floor(min(lls,na.rm=TRUE)),ceiling(max(lls,na.rm=TRUE))))
        axis(2,las=1,at=unique(round(lls[!is.na(lls)])),labels=unique(round(lls[!is.na(lls)])))
      }
      points(x=which.max(lls),y=lls[which.max(lls)],pch=16,cex=1.25,col="red")
      axis(2,las=1,at=unique(round(lls[!is.na(lls)])),labels=unique(round(lls[!is.na(lls)])))
    invisible(dev.off())
  }
}