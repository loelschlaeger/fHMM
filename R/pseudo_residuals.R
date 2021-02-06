#' Compute and visualize pseudo-residuals
#'
#' @param controls A list of controls
#' @param data A list of processed data information
#' @param fit A fitted model
#' @param decoding A matrix of decoded states

pseudo_residuals = function(controls,data,fit,decoding){
  
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
  
  if(check_saving(name = "pseudo_residuals", filetype = "pdf", controls = controls)){
    filename = paste0("models/",controls[["id"]],"/pseudo_residuals.pdf")
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
  }
}
