#' @title Optimization
#' @description Maximizes the model's log-likelihood function.
#' @param data A list of processed data information.
#' @param controls A list of controls.
#' @return A list of fitted model information.
#' @details Uses \code{nlm} for numerical optimization.

max_likelihood = function(data,controls){

  if(!is.null(controls[["fit"]][["seed"]])){
    set.seed(controls[["fit"]][["seed"]])
  }
  runs = controls[["fit"]][["runs"]]
  lls = rep(NA,runs) 
  mods = list() 
  
  ### define optimizer
  if(controls[["model"]]=="hmm") target = nLL_hmm
  if(controls[["model"]]=="hhmm") target = nLL_hhmm
  optimized = function(start_value){
    nlm_out = nlm(f = target,
                  p = start_value,
                  observations = data[["data"]],
                  controls = controls,
                  iterlim = controls[["fit"]][["iterlim"]],
                  steptol = controls[["fit"]][["steptol"]],
                  gradtol = controls[["fit"]][["gradtol"]],
                  print.level = controls[["fit"]][["print.level"]],
                  typsize = start_value,
                  hessian = FALSE)
    return(nlm_out)
  }
  
  ### generate start values
  generate_start_values = function(controls,runs){
    start_values = list()
    if(controls[["fit"]][["at_true"]]){
      start_values[[1]] = data[["thetaUncon0"]]
    }
    if(!controls[["fit"]][["at_true"]]){
      for(run in seq_len(runs)){
        start_values[[run]] = init_est(controls)
      }
    }
    return(start_values)
  }
  
  ### check if log-likelihoods at start values can be computed 
  failed_start_values = function(values){
    failed = union(which(is.nan(values)),which(is.na(values)))
    failed = c(failed,which(abs(values[-failed])>1e100))
    return(failed)
  }
  
  ### adjust 'scale_par' based on method of moments estimates
  adjust_scale_par = function(controls,data){
    scale_par = c(NA,NA)
    if(controls[["model"]]=="hmm"){
      scale_par[1] = mean(c(mean(data,na.rm="TRUE"),sd(data,na.rm="TRUE")))
    }
    if(controls[["model"]]=="hhmm"){
      scale_par[1] = mean(c(mean(data[,1],na.rm="TRUE"),sd(data[,1],na.rm="TRUE")))
      scale_par[2] = mean(c(mean(data[,-1],na.rm="TRUE"),sd(data[,-1],na.rm="TRUE")))
    }
    return(scale_par)
  }
  
  ### select start values
  message("Selecting start values...",appendLF = FALSE)
  controls[["fit"]][["scale_par"]] = adjust_scale_par(controls,data[["data"]])
  start_values = generate_start_values(controls,runs)
  ll_at_start_values = rep(NA,runs)
  for(run in seq_len(runs)){
    ll_at_start_values[run] = suppressWarnings(target(start_values[[run]],data[["data"]],controls))
  }
  message("\r",sprintf("Start values selected. %10s"," "))
  
  ### check fails
  fails = failed_start_values(ll_at_start_values)
  if(length(fails)==runs)
    stop("F.2")

  if(length(fails)/runs>0.5)
    warning("F.3")

  if(length(fails)>0){
    runs_seq = seq_len(runs)[-fails]
  } else {
    runs_seq = seq_len(runs)
  }
  
  ### start maximization
  start_time = Sys.time()
  progress(run = 0, total_runs = length(runs_seq), start_time = start_time)
  for (run in runs_seq){
    suppressWarnings({ tryCatch({ mods[[run]] = optimized(start_values[[run]])
    if(mods[[run]][["code"]] %in% controls[["fit"]][["accept"]] || controls[["fit"]][["at_true"]]){
      lls[run] = -mods[[run]]$minimum
    }
    },error = function(e){})
    })
    progress(run = run, total_runs = length(runs_seq), start_time = start_time)
  }
  end = Sys.time()
  message("Estimation finished.")
  if(all(is.na(lls))){
    stop("F.4")
  } else {
    ### estimation Info
    writeLines(sprintf("- %s %s minute(s)","estimation time:",ceiling(difftime(end,start_time,units='mins'))))
    if(!controls[["fit"]][["at_true"]]){
      writeLines(sprintf("- %s %s out of %s runs","accepted runs:",sum(!is.na(lls)),length(lls)))
    }
    
    ### compute Hessian
    message("Computing the Hessian...",appendLF = FALSE)
    hessian = suppressWarnings(nlm(f = target,
                                   p = mods[[which.max(lls)]][["estimate"]],
                                   observations = data[["data"]],
                                   controls = controls,
                                   iterlim = 1,
                                   hessian = TRUE,
                                   typsize = mods[[which.max(lls)]][["estimate"]])[["hessian"]])
    message("\r",sprintf("Hessian computed. %10s"," "))
    
    ### create and return fit object
    fit = check_estimation(mods,lls,data,hessian,controls)
    return(fit)
  }
}