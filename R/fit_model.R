#' Model fitting for the fHMM package.
#' @description 
#' This function fits an (hierarchical) HMM for the fHMM package.
#' @param data 
#' An object of class \code{fHMM_data}.
#' @param seed
#' Set a seed for the sampling of initial values.
#' @return 
#' An object of class \code{fHMM_model}, which is a list of
#' \itemize{
#'   \item ...
#' }

fit_model = function(data, seed = NULL){

  ### check inputs
  if(class(data) != "fHMM_data")
    stop("'data' is not of class 'fHMM_data'.")
  
  ### set seed
  if(!is.null(seed))
    set.seed(seed)
  
  ### generate start values
  start_values = list()
  if(data[["controls"]][["fit"]][["origin"]]){
    start_values[[1]] = par2parUncon(data[["true_parameters"]], data[["controls"]])
  } else {
    ### compute parameter scales based on the method of moments
    scale_par = c(1,1)
    if(!data[["controls"]][["hierarchy"]]){
      scale_par[1] = mean(c(mean(data[["data"]],na.rm="TRUE"),sd(data[["data"]],na.rm="TRUE")))
    } else {
      scale_par[1] = mean(c(mean(data[["data"]][,1],na.rm="TRUE"),sd(data[["data"]][,1],na.rm="TRUE")))
      scale_par[2] = mean(c(mean(data[["data"]][,-1],na.rm="TRUE"),sd(data[["data"]][,-1],na.rm="TRUE")))
    }
    for(run in 1:data[["controls"]][["fit"]][["runs"]])
      start_values[[run]] = par2parUncon(
        set_parameters(data[["controls"]], scale_par = scale_par), 
        data[["controls"]])
  }
  
  ### define likelihood function
  target = ifelse(!data[["controls"]][["hierarchy"]], nLL_hmm, nLL_hhmm)
  
  ### check start values
  cat("Checking start values ... ")
  ll_at_start_values = rep(NA,data[["controls"]][["fit"]][["runs"]])
  for(run in 1:data[["controls"]][["fit"]][["runs"]]){
    ll = target(parUncon = start_values[[run]], 
                observations = data[["data"]],
                controls = data[["controls"]])
    if(!(is.na(ll) || is.nan(ll) || abs(ll)>1e100)){
      ll_at_start_values[run] = ll
    }
  }
  if(sum(is.na(ll_at_start_values)) == data[["controls"]][["fit"]][["runs"]])
    stop("F.2")
  if(sum(is.na(ll_at_start_values)) / data[["controls"]][["fit"]][["runs"]] > 0.5)
    warning("F.3", immediate. = TRUE)
  runs_seq = which(!is.na(ll_at_start_values))
  cat("Done.\n")
  
  ### start optimization
  start_time = Sys.time()
  mods = list()
  lls = rep(NA,data[["controls"]][["fit"]][["runs"]])
  progress(run = 0, total_runs = length(runs_seq), start_time = start_time,
           message = "Likelihood maximization:")
  for(run in 1:data[["controls"]][["fit"]][["runs"]]){
    if(!is.na(ll_at_start_values[run])){
      suppressWarnings({
        mod = try(
          nlm(f = target,
              p = start_values[[run]],
              observations = data[["data"]],
              controls = data[["controls"]],
              iterlim = data[["controls"]][["fit"]][["iterlim"]],
              steptol = data[["controls"]][["fit"]][["steptol"]],
              gradtol = data[["controls"]][["fit"]][["gradtol"]],
              print.level = data[["controls"]][["fit"]][["print.level"]],
              typsize = start_values[[run]],
              hessian = FALSE),
          silent = TRUE)
      })
      if(class(mod) != "try-error" && mod[["code"]] %in% data[["controls"]][["fit"]][["accept"]]){
        mods[[run]] = mod
        lls[run] = -mod[["minimum"]]
      }
    }
    progress(run = run, total_runs = data[["controls"]][["fit"]][["runs"]], 
             start_time = start_time, message = "Likelihood maximization:")
  }
  end_time = Sys.time()
  
  ### evaluate estimation
  if(all(is.na(lls))){
    stop("F.4")
  } 

  ### compute Hessian
  cat("Computing the Hessian ... ")
  hessian = suppressWarnings(nlm(f = target,
                                 p = mods[[which.max(lls)]][["estimate"]],
                                 observations = data[["data"]],
                                 controls = data[["controls"]],
                                 iterlim = 1,
                                 hessian = TRUE,
                                 typsize = mods[[which.max(lls)]][["estimate"]])[["hessian"]])
  cat("Done.\n")
  
  ### extract estimation results
  mod = mods[[which.max(lls)]]
  ll = -mod[["minimum"]]
  
  ### order estimates
  # parUncon = mod[["estimate"]]
  # thetaCon = thetaUncon2thetaCon(thetaUncon,controls)
  # thetaList = thetaCon2thetaList(thetaCon,controls)
  # thetaListOrdered = thetaList2thetaListOrdered(thetaList,controls)
  # permut = diag(length(thetaUncon))[match(thetaCon,thetaConOrdered),]
  # hessianOrdered = permut %*% hessian %*% t(permut)
  # hessianOrdered[is.na(hessianOrdered)] = 0
  
  ### create and return 'fHMM_model' object
  out = list("data" = data, 
             "estimated_parameter" = mod[["estimate"]],
             "nlm_output" = mod,
             "estimation_time" = ceiling(difftime(end_time,start_time,units='mins')),
             "ll" = ll,
             "lls" = lls, 
             "gradient" = mod$gradient,
             "hessian" = hessian)
  class(out) = "fHMM_model"
  return(out)
}