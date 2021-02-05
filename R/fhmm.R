#' Fit (hierarchical) hidden Markov models to financial data
#'
#' @param controls A list of controls
#' @param events A list of historical events, default \code{NULL}
#' @param warn An integer setting the handling of warning messages, default \code{1}
#' @param sim_par A vector of model parameters for simulation, default \code{NULL}
#' 
#' @examples 
#' ### Fitting a 3-state HMM with state-dependent t-distributions to simulated data
#' controls = list(
#'  id           = "HMM_3_t",
#'  sdds         = c("t",NA),
#'  states       = c(3,0),
#'  time_horizon = c(500,NA),
#'  seed         = 1
#' )
#' fhmm(controls)

fhmm = function(controls,events=NULL,warn=1,sim_par=NULL){
  
  ### set handling of warnings
  options(warn=warn); on.exit(options(warn=0))
  
  ### create output folder
  if(dir.exists(paste0("models/",controls[["id"]])) & controls[["id"]]!="test"){
    stop(sprintf("%s (%s)",exception("S.1")[2],exception("S.1")[1]))
  } else {
    if(!dir.exists(paste0("models/",controls[["id"]]))){
      dir.create(paste0("models/",controls[["id"]]))
    }
  }
  
  ### execute code
  tryCatch(
    {
    sink(file = paste0("models/",controls[["id"]],"/protocol.txt"),split = TRUE)
      controls = check_controls(controls)
      data     = process_data(controls,sim_par)
      fit      = max_likelihood(data,controls)
      decoding = apply_viterbi(data,fit,controls)
    sink()
    create_visuals(data,fit,decoding,controls,events) 
    },
    error   = function(cond) message(cond),
    finally = { for(i in seq_len(sink.number())) sink() }
  )
}
