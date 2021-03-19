#' @title Fit (hierarchical) hidden Markov models to financial data
#' @description Performs data processing, fitting, state decoding and visualization.
#' @param controls A list of controls.
#' @param events A list of (historical, financial) events, default \code{NULL}.
#' @param sim_par A list of model parameters for simulation in \code{thetaList} format, default \code{NULL}.
#' @details 
#' A model is specified by setting parameters of the named list \code{controls} and passing it to \code{fit_hmm}. 
#' Some control parameters are mandatory, the others are set to default values if not specified.
#' See the documentation <https://github.com/loelschlaeger/fHMM#readme> for a comprehensive list of all control parameters and their default values.
#' @return No return value. Estimation results are saved in "\code{controls[["path"]]}/models/\code{controls[["id"]]}".
#' @examples 
#' ### fitting a 3-state HMM with state-dependent t-distributions to simulated data
#' controls = list(
#'   path    = tempdir(),
#'   id      = "HMM_3_t",
#'   states  = c(3,0),
#'   sdds    = c("t",NA),
#'   horizon = c(500,NA),
#'   fit     = list("runs" = 10, "seed" = 1)
#' )
#' fit_hmm(controls)
#' @export

fit_hmm = function(controls,events=NULL,sim_par=NULL){
  
  ### check controls[["path"]]
  if(is.null(controls[["path"]]) || !dir.exists(controls[["path"]])){
    stop(sprintf("%s (%s)",exception("S.3")[2],exception("S.3")[1]))
  }
  
  ### create folder
  if(!dir.exists(paste0(controls[["path"]],"/models"))){
    dir.create(paste0(controls[["path"]],"/models"))
  }
  if(!dir.exists(paste0(controls[["path"]],"/data"))){
    dir.create(paste0(controls[["path"]],"/data"))
  }
  if(dir.exists(paste0(controls[["path"]],"/models/",controls[["id"]])) & controls[["id"]]!="test"){
    stop(sprintf("%s (%s)",exception("S.1")[2],exception("S.1")[1]))
  } else {
    if(!dir.exists(paste0(controls[["path"]],"/models/",controls[["id"]]))){
      dir.create(paste0(controls[["path"]],"/models/",controls[["id"]]))
    }
  }
  
  ### execute code
  tryCatch(
    {
      sink(file = paste0(controls[["path"]],"/models/",controls[["id"]],"/protocol.txt"),split = TRUE)
        controls = check_controls(controls)
        data     = process_data(controls,sim_par)
        fit      = max_likelihood(data,controls)
        decoding = apply_viterbi(data,fit,controls)
      sink()
      create_visuals(data,fit,decoding,controls,events) 
    },
    error = function(cond) message(paste0(cond),appendLF=FALSE),
    finally = { for(i in seq_len(sink.number())) sink() }
  )
}