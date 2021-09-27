#' @title 
#' Fit (hierarchical) hidden Markov models to financial data
#' @description 
#' Performs data processing, fitting, state decoding and visualization.
#' @param controls A list of controls (optional).
#' @param events A list of (historical, financial) events (optional).
#' @param sim_par A list of model parameters for simulation in \code{thetaList} format, default \code{NULL} (optional).
#' @details 
#' Specify a model by setting parameters of the named list \code{controls} and passing it to \code{fit_hmm}. 
#' See the vignettes on how to specify \code{controls}.
#' @return 
#' No return value. Estimation results are saved in "\code{controls[["path"]]}/models/\code{controls[["id"]]}".
#' @examples 
#' ### fitting a 2-state HMM with state-dependent t-distributions to simulated data
#' controls = list(
#'   path    = tempdir(),
#'   id      = "test",
#'   model   = "hmm",
#'   states  = 2,
#'   sdds    = "t",
#'   horizon = 200,
#'   fit     = list("runs" = 10, "seed" = 1)
#' )
#' fit_hmm(controls)
#' @export

fit_hmm = function(controls, events, sim_par){
  
  ### check inputs
  if(missing(controls)) controls = list()
  if(missing(events)) events = NULL
  if(missing(sim_par)) sim_par = NULL
   
  ### check saving path
  if(is.null(controls[["path"]]))
    controls[["path"]] = tempdir()
  if(is.null(controls[["id"]]))
    controls[["id"]] = "test"
  if(!dir.exists(controls[["path"]]))
    stop("S.3")
  
  ### create folder
  if(!dir.exists(paste0(controls[["path"]],"/models")))
    dir.create(paste0(controls[["path"]],"/models"))
  if(!dir.exists(paste0(controls[["path"]],"/data")))
    dir.create(paste0(controls[["path"]],"/data"))
  if(dir.exists(paste0(controls[["path"]],"/models/",controls[["id"]])) & controls[["id"]]!="test"){
    stop("S.1")
  } else {
    if(!dir.exists(paste0(controls[["path"]],"/models/",controls[["id"]])))
      dir.create(paste0(controls[["path"]],"/models/",controls[["id"]]))
  }
  
  ### execute model fitting
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
    finally = {
        for(i in seq_len(sink.number())) sink()
      }
  )
}
