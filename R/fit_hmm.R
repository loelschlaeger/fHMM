#' Fit (hierarchical) hidden Markov models to financial data
#'
#' @param controls A list of controls
#' @param events A list of historical events, default \code{NULL}
#' @param sim_par A vector of model parameters for simulation, default \code{NULL}
#' 
#' @details 
#' \itemize{
#' A model is specified by setting parameters of the named list \code{controls} and passing it to \code{fit_hmm}. 
#' The following parameters are mandatory:
#' \item{\code{states}: a numeric vector of length 2, determining the model type and the number of states: If \code{states = c(x,0)}, a HMM with \code{x} states is estimated. If \code{states = c(x,y)}, a HHMM with \code{x} coarse-scale and \code{y} fine-scale states is estimated.}
#' \item{\code{sdds}: a character vector of length 2, specifying the state-dependent distributions for both scales: \code{"t"}, \code{"t(x)"}, \code{"gamma"}.}
#' \item{\code{data_cs_type}: a character, determining the type of coarse-scale data (only for a HHMM and empirical data): \code{"mean"}, \code{"mean_abs"}, \code{"sum_abs"}.}
#' }
#' \itemize{
#' The following parameters are optional and set to default values if not specified:
#' \item{\code{accept_codes}: either a numeric vector or \code{"all"} (accepting all codes)}
#' \item{\code{at_true}: a boolean, determining whether the optimization is initialised at the true parameter values} 
#' \item{\code{data_col}: a character vector of length 2, containing the name of the desired column of \code{data_source} for both scales}
#' \item{\code{data_source}: a character vector of length 2, containing the file names of the empirical data (data is simulated if \code{data_source = c(NA,NA)})}
#' \item{\code{gradtol}: setting the tolerance at which the scaled gradient is considered close enough to zero}
#' \item{\code{id}: a character, identifying the model}
#' \item{\code{iterlim}: an integer, specifying the maximum number of optimization iterations to be performed before termination}
#' \item{\code{overwrite}: a boolean, determining whether overwriting of existing results on the same \code{id} is allowed}
#' \item{\code{print_level}: an integer, determining the level of printing during the optimization}
#' \item{\code{runs}: an integer, setting the number of optimization runs}
#' \item{\code{scale_par}: a positive numeric vector of length two, scaling the model parameters}
#' \item{\code{seed}: setting a seed for the simulation and the optimization}
#' \item{\code{steptol}: setting the minimum allowable relative step length during the optimization}
#' \item{\code{time_horizon}: a vector of length 2 (first entry is mandatory if data is simulated, second entry is mandatory if the model is a HHMM), determining the length of the time horizion(s) and can either a numeric value be one of \code{w}, \code{m}, \code{q}, \code{y}}
#' \item{\code{truncate_data}: a vector of length 2, containing lower and upper date limits to select a subset of the empirical data}
#' }

fit_hmm = function(controls,events=NULL,sim_par=NULL){
  
  ### create folder
  if(!dir.exists("models")){
    dir.create("models")
  }
  if(!dir.exists("data")){
    dir.create("data")
  }
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
    error = function(cond) message(paste0(cond),appendLF=FALSE),
    finally = { for(i in seq_len(sink.number())) sink() }
  )
}
