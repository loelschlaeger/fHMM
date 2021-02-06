#' Fit (hierarchical) hidden Markov models to financial data
#'
#' @param controls A list of controls
#' @param events A list of historical events, default \code{NULL}
#' @param warn An integer setting the handling of warning messages, default \code{1}
#' @param sim_par A vector of model parameters for simulation, default \code{NULL}
#' 
#' @details 
#' A model is specified by setting parameters of the named list \code{controls} and passing it to \code{hhmmf}. 
#' The following parameters are mandatory:
#' \item{states}{a numeric vector of length 2, determining the model type and the number of states: If \code{states = c(x,0)}, a HMM with \code{x} states is estimated. If \code{states = c(x,y)}, a HHMM with \code{x} coarse-scale and \code{y} fine-scale states is estimated.
#' \item{sdds}{a character vector of length 2, specifying the state-dependent distributions for both scales: \code{"t"}, \code{"t(x)"}, \code{"gamma"}.
#' \item{data_cs_type}{a character, determining the type of coarse-scale data (only for a HHMM and empirical data): \code{"mean"}, \code{"mean_abs"}, \code{"sum_abs"}.
#' \item either \code{data_source} along with \code{data_col} (for empirical data) or \code{time_horizon} (for simulated data)
#' The following parameters are optional and set to default values if not specified:
#' \item{accept_codes}{either a numeric vector or \code{"all"} (accepting all codes)}
#' \item{at_true}{a boolean, determining whether the optimization is initialised at the true parameter values} 
#' \item{data_col}{a character vector of length 2, containing the name of the desired column of \code{data_source} for both scales}
#' \item{data_source}{a character vector of length 2, containing the file names of the empirical data (data is simulated if \code{data_source = c(NA,NA)})}
#' \item{gradtol}{setting the tolerance at which the scaled gradient is considered close enough to zero}
#' \item{id}{a character, identifying the model}
#' \item{iterlim}{an integer, specifying the maximum number of optimization iterations to be performed before termination}
#' \item{overwrite}{a boolean, determining whether overwriting of existing results on the same \code{id} is allowed}
#' \item{print_level}{an integer, determining the level of printing during the optimization}
#' \item{runs}{an integer, setting the number of optimization runs}
#' \item{scale_par}{a positive numeric vector of length two, scaling the model parameters}
#' \item{seed}{setting a seed for the simulation and the optimization}
#' \item{steptol}{setting the minimum allowable relative step length during the optimization}
#' \item{time_horizon}{a vector of length 2 (first entry is mandatory if data is simulated, second entry is mandatory if the model is a HHMM), determining the length of the time horizion(s) and can either a numeric value be one of \code{w}, \code{m}, \code{q}, \code{y}}
#' \item{truncate_data}{a vector of length 2, containing lower and upper date limits to select a subset of the empirical data}
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
    error = function(cond) message(cond),
    finally = { for(i in seq_len(sink.number())) sink() }
  )
}
