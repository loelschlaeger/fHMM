#' Perform estimation of HHMMs on financial data
#'
#' @param controls A list of controls
#' @param events A list of historical events, default NULL
#' @param warn An integer setting the handling of warning messages, default 1
#' @param sim_par A vector of model parameters for simulation, default NULL

hhmmf = function(controls,events=NULL,warn=1,sim_par=NULL){
  ### set handling of warnings
  options(warn=warn); on.exit(options(warn=0))
  ### create output folder
  if(dir.exists(paste0("models/",controls[["id"]])) & controls[["id"]]!="test"){
    stop(paste0("Model '",controls[["id"]],"' already exists."),call.=FALSE)
  } else {
    if(!dir.exists(paste0("models/",controls[["id"]]))){
      dir.create(paste0("models/",controls[["id"]]))
    }
  }
  ### execute HHMM_Finance Code 
  tryCatch(
    {sink(file = paste0("models/",controls[["id"]],"/protocol.txt"),split = TRUE)
      controls = check_controls(controls); writeLines(paste0(rep("-",45),collapse=""))
      data     = get_data(controls,sim_par); writeLines(paste0(rep("-",45),collapse=""))
      fit      = max_likelihood(data,controls)
      decoding = apply_viterbi(data,fit,controls)
    sink()
    create_visuals(data,fit,decoding,controls,events)
    },
    error   = function(cond) message(cond),
    finally = { for(i in seq_len(sink.number())) sink()
    }
  )
}