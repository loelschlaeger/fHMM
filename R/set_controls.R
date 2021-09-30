#' Set and check controls for the fHMM package.
#' @description 
#' This function sets and checks the specification of controls for the fHMM 
#' package. 
#' @details 
#' See the vignettes for more information on how to specify \code{controls}.
#' @param controls 
#' A list of controls. 
#' Either none, all, or selected parameters can be specified. 
#' Unspecified parameters are set to default values (the values in brackets). 
#' If \code{hierarchy = TRUE}, parameters with a \code{(*)} must be a vector of 
#' length 2, where the first entry corresponds to the coarse-scale and the 
#' second entry to the fine-scale layer.
#' \itemize{
#'   \item \code{hierarchy} (\code{FALSE}): 
#'   A boolean, set to \code{TRUE} for an hierarchical HMM.
#'   \item \code{states} \code{(*)} (\code{2}):
#'   The number of states of the underlying Markov chain.
#'   \item \code{sdds} \code{(*)} (\code{"t(df = Inf)"}):
#'   Specifying the state-dependent distribution, one of the \code{"t"} (the 
#'   t-distribution) or \code{"gamma"} (the gamma distribution). To fix one or
#'   more parameter values, write e.g. \code{"t(mu = 0, sigma = 1, df = Inf)"} 
#'   or \code{"gamma(mu = 0, sigma = 1)"}, respectively.
#'   \item \code{horizon} \code{(*)} (\code{100}):
#'   A numeric, specifying the length of the time horizon. Alternatively, the 
#'   second entry of \code{horizon} can be one of:
#'   \itemize{
#'     \item \code{"w"} for a week,
#'     \item \code{"m"} for a month,
#'     \item \code{"q"} for a quarter,
#'     \item \code{"y"} for a year.
#'   }
#'   \item \code{data} (\code{NA}): A list of controls specifying the data. If 
#'   \code{data = NA}, data gets simulated. Otherwise:
#'   \itemize{
#'     \item \code{file} \code{(*)}:
#'     A character, the path to a .csv-file with financial data, which must 
#'     have a column named \code{"Date"} (with dates) and \code{column} 
#'     (with financial data).
#'     \item \code{column} \code{(*)}:
#'     A character, the name of the column in \code{file} with financial data.
#'     \item \code{from} (\code{NA}):
#'     A character of the format \code{"YYYY-MM-DD"}, setting a lower data 
#'     limit. No lower limit if \code{from = NA}.
#'     \item \code{to} (\code{NA}):
#'     A character of the format \code{"YYYY-MM-DD"}, setting an upper data 
#'     limit. No upper limit if \code{from = NA}.
#'     \item \code{logreturns} \code{(*)} (\code{FALSE}):
#'     A boolean, if \code{TRUE} the data is transformed to log-returns.
#'     \item \code{merge} (\code{"mean(x)"}):
#'     Only relevant, if \code{hierarchy = TRUE}. A character, which is an 
#'     expression to merge fine-scale data \code{x} into one coarse-scale 
#'     observation. For example,
#'     \itemize{
#'       \item \code{merge = "mean(x)"} defines the mean of the 
#'       fine-scale data as the coarse-scale observation,
#'       \item \code{merge = "mean(abs(x))} for the mean of the
#'       absolute values,
#'       \item \code{merge = "sum(abs(x))} for the sum of of the
#'       absolute values,
#'       \item \code{merge = "(tail(x,1)-head(x,1))/head(x,1)} for
#'       the relative change of the first to the last fine-scale observation.
#'     }
#'   }
#'   \item \code{fit}: A list of controls specifying the model fitting:
#'   \itemize{
#'     \item \code{runs} (\code{100}):
#'     An integer, setting the number of optimization runs.
#'     \item \code{origin} (\code{FALSE}):
#'     A boolean, if \code{TRUE} the optimization is initialized at the true
#'     parameter values. Only for simulated data. If \code{origin = TRUE}, this
#'     sets \code{run = 1} and \code{accept = 1:5}.
#'     \item \code{accept} (\code{1:3}):
#'     An integer (vector), specifying which optimization runs are accepted
#'     based on the output code of \code{\link[base]{nlm}}.
#'     \item \code{gradtol} (\code{1e-6}): 
#'     Passed on to \code{\link[base]{nlm}}.
#'     \item \code{iterlim} (\code{200}): 
#'     Passed on to \code{\link[base]{nlm}}.
#'     \item \code{print.level} (\code{0}): 
#'     Passed on to \code{\link[base]{nlm}}.
#'     \item \code{steptol} (\code{1e-6}): 
#'     Passed on to \code{\link[base]{nlm}}.
#'   }
#' }
#' @return 
#' An object of class \code{RprobitB_controls}. 
#' @examples 
#' controls = list(
#'   states  = 2,
#'   sdds    = "t(mu = 0, sigma = 1, df = 1)",
#'   horizon = 400,
#'   fit     = list("runs" = 50)
#' )
#' set_controls(controls)
#' @export

set_controls = function(controls = NULL) {
  
  if(class(controls) != "fHMM_controls"){
    ### initialize controls
    if(is.null(controls)) 
      controls = list()
    
    ### define names of all controls
    all_controls = c("hierarchy","states","sdds","horizon","data","fit")
    data_controls = c("file","column","from","to","logreturns","merge")
    fit_controls = c("runs","origin","accept","gradtol","iterlim","print.level","steptol")
    
    ### check redundant controls
    if(!is.null(controls)){
      redundant_controls = setdiff(names(controls), all_controls)
      if(length(redundant_controls) > 0) {
        warning("Element(s) ",paste(redundant_controls, collapse = ", "),
                " in 'controls' ignored.")
        controls[redundant_controls] = NULL
      }
      if(!is.null(controls[["data"]])){
        redundant_controls = setdiff(names(controls[["data"]]), data_controls)
        if(length(redundant_controls) > 0) {
          warning("Element(s) ",paste(redundant_controls, collapse = ", "),
                  " in 'controls$data' ignored.")
          controls[["data"]][redundant_controls] = NULL
        }
      }
      if(!is.null(controls[["fit"]])){
        redundant_controls = setdiff(names(controls[["fit"]]), fit_controls)
        if(length(redundant_controls) > 0) {
          warning("Element(s) ",paste(redundant_controls, collapse = ", "),
                  " in 'controls$fit' ignored.")
          controls[["fit"]][redundant_controls] = NULL
        }
      }
    }
  }
  
  ### set missing controls to default control values
  if(!"hierarchy" %in% names(controls))                  
    controls[["hierarchy"]] = FALSE
  if(!"states" %in% names(controls))                 
    controls[["states"]] = 2
  if(!"sdds" %in% names(controls))                   
    controls[["sdds"]] = "t"
  if(!"horizon" %in% names(controls))                
    controls[["horizon"]] = 100
  if(!"data" %in% names(controls)){
    controls[["data"]] = NA
  } else {
    if(!"file" %in% names(controls[["data"]])){
      if(controls[["hierarchy"]]){                
        controls[["data"]][["file"]] = NA
      } else {
        controls[["data"]][["file"]] = c(NA,NA)
      }
    }
    if(!"column" %in% names(controls[["data"]])){
      if(controls[["hierarchy"]]){                
        controls[["data"]][["column"]] = NA
      } else {
        controls[["data"]][["column"]] = c(NA,NA)
      }
    }
    if(!"from" %in% names(controls[["data"]]))     
      controls[["data"]][["from"]] = NA
    if(!"to" %in% names(controls[["data"]]))     
      controls[["data"]][["to"]] = NA
    if(!"logreturns" %in% names(controls[["data"]])){
      if(controls[["hierarchy"]]){                
        controls[["data"]][["logreturns"]] = FALSE
      } else {
        controls[["data"]][["logreturns"]] = c(FALSE,FALSE)
      }
    }
    if(!"merge" %in% names(controls[["data"]])) 
      controls[["data"]][["merge"]] = "mean(x)"
  }
  if(!"fit" %in% names(controls))                    
    controls[["fit"]] = list()
  if(!"runs" %in% names(controls[["fit"]]))          
    controls[["fit"]][["runs"]] = 100
  if(!"origin" %in% names(controls[["fit"]]))       
    controls[["fit"]][["origin"]] = FALSE
  if(!"accept" %in% names(controls[["fit"]]))        
    controls[["fit"]][["accept"]] = 1:3
  if(!"gradtol" %in% names(controls[["fit"]]))       
    controls[["fit"]][["gradtol"]] = 1e-6
  if(!"iterlim" %in% names(controls[["fit"]]))       
    controls[["fit"]][["iterlim"]] = 200
  if(!"print.level" %in% names(controls[["fit"]]))   
    controls[["fit"]][["print.level"]] = 0
  if(!"steptol" %in% names(controls[["fit"]]))      
    controls[["fit"]][["steptol"]] = 1e-6

  ### define control that specifies if data gets simulated
  controls[["simulated"]] = FALSE
  if(!is.list(controls[["data"]]) || length(controls[["data"]] == 0))
    controls[["simulated"]] = TRUE
  
  ### check state-dependent distributions and extract fixed parameter values
  out_split_sdd = split_sdd(sdd = controls[["sdds"]][1])
  

  ### check single controls
  if(!is.character(controls[["path"]])) 
    stop("The control 'path' must be a character.")
  if(!controls[["model"]] %in% c("hmm","hhmm")) 
    stop("The control 'model' must be one of 'hmm' or 'hhmm'.")
  if(controls[["model"]] == "hmm") 
    if(!(is.integer(controls[["states"]]) & length(controls[["states"]]) == 1 & all(controls[["states"]] >= 2))) 
      stop("The control 'states' must be an integer greater or equal 2.")
  if(controls[["model"]] == "hhmm")
    if(!(is.integer(controls[["states"]]) & length(controls[["states"]]) == 2 & all(controls[["states"]] >= 2))) 
      stop("The control 'states' must be a numeric vector of length 2 containing integers greater or equal 2.")
  if(controls[["model"]] == "hmm")
    if(!(length(controls[["sdds"]]) == 1 & controls[["sdds"]] %in% c("t","gamma")))
      stop("The control 'sdds' must be one of 't' or 'gamma'.")
  if(controls[["model"]] == "hhmm")
    if(!(length(controls[["sdds"]]) == 2 & all(controls[["sdds"]] %in% c("t","gamma"))))
      stop("The control 'sdds' must be a character vector of length 2 containing 't' or 'gamma'.")
  if(controls[["model"]]=="hmm")
    if(controls[["sim"]] & !is.integer(controls[["horizon"]]) & length(controls[["horizon"]]!=1))
      stop("The control 'horizon' must be an integer.")
  if(controls[["model"]]=="hhmm"){
    if(length(controls[["horizon"]])!=2)
      stop("The control 'horizon' must be a vector of length 2.")
    if(!(is.integer(controls[["horizon"]][2]) || controls[["horizon"]][2] %in% c("w","m","q","y")))
      stop("The second entry of the control 'horizon' must be an integer or one of 'w', 'm', 'q', 'y'.")
    if(controls[["sim"]] & !(is.integer(controls[["horizon"]][1])))
      stop("The first entry of the control 'horizon' must be an integer.")
  }
  
  ### check 'data' controls
  if(!controls[["sim"]]){
    if(controls[["model"]]=="hmm"){
      if(!(is.character(controls[["data"]][["file"]])) && length(controls[["data"]][["file"]] == 1))
        stop("The control 'file' must be a character.")
      if(is.na(controls[["data"]][["file"]]))
        stop("The control 'file' in 'data' has to be specified.")
      if(is.na(controls[["data"]][["column"]]))
        stop("The control 'column' in 'data' has to be specified.")
    }
    if(controls[["model"]]=="hhmm"){
      if(!(is.character(controls[["data"]][["file"]])) && length(controls[["data"]][["file"]] == 1))
        stop("The control 'file' must be a character vector of length two.")
      if(any(is.na(controls[["data"]][["file"]])))
        stop("The control 'file' in 'data' has to be specified.")
      if(!controls[["sim"]] & any(is.na(controls[["data"]][["column"]])))
        stop("The control 'column' in 'data' has to be specified.")
      if(!is.function(controls[["data"]][["cs_transform"]]))
        stop("The control 'cs_transform' in 'data' has to of class 'function'.")
    }
  }

  ### check 'fit' controls
  if(!is.integer(controls[["fit"]][["runs"]]))
    stop("The control 'runs' in 'fit' must be an integer.")
  if(!is.logical(controls[["fit"]][["at_true"]]))
    stop("The control 'at_true' in 'fit' must be an integer.")
  if(any(controls[["fit"]][["accept"]]=="all"))
    controls[["fit"]][["accept"]] = 1:5
  
  ### check if data paths are correct
  if(!controls[["sim"]]){
    for(i in c(1,2)){
      if(!is.na(controls[["data"]][["file"]][i])){
        if(!grepl(".csv$",controls[["data"]][["file"]][i])){
          controls[["data"]][["file"]][i] = paste0(controls[["data"]][["file"]][i],".csv")
        }
        if(!file.exists(paste0(controls[["path"]],"/data/",controls[["data"]][["file"]][i]))){
          stop(paste0("File '",controls[["path"]],"/data/",controls[["data"]][["file"]][i],"' not found."))
        }
        if(!controls[["data"]][["column"]][i] %in% colnames(read.csv(file=paste0("data/",controls[["data"]][["file"]][i])))){
          stop(paste0("Column '",controls[["data"]][["column"]][i],"' not found in the file '",controls[["path"]],"data/",controls[["data"]][["file"]][i],"'."))
        }
      }
    }
  }
  
  ### return controls
  class(controls) = "fHMM_controls"
  return(controls)
}