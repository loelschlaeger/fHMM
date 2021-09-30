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
#'   \item \code{sdds} \code{(*)} (\code{"t"}):
#'   One of the following, specifying the state-dependent distribution:
#'   \itemize{
#'     \item \code{"t"}, the t-distribution,
#'     \item \code{"t(x)"}, the t-distribution with \code{x} degrees of freedom
#'     (where \code{x = Inf} yields the normal distribution),
#'     \item \code{"gamma"}, the gamma distribution.
#'   }
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
#'     \item \code{from}:
#'     A character of the format \code{"YYYY-MM-DD"}, setting a lower data limit.
#'     \item \code{to}:
#'     A character of the format \code{"YYYY-MM-DD"}, setting an upper data limit.
#'     \item \code{logreturns} \code{(*)}:
#'     A boolean, if \code{TRUE} the data is transformed to log-returns.
#'     \item \code{merge}:
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
#'     sets \code{run = 1} and \code{accept = "all"}.
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
#'   sdds    = "t",
#'   horizon = 400,
#'   fit     = list("runs" = 50)
#' )
#' set_controls(controls)
#' @export

set_controls = function(controls = NULL) {
  
  ### define names of all controls
  all_controls = c("model","states","sdds","horizon","data","fit")
  data_controls = c("file","column","truncate","transformation","merge")
  fit_controls = c("runs","origin","accept","gradtol","iterlim","print.level",
                   "steptol","stepmax")
  
  ### initialize controls
  if(is.null(controls)) 
    controls = list()
  
  ### check redundant controls
  redundant_controls = setdiff(names(controls), all_controls)
  if(length(redundant_controls) > 0)
    warning("Element(s) ",paste(redundant_controls, collapse = ", "),
            " in 'controls' ignored.")
  
  ### set default control values
  if(!"model" %in% names(controls))                  
    controls[["model"]] = "hmm"
  if(!"states" %in% names(controls))                 
    controls[["states"]] = 2
  if(!"sdds" %in% names(controls))                   
    controls[["sdds"]] = "t"
  if(!"horizon" %in% names(controls))                
    controls[["horizon"]] = 1000
  
  if(!"data" %in% names(controls)){
    controls[["data"]] = NA
  } else {
    if(!"file" %in% names(controls[["data"]])){
      if(controls[["model"]] == "hmm")                 
        controls[["data"]][["file"]] = NA
      if(controls[["model"]] == "hhmm")                
        controls[["data"]][["file"]] = c(NA,NA)
    }
    if(!"column" %in% names(controls[["data"]])){
      if(controls[["model"]] == "hmm")                 
        controls[["data"]][["column"]] = NA
      if(controls[["model"]] == "hhmm")                
        controls[["data"]][["column"]] = c(NA,NA)
    }
    if(!"truncate" %in% names(controls[["data"]]))     
      controls[["data"]][["truncate"]] = c(NA,NA)
    if(!"cs_transform" %in% names(controls[["data"]])) 
      controls[["data"]][["cs_transform"]] = NA
    if(!"log_returns" %in% names(controls[["data"]])){
      if(controls[["model"]] == "hmm")                 
        controls[["data"]][["log_returns"]] = TRUE
      if(controls[["model"]] == "hhmm")                
        controls[["data"]][["log_returns"]] = c(TRUE,TRUE)
    }
  }
  
  if(!"fit" %in% names(controls))                    
    controls[["fit"]] = list()
  if(!"runs" %in% names(controls[["fit"]]))          
    controls[["fit"]][["runs"]] = 100
  if(!"at_true" %in% names(controls[["fit"]]))       
    controls[["fit"]][["at_true"]] = FALSE
  if(!"accept" %in% names(controls[["fit"]]))        
    controls[["fit"]][["accept"]] = c(1,2)
  if(!"print.level" %in% names(controls[["fit"]]))   
    controls[["fit"]][["print.level"]] = 0
  if(!"gradtol" %in% names(controls[["fit"]]))       
    controls[["fit"]][["gradtol"]] = 1e-6
  if(!"stepmax" %in% names(controls[["fit"]]))       
    controls[["fit"]][["stepmax"]] = 1
  if(!"steptol" %in% names(controls[["fit"]]))      
    controls[["fit"]][["steptol"]] = 1e-6
  if(!"iterlim" %in% names(controls[["fit"]]))       
    controls[["fit"]][["iterlim"]] = 200

  ### define control that defines if data gets simulated
  if(is.list(controls[["data"]])) {
    if(length(controls[["data"]]) == 0){
      controls[["sim"]] = TRUE
    } else {
      controls[["sim"]] = FALSE
    }
  } else if (is.null(controls[["data"]])) {
    controls[["sim"]] = TRUE
  } else if (is.na(controls[["data"]])) {
    controls[["sim"]] = TRUE
  } else {
    controls[["sim"]] = FALSE
  }
  
  ### check state-dependent distributions
  all_sdds = c("gamma","t")
  extract_dfs = function(x) grepl("^[t][\\(]([1-9][0-9]*|Inf)[\\)]$",x)
  controls[["fixed_dfs"]] = c(NA,NA)
  controls[["fixed_dfs"]][extract_dfs(controls[["sdds"]])] = as.numeric(sub("\\).*", "", sub(".*\\(", "", controls[["sdds"]][extract_dfs(controls[["sdds"]])])))
  controls[["sdds"]][extract_dfs(controls[["sdds"]])] = "t"
  
  ### function that checks if value is an integer
  is.integer = function(x) is.numeric(x) && x>=0 && x%%1==0
  
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