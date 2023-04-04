#' Set and validate model specifications
#' 
#' @description
#' This function sets and validates specifications for model estimation with
#' the \{fHMM\} package.
#'
#' It is recommended to set all specifications before fitting the model, but 
#' they can also be specified in the corresponding functions.
#' 
#' @details
#' See the [vignette on controls](https://loelschlaeger.de/fHMM/articles) 
#' for more details.
#' 
#' @param controls
#' Either a \code{list} or an object of class \code{fHMM_controls}.
#'
#' The \code{list} can contain the following elements, which are described 
#' in more detail below:
#' \itemize{
#'   \item \code{hierarchy}, defines an hierarchical HMM,
#'   \item \code{states}, defines the number of states,
#'   \item \code{sdds}, defines the state-dependent distributions,
#'   \item \code{horizon}, defines the time horizon,
#'   \item \code{period}, defines a flexible, periodic fine-scale time horizon,
#'   \item \code{data}, a \code{list} of controls that define the data,
#'   \item \code{fit}, a \code{list} of controls that define the fitting process,
#'   \item \code{seed}, defines a seed for reproducibility,
#'   \item \code{verbose}, defines whether to print progress messages.
#' }
#' Either none, all, or selected elements can be specified.
#' 
#' Unspecified parameters are set to their default values, see below.
#' 
#' Specifications in \code{controls} override individual specifications.
#' 
#' @param hierarchy
#' A \code{logical}, set to \code{TRUE} for an hierarchical HMM.
#'
#' If \code{hierarchy = TRUE}, some of the other controls must be specified for
#' both layers.
#' 
#' By default, \code{hierarchy = FALSE}.
#' 
#' @param states
#' An \code{integer}, the number of states of the underlying Markov chain.
#' 
#' If \code{hierarchy = TRUE}, \code{states} must be a \code{vector} of length 2.
#' The first entry corresponds to the coarse-scale layer, while the second entry
#' corresponds to the fine-scale layer.
#' 
#' By default, \code{states = 2} if \code{hierarchy = FALSE} and 
#' \code{states = c(2, 2)} if \code{hierarchy = TRUE}.
#'
#' @param sdds
#' A \code{character}, specifying the state-dependent distribution.
#' One of 
#' \itemize{
#'   \item \code{"t"} (the t-distribution),
#'   \item \code{"gamma"} (the gamma distribution),
#'   \item \code{"lnorm"} (the log-normal distribution),
#'   \item \code{"poisson"} (the Poisson distribution).
#' }
#' 
#' The distribution parameters 
#' \itemize{
#'   \item mean \code{mu},
#'   \item standard deviation \code{sigma} (not for Poisson distribution),
#'   \item degrees of freedom \code{df} (only for t-distribution),
#' }
#' can be fixed via, e.g., \code{"t(df = Inf)"} or 
#' \code{"gamma(mu = 0, sigma = 1)"}.
#' To fix different values of a parameter for different states, separate by
#' "|", e.g. \code{"poisson(mu = 1|2|3)"}. 
#' 
#' If \code{hierarchy = TRUE}, \code{sdds} must be a \code{vector} of length 2.
#' The first entry corresponds to the coarse-scale layer, while the second entry
#' corresponds to the fine-scale layer.
#' 
#' By default, \code{sdds = "t(df = Inf)"} (normal distribution) if 
#' \code{hierarchy = FALSE} and \code{sdds = c("t(df = Inf)", "t(df = Inf)")} 
#' if \code{hierarchy = TRUE}.
#'
#' See also \code{\link{fHMM_sdds}} for more details.
#'
#' @param horizon
#' A \code{numeric}, specifying the length of the time horizon. 
#' 
#' If \code{hierarchy = TRUE}, \code{horizon} must be a \code{vector} of length 2.
#' The first entry corresponds to the coarse-scale layer, while the second entry
#' corresponds to the fine-scale layer.
#' 
#' By default, \code{horizon = 100} if \code{hierarchy = FALSE} and 
#' \code{horizon = c(100, 30)} if \code{hierarchy = TRUE}.
#' 
#' If \code{data} is specified (i.e., not \code{NA}), the first entry of 
#' \code{horizon} is ignored and the (coarse-scale) time horizon is defined by
#' \code{data}.
#'
#' @param period
#' Only relevant if \code{hierarchy = TRUE} and \code{horizon[2] = NA}.
#' 
#' In this case, a \code{character} which specifies a flexible, periodic 
#' fine-scale time horizon and can be one of
#' \itemize{
#'   \item \code{"w"} for a week,
#'   \item \code{"m"} for a month,
#'   \item \code{"q"} for a quarter,
#'   \item \code{"y"} for a year.
#' } 
#' 
#' By default, \code{period = "m"}.
#'
#' @param data
#' Either \code{NA} (the default), in which case data is simulated, or a 
#' \code{list} of controls specifying the data.
#' 
#' The \code{list} can contain the following elements, which are described 
#' in more detail below:
#' \itemize{
#'   \item \code{file}, defines the data source,
#'   \item \code{date_column}, defines the date column,
#'   \item \code{data_column}, defines the data column,
#'   \item \code{from}, defines a lower date limit,
#'   \item \code{to}, defines an upper date limit,
#'   \item \code{logreturns}, defines a data transformation to log-returns,
#'   \item \code{merge}, defines the merging for a coarse-scale observation.
#' }
#' Either none, all, or selected elements can be specified.
#' 
#' Unspecified parameters are set to their default values, see below.
#' 
#' Specifications in \code{data} override individual specifications.
#'
#' @param file
#' Either:
#' \itemize{
#'   \item A \code{data.frame}, which must have a column named 
#'         \code{date_column} (with dates) and
#'         \code{data_column} (with financial data). 
#'         If \code{hierarchy = TRUE}, this \code{data.frame} is used for
#'         both the coarse- and the fine-scale layer. To have different 
#'         data sets for these layers, \code{file} can be a \code{list}
#'         of 2 \code{data.frame}.
#'   \item A \code{character}, the path to a .csv-file with data, 
#'         which must have a column named \code{date_column} (with dates) 
#'         and \code{data_column} (with observations).
#'         If \code{hierarchy = TRUE}, \code{file} must be a \code{vector} of 
#'         length 2. The first entry corresponds to the coarse-scale layer, 
#'         while the second entry corresponds to the fine-scale layer.
#' }
#' 
#' @param date_column
#' A \code{character}, the name of the column in \code{file} with dates. 
#' Can be \code{NA} in which case consecutive integers are used 
#' as time points.
#'
#' If \code{hierarchy = TRUE}, \code{date_column} must be a \code{vector} of length 2.
#' The first entry corresponds to the coarse-scale layer, while the second entry
#' corresponds to the fine-scale layer.
#' 
#' By default, \code{date_column = "Date"} if \code{hierarchy = FALSE} and
#' \code{date_column = c("Date", "Date")} if \code{hierarchy = TRUE}.
#' 
#' @param data_column
#' A \code{character}, the name of the column in \code{file} with observations. 
#'
#' If \code{hierarchy = TRUE}, \code{data_column} must be a \code{vector} of length 2.
#' The first entry corresponds to the coarse-scale layer, while the second entry
#' corresponds to the fine-scale layer.
#' 
#' By default, \code{data_column = "Close"} if \code{hierarchy = FALSE} and
#' \code{data_column = c("Close", "Close")} if \code{hierarchy = TRUE}. 
#' 
#' @param from
#' A \code{character} of the format \code{"YYYY-MM-DD"}, setting a lower 
#' date limit. No lower limit if \code{from = NA} (default). 
#' 
#' Ignored if \code{date_column} is \code{NA}.
#'
#' @param to
#' A \code{character} of the format \code{"YYYY-MM-DD"}, setting an upper
#' date limit. No lower limit if \code{to = NA} (default). 
#' 
#' Ignored if \code{date_column} is \code{NA}.
#'
#' @param logreturns
#' A \code{logical}, if \code{TRUE} the data is transformed to log-returns. 
#'
#' If \code{hierarchy = TRUE}, \code{logreturns} must be a \code{vector} of length 2.
#' The first entry corresponds to the coarse-scale layer, while the second entry
#' corresponds to the fine-scale layer.
#' 
#' By default, \code{logreturns = FALSE}.
#'
#' @param merge
#' Only relevant if \code{hierarchy = TRUE}. 
#' 
#' In this case, a \code{function}
#' which merges an input numeric vector of fine-scale data \code{x} into one
#' coarse-scale observation. For example,
#' \itemize{
#'   \item \code{merge = function(x) mean(x)} (default) defines the mean of the
#'   fine-scale data as the coarse-scale observation,
#'   \item \code{merge = function(x) mean(abs(x))} for the mean of the
#'   absolute values,
#'   \item \code{merge = function(x) sum(abs(x))} for the sum of the
#'   absolute values,
#'   \item \code{merge = function(x) (tail(x,1)-head(x,1))/head(x,1)} for
#'   the relative change of the first to the last fine-scale observation.
#' } 
#'
#' @param fit
#' A \code{list} of controls specifying the model fitting.
#' 
#' The \code{list} can contain the following elements, which are described 
#' in more detail below:
#' \itemize{
#'   \item \code{runs}, defines the number of numerical optimization runs,
#'   \item \code{origin}, defines initialization at the true parameters,
#'   \item \code{accept}, defines the set of accepted optimization runs,
#'   \item \code{gradtol}, defines the gradient tolerance,
#'   \item \code{iterlim}, defines the iteration limit,
#'   \item \code{print.level}, defines the level of printing,
#'   \item \code{steptol}, defines the minimum allowable relative step length.
#' }
#' Either none, all, or selected elements can be specified.
#' 
#' Unspecified parameters are set to their default values, see below.
#' 
#' Specifications in \code{fit} override individual specifications.
#' 
#' @param runs
#' An \code{integer}, setting the number of randomly initialized 
#' optimization runs from which the best one is selected as the final model.
#'
#' By default, \code{runs = 100}.
#'
#' @param origin
#' Only relevant for simulated data, i.e., if \code{data} is \code{NA}. 
#'
#' In this case, a \code{logical}. If \code{origin = TRUE} the optimization is 
#' initialized at the true parameter values. 
#' This sets \code{run = 1} and \code{accept = 1:5}.
#'
#' By default, \code{origin = FALSE}.
#'
#' @param accept
#' An \code{integer} (vector), specifying which optimization runs are 
#' accepted based on the output code of \code{\link[stats]{nlm}}. 
#' 
#' By default, \code{accept = 1:3}.
#'
#' @param gradtol
#' A positive \code{numeric} value, specifying the gradient tolerance, passed on 
#' to \code{\link[stats]{nlm}}.
#'
#' By default, \code{gradtol = 1e-6}.
#'
#' @param iterlim
#' A positive \code{integer} value, specifying the iteration limit, passed on 
#' to \code{\link[stats]{nlm}}.
#'
#' By default, \code{iterlim = 200}. 
#'
#' @param print.level
#' One of \code{0}, \code{1}, and \code{2} to control the verbosity of the 
#' optimization, passed on to \code{\link[stats]{nlm}}.
#'
#' By default, \code{print.level = 0}.
#'
#' @param steptol
#' A positive \code{numeric} value, specifying the step tolerance, passed on 
#' to \code{\link[stats]{nlm}}.
#'
#' By default, \code{gradtol = 1e-6}.
#' 
#' @param ncluster
#' A positive \code{integer} value, specifying the number of clusters for
#' parallel optimization runs.
#'
#' By default, \code{ncluster = 1}.
#'
#' @param seed
#' An \code{integer}, a seed for reproducibility (e.g., for data simulation and
#' random initialization of optimization runs), passed on to 
#' \code{\link{set.seed}}.
#'
#' By default, \code{seed = NULL} (i.e., no seed).
#'
#' @param verbose
#' A \code{logical}, if \code{TRUE} progress messages are printed.
#'
#' By default, \code{verbose = TRUE}.
#' 
#' @return
#' An object of class \code{fHMM_controls}.
#' 
#' @examples
#' ### HMM controls for simulated data
#' controls <- list(
#'   states  = 2,
#'   sdds    = "t(mu = 0)",
#'   fit     = list("runs" = 50)
#' )
#' set_controls(controls)
#' 
#' ### HMM controls with empirical data 
#' data <- download_data("^GDAXI", file = NULL)
#' controls <- list(
#'   states  = 3,
#'   sdds    = "lnorm",
#'   data    = list(
#'     "file"        = data, 
#'     "date_column" = "Date", 
#'     "data_column" = "Adj.Close"
#'   )
#' )
#' set_controls(controls)
#' 
#' ### HMM controls with empirical data from .csv-file
#' controls <- list(
#'   states  = 4,
#'   sdds    = "t",
#'   data    = list(
#'     "file"        = system.file("extdata", "dax.csv", package = "fHMM"), 
#'     "date_column" = "Date", 
#'     "data_column" = "Close",
#'     "logreturns"  = TRUE
#'   )
#' )
#' set_controls(controls)
#'
#' ### Hierarchical HMM controls for simulated data
#' controls <- list(
#'   hierarchy = TRUE,
#'   states    = c(3, 2)
#' )
#' set_controls(controls)
#' 
#' ### Hierarchical HMM controls with empirical data
#' controls <- list(
#'   hierarchy = TRUE,
#'   states  = c(3, 2),
#'   sdds    = c("t", "t"),
#'   data    = list(
#'     "file"        = list(dax, vw), 
#'     "date_column" = c("Date", "Date"), 
#'     "data_column" = c("Close", "Close"),
#'     "logreturns"  = c(TRUE, TRUE)
#'   )
#' )
#' set_controls(controls)
#' 
#' @export
#' 
#' @importFrom utils read.csv

set_controls <- function(
    controls = list(), 
    hierarchy = FALSE, 
    states = if (!hierarchy) 2 else c(2, 2), 
    sdds = if (!hierarchy) "t(df = Inf)" else c("t(df = Inf)", "t(df = Inf)"), 
    horizon = if (!hierarchy) 100 else c(100, 30),
    period = "m", 
    data = NA,
    file, 
    date_column = if (!hierarchy) "Date" else c("Date", "Date"), 
    data_column = if (!hierarchy) "Close" else c("Close", "Close"), 
    from = NA, to = NA, logreturns = FALSE, merge = function(x) mean(x),
    fit = list(),
    runs = 100, origin = FALSE, accept = 1:3, gradtol = 1e-6, iterlim = 200,
    print.level = 0, steptol = 1e-6, ncluster = 1,
    seed = NULL, verbose = TRUE
  ) {
  
  ### check if 'controls' already is an 'fHMM_controls' object
  if (!inherits(controls,"fHMM_controls")) {
    
    ### initialize controls
    if (!is.list(controls)) {
      stop(
        "Input 'controls' must be a list.", 
        call. = FALSE
      )
    }

    ### define names of all controls
    all_controls <- c(
      "hierarchy", "states", "sdds", "horizon", "period", "data", "fit", "seed",
      "verbose"
    )
    data_controls <- c(
      "file", "date_column", "data_column", "from", "to", "logreturns", "merge"
    )
    fit_controls <- c(
      "runs", "origin", "accept", "gradtol", "iterlim", "print.level", 
      "steptol", "ncluster"
    )

    ### check redundant controls
    if (length(controls) > 0) {
      
      ### overall
      redundant_controls <- setdiff(names(controls), all_controls)
      if (length(redundant_controls) > 0) {
        warning(
          "Element(s) ", paste(redundant_controls, collapse = ", "), 
          " in 'controls' ignored.", 
          " Did you misspll?",
          call. = FALSE
        )
        controls[redundant_controls] <- NULL
      }
      
      ### data controls
      if (!is.null(controls[["data"]])) {
        if (!is.list(controls[["data"]])) {
          stop(
            "Element 'data' in input 'controls' must be a list.", 
            call. = FALSE
          )
        }
        redundant_data_controls <- setdiff(names(controls[["data"]]), data_controls)
        if (length(redundant_data_controls) > 0) {
          warning(
            "Element(s) ", paste(redundant_data_controls, collapse = ", "), 
            " in 'controls$data' ignored.", 
            " Did you misspll?",
            call. = FALSE
          )
          controls[["data"]][redundant_data_controls] <- NULL
        }
      }
      if (is.list(data)) {
        redundant_data_controls <- setdiff(names(data), data_controls)
        if (length(redundant_data_controls) > 0) {
          warning(
            "Element(s) ", paste(redundant_data_controls, collapse = ", "), 
            " in 'data' ignored.", 
            " Did you misspll?",
            call. = FALSE
          )
          data[redundant_data_controls] <- NULL
        }
      }
      
      ### fit controls
      if (!is.null(controls[["fit"]])) {
        if (!is.list(controls[["fit"]])) {
          stop(
            "Element 'fit' in input 'controls' must be a list.", 
            call. = FALSE
          )
        }
        redundant_fit_controls <- setdiff(names(controls[["fit"]]), fit_controls)
        if (length(redundant_fit_controls) > 0) {
          warning(
            "Element(s) ", paste(redundant_fit_controls, collapse = ", "), 
            " in 'controls$fit' ignored.", 
            " Did you misspll?",
            call. = FALSE
          )
          controls[["fit"]][redundant_fit_controls] <- NULL
        }
      }
      if (is.list(fit)) {
        redundant_fit_controls <- setdiff(names(fit), fit_controls)
        if (length(redundant_fit_controls) > 0) {
          warning(
            "Element(s) ", paste(redundant_fit_controls, collapse = ", "), 
            " in 'fit' ignored.", 
            " Did you misspll?",
            call. = FALSE
          )
          fit[redundant_fit_controls] <- NULL
        }
      }
    }
  }
  
  ### check 'hierarchy' control
  if (!is.null(controls[["hierarchy"]])) {
    hierarchy <- controls[["hierarchy"]]
  } else {
    controls[["hierarchy"]] <- hierarchy
  }
  if (!isTRUE(hierarchy) && !isFALSE(hierarchy)) {
    stop("The control 'hierarchy' must be TRUE or FALSE.", call. = FALSE)
  }
  
  ### set missing 'data' controls
  if (is.na(data)) {
    simulated <- TRUE
  }
  # TODO: continue work from here
  if (!"data" %in% names(controls) || identical(controls[["data"]], NA)) {
    controls[["data"]] <- NA
    controls[["simulated"]] <- TRUE
  } else {
    if (!"file" %in% names(controls[["data"]])) {
      controls[["data"]][["file"]] <- if (controls[["hierarchy"]]) {
        c(NA_character_, NA_character_) 
      } else {
        NA_character_
      }
      controls[["simulated"]] <- TRUE
    } else {
      controls[["simulated"]] <- FALSE
      if (!"date_column" %in% names(controls[["data"]])) {
        controls[["data"]][["date_column"]] <- if (controls[["hierarchy"]]) c("Date", "Date") else "Date"
      }
      if (!"data_column" %in% names(controls[["data"]])) {
        controls[["data"]][["data_column"]] <- if (controls[["hierarchy"]]) c("Close", "Close") else "Close"
      }
      if (!"from" %in% names(controls[["data"]])) {
        controls[["data"]][["from"]] <- NA_character_
      }
      if (!"to" %in% names(controls[["data"]])) {
        controls[["data"]][["to"]] <- NA_character_
      }
      if (!"logreturns" %in% names(controls[["data"]])) {
        controls[["data"]][["logreturns"]] <- if (controls[["hierarchy"]]) c(FALSE, FALSE) else FALSE
      }
      if (!"merge" %in% names(controls[["data"]])) {
        controls[["data"]][["merge"]] <- function(x) mean(x)
      }
    }
  }
  
  
  ### set missing 'fit' controls
  
  
  ### set missing 'controls' 
  
  
  

  ### set missing controls to default control values
  if (!"hierarchy" %in% names(controls)) {
    controls[["hierarchy"]] <- FALSE
  }
  if (!isTRUE(controls[["hierarchy"]]) && !isFALSE(controls[["hierarchy"]])) {
    stop("The control 'hierarchy' must be a TRUE or FALSE.", call. = FALSE)
  }
  if (!"states" %in% names(controls)) {
    controls[["states"]] <- if (controls[["hierarchy"]]) c(2, 2) else 2
  }
  if (!"sdds" %in% names(controls)) {
    controls[["sdds"]] <- if (controls[["hierarchy"]]) c("t(df = Inf)", "t(df = Inf)") else "t(df = Inf)"
  }
  if (!"horizon" %in% names(controls)) {
    if (!"period" %in% names(controls)) {
      controls[["horizon"]] <- if (controls[["hierarchy"]]) c(100, 30) else 100
    } else {
      controls[["horizon"]] <- if (controls[["hierarchy"]]) c(NA_integer_, NA_integer_) else NA_integer_
    }
  }
  if (!"period" %in% names(controls)) {
    controls[["period"]] <- "m"
  }
  if (!"data" %in% names(controls) || identical(controls[["data"]], NA)) {
    controls[["data"]] <- NA
    controls[["simulated"]] <- TRUE
  } else {
    if (!"file" %in% names(controls[["data"]])) {
      controls[["data"]][["file"]] <- if (controls[["hierarchy"]]) {
        c(NA_character_, NA_character_) 
      } else {
        NA_character_
      }
      controls[["simulated"]] <- TRUE
    } else {
      controls[["simulated"]] <- FALSE
      if (!"date_column" %in% names(controls[["data"]])) {
        controls[["data"]][["date_column"]] <- if (controls[["hierarchy"]]) c("Date", "Date") else "Date"
      }
      if (!"data_column" %in% names(controls[["data"]])) {
        controls[["data"]][["data_column"]] <- if (controls[["hierarchy"]]) c("Close", "Close") else "Close"
      }
      if (!"from" %in% names(controls[["data"]])) {
        controls[["data"]][["from"]] <- NA_character_
      }
      if (!"to" %in% names(controls[["data"]])) {
        controls[["data"]][["to"]] <- NA_character_
      }
      if (!"logreturns" %in% names(controls[["data"]])) {
        controls[["data"]][["logreturns"]] <- if (controls[["hierarchy"]]) c(FALSE, FALSE) else FALSE
      }
      if (!"merge" %in% names(controls[["data"]])) {
        controls[["data"]][["merge"]] <- function(x) mean(x)
      }
    }
  }
  if (!"fit" %in% names(controls)) {
    controls[["fit"]] <- list()
  }
  if (!"runs" %in% names(controls[["fit"]])) {
    controls[["fit"]][["runs"]] <- 100
  }
  if (!"origin" %in% names(controls[["fit"]])) {
    controls[["fit"]][["origin"]] <- FALSE
  }
  if (!"accept" %in% names(controls[["fit"]])) {
    controls[["fit"]][["accept"]] <- 1:3
  }
  if (!"gradtol" %in% names(controls[["fit"]])) {
    controls[["fit"]][["gradtol"]] <- 1e-6
  }
  if (!"iterlim" %in% names(controls[["fit"]])) {
    controls[["fit"]][["iterlim"]] <- 200
  }
  if (!"print.level" %in% names(controls[["fit"]])) {
    controls[["fit"]][["print.level"]] <- 0
  }
  if (!"steptol" %in% names(controls[["fit"]])) {
    controls[["fit"]][["steptol"]] <- 1e-6
  }

  ### check single controls
  if (controls[["hierarchy"]]) {
    ### controls with hierarchy
    if (!(all(is_number(controls[["states"]], int = TRUE)) &&
      length(controls[["states"]]) == 2 && all(controls[["states"]] >= 2))) {
      stop("The control 'states' must be a vector of length 2 containing integers greater or equal 2.",
           call. = FALSE)
    }
    if (!controls[["simulated"]]) {
      controls[["horizon"]][1] <- NA_integer_
    }
    if (is.na(controls[["horizon"]][1])) {
      controls[["horizon"]][1] <- NA_integer_
    }
    if (is.na(controls[["horizon"]][2])) {
      controls[["horizon"]][2] <- NA_integer_
    }
    if (all(is.na(controls[["horizon"]]))) {
      if (length(controls[["horizon"]]) != 2) {
        stop("The control 'horizon' must be a vector of length 2.",
            call. = FALSE)
      }
    } else if (!(length(controls[["horizon"]]) == 2 && all(is_number(controls[["horizon"]][!is.na(controls[["horizon"]])], int = TRUE, pos = TRUE)))) {
      stop("The control 'horizon' must be an integer vector of length 2.",
           call. = FALSE)
    }
    if (!is.na(controls[["horizon"]][2])) {
      controls[["period"]] <- NA
    }
    if (!is.na(controls[["period"]])) {
      if (!controls[["period"]] %in% c("w", "m", "q", "y")) {
        stop("The control 'period' must be eiter 'NA' or one of 'w', 'm', 'q', 'y'.",
             call. = FALSE)
      }
    }
  } else {
    ### controls without hierarchy
    if (!(is_number(controls[["states"]], int = TRUE) &&
      length(controls[["states"]]) == 1 && all(controls[["states"]] >= 2))) {
      stop("The control 'states' must be an integer greater or equal 2.",
           call. = FALSE)
    }
    if (!controls[["simulated"]]) {
      controls[["horizon"]] <- NA
    } else {
      if (!(length(controls[["horizon"]]) == 1 && is_number(controls[["horizon"]], int = TRUE, pos = TRUE))) {
        stop("The control 'horizon' must be an integer.",
             call. = FALSE)
      }
    }
    controls[["period"]] <- NA
    controls[["data"]][["merge"]] <- NA
  }
  if (!inherits(controls[["sdds"]], "fHMM_sdds")) {
    if (!is.character(controls[["sdds"]]) ||
      length(controls[["sdds"]]) != ifelse(controls[["hierarchy"]], 2, 1)) {
      stop(
        "The control 'sdds' must be a character ",
        if (controls[["hierarchy"]]) "vector ", "of length ",
        ifelse(controls[["hierarchy"]], 2, 1), ".",
        call. = FALSE
      )
    }
    controls[["sdds"]] <- fHMM_sdds(sdds = controls[["sdds"]])
  }

  ### check 'data' controls
  if (controls[["simulated"]]) {
    controls[["data"]] <- NA
  } else {
    if (controls[["hierarchy"]]) {
      ### controls with hierarchy
      if (is.data.frame(controls[["data"]][["file"]])) {
        controls[["data"]][["file"]] <- list(controls[["data"]][["file"]], controls[["data"]][["file"]])
        controls[["data"]][["data_inside"]] <- TRUE
      } else if (is.list(controls[["data"]][["file"]])) {
        data_list <- controls[["data"]][["file"]]
        if (length(data_list) != 2) {
          stop("The control 'file' in 'data' must be a list of length two.",
               call. = FALSE)
        }
        if (!all(sapply(data_list, is.data.frame))) {
          stop("The control 'file' in 'data' must be a list of two data.frame.",
               call. = FALSE)
        }
        controls[["data"]][["data_inside"]] <- TRUE
      } else {
        controls[["data"]][["data_inside"]] <- FALSE
        if (!(is.character(controls[["data"]][["file"]]) && length(controls[["data"]][["file"]]) == 2)) {
          stop("The control 'file' in 'data' must be a character vector of length two.",
              call. = FALSE)
        }
      }
      if (!(all(is.na(controls[["data"]][["date_column"]])) || (all(!is.na(controls[["data"]][["date_column"]])) && all(is.character(controls[["data"]][["date_column"]]))) &&
        length(controls[["data"]][["date_column"]]) == 2)) {
        stop("The control 'date_column' in 'data' must be a vector with two characters or two NA's.",
             call. = FALSE)
      }
      if (!(all(!is.na(controls[["data"]][["data_column"]])) && is.character(controls[["data"]][["data_column"]]) && length(controls[["data"]][["data_column"]]) == 2)) {
        stop("The control 'data_column' in 'data' must be a character vector of length two.",
             call. = FALSE)
      }
      if (!(is.logical(controls[["data"]][["logreturns"]]) && length(controls[["data"]][["logreturns"]]) == 2)) {
        stop("The control 'logreturns' in 'data' must be a boolean vector of length two.",
             call. = FALSE)
      }
      if (!is.function(controls[["data"]][["merge"]])) {
        stop("The control 'merge' in 'data' must be of class 'function'.",
             call. = FALSE)
      }
      try_merge <- try(controls[["data"]][["merge"]](-10:10), silent = TRUE)
      if (inherits(try_merge,"try-error") || !is.numeric(try_merge) || length(try_merge) != 1) {
        stop("The controls 'merge' in 'data' must merge a numeric vector into a single numeric value.",
             call. = FALSE)
      }
    } else {
      ### controls without hierarchy
      if (is.data.frame(controls[["data"]][["file"]])) {
        controls[["data"]][["file"]] <- list(controls[["data"]][["file"]])
        controls[["data"]][["data_inside"]] <- TRUE
      } else {
        controls[["data"]][["data_inside"]] <- FALSE
        if (!(is.character(controls[["data"]][["file"]]) && length(controls[["data"]][["file"]]) == 1)) {
          stop("The control 'file' in 'data' must be a character.",
               call. = FALSE)
        }
      }
      if (!((is.character(controls[["data"]][["date_column"]]) || is.na(controls[["data"]][["date_column"]])) && length(controls[["data"]][["date_column"]]) == 1)) {
        stop("The control 'date_column' in 'data' must be a character or NA.",
             call. = FALSE)
      }
      if (!(is.character(controls[["data"]][["data_column"]])) && length(controls[["data"]][["data_column"]]) == 1) {
        stop("The control 'data_column' in 'data' must be a character or NA.",
             call. = FALSE)
      }
      if (!(is.logical(controls[["data"]][["logreturns"]])) && length(controls[["data"]][["logreturns"]]) == 1) {
        stop("The control 'logreturns' in 'data' must be a boolean.",
             call. = FALSE)
      }
      controls[["data"]][["merge"]] <- NA
    }
    if (all(is.na(controls[["data"]][["date_column"]]))) {
      controls[["data"]][["from"]] <- NA_character_
      controls[["data"]][["to"]] <- NA_character_
    }
    if (!is.na(controls[["data"]][["from"]])) {
      controls[["data"]][["from"]] <- check_date(controls[["data"]][["from"]])
    }
    if (!is.na(controls[["data"]][["to"]])) {
      controls[["data"]][["to"]] <- check_date(controls[["data"]][["to"]])
    }
  }

  ### check 'fit' controls
  if (!is_number(controls[["fit"]][["runs"]], int = TRUE, pos = TRUE)) {
    stop("The control 'runs' in 'fit' must be an integer.", 
         call. = FALSE)
  }
  if (!isTRUE(controls[["fit"]][["origin"]]) && 
      !isFALSE(controls[["fit"]][["origin"]])) {
    stop("The control 'origin' in 'fit' must be a boolean.",
         call. = FALSE)
  }
  if (controls[["fit"]][["origin"]]) {
    controls[["fit"]][["runs"]] <- 1
    controls[["fit"]][["accept"]] <- 1:5
  }
  if (any(controls[["fit"]][["accept"]] == "all")) {
    controls[["fit"]][["accept"]] <- 1:5
  }
  if (!all(controls[["fit"]][["accept"]] %in% 1:5)) {
    stop("The control 'accept' in 'fit' must be vector of integers from 1 to 5.",
         call. = FALSE)
  }
  if (!(length(controls[["fit"]][["gradtol"]]) == 1 && is_number(controls[["fit"]][["gradtol"]], pos = TRUE))) {
    stop("The control 'gradtol' in 'fit' must be positive numeric value.",
         call. = FALSE)
  }
  if (!(length(controls[["fit"]][["iterlim"]]) == 1 && is_number(controls[["fit"]][["iterlim"]], int = TRUE, pos = TRUE))) {
    stop("The control 'iterlim' in 'fit' must be a positive integer.",
         call. = FALSE)
  }
  if (!(length(controls[["fit"]][["print.level"]]) == 1 && controls[["fit"]][["print.level"]] %in% 0:2)) {
    stop("The control 'print.level' in 'fit' must be one of 0, 1, and 2.",
         call. = FALSE)
  }
  if (!(length(controls[["fit"]][["steptol"]]) == 1 && is_number(controls[["fit"]][["steptol"]], pos = TRUE))) {
    stop("The control 'steptol' in 'fit' must be positive numeric value.",
         call. = FALSE)
  }

  ### check if data paths and column names are correct
  if (!controls[["simulated"]]) {
    indices <- if (controls[["hierarchy"]]) 1:2 else 1
    for (i in indices) {
      if (controls[["data"]][["data_inside"]]) {
        data <- controls[["data"]][["file"]][[i]]
        if (!is.na(controls[["data"]][["date_column"]][i])) {
          if (!controls[["data"]][["date_column"]][i] %in% colnames(data)) {
            stop("Date column '", controls[["data"]][["date_column"]][i], 
                 "' not found in supplied data.frame.",
                 call. = FALSE)
          }
        }
        if (!controls[["data"]][["data_column"]][i] %in% colnames(data)) {
          stop("Data column '", controls[["data"]][["data_column"]][i], 
               "' not found in supplied data.frame.",
               call. = FALSE)
        }
        
      } else {
        controls[["data"]][["file"]][i] <- suppressWarnings(normalizePath(controls[["data"]][["file"]][i]))
        if (!file.exists(controls[["data"]][["file"]][i])) {
          stop("File '", controls[["data"]][["file"]][i], "' not found.",
              call. = FALSE)
        }
        read_try <- suppressWarnings(
          try(utils::read.csv(file = controls[["data"]][["file"]][i]), silent = TRUE)
        )
        if (inherits(read_try,"try-error")) {
          stop("Unable to read '", controls[["data"]][["file"]][i], "'.",
              call. = FALSE)
        }
        if (!is.na(controls[["data"]][["date_column"]][i])) {
          if (!controls[["data"]][["date_column"]][i] %in% colnames(read_try)) {
            stop("Date column '", controls[["data"]][["date_column"]][i], "' not found in '", 
                 controls[["data"]][["file"]][i], "'.",
                 call. = FALSE)
          }
        }
        if (!controls[["data"]][["data_column"]][i] %in% colnames(read_try)) {
          stop("Data column '", controls[["data"]][["data_column"]][i], 
               "' not found in '", controls[["data"]][["file"]][i], "'.",
               call. = FALSE)
        }
      }
    }
  }

  ### return controls
  class(controls) <- c("fHMM_controls", "list")
  return(controls)
}

#' @rdname set_controls
#' @param x
#' An object of class \code{fHMM_controls}.
#' @param ...
#' Currently not used.
#' @exportS3Method 

print.fHMM_controls <- function(x, ...) {
  cat("fHMM controls:\n")
  cat("* hierarchy:", x[["hierarchy"]], "\n")
  cat("* data type:", ifelse(x[["simulated"]], "simulated", "empirical"), "\n")
  cat("* number of states:", x[["states"]], "\n")
  cat("* sdds: ")
  print(x[["sdds"]])
  cat("\n")
  cat(
    "* number of runs:", x[["fit"]][["runs"]],
    ifelse(x[["fit"]][["at_true"]], "(initialised at true values)", ""), "\n"
  )
  invisible(x)
}
