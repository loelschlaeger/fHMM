#' Define model specifications
#' 
#' @description
#' This function defines specifications for model estimation.
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
#'   \item \code{fit}, a \code{list} of controls that define the model fitting,
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
#' the coarse-scale and the fine-scale layer.
#' 
#' By default, \code{hierarchy = FALSE}.
#' 
#' @param states
#' An \code{integer}, the number of states of the underlying Markov chain.
#' 
#' If \code{hierarchy = TRUE}, \code{states} must be a \code{vector} of length 
#' 2. The first entry corresponds to the coarse-scale layer, while the second 
#' entry corresponds to the fine-scale layer.
#' 
#' By default, \code{states = 2} if \code{hierarchy = FALSE} and 
#' \code{states = c(2, 2)} if \code{hierarchy = TRUE}.
#'
#' @param sdds
#' A \code{character}, specifying the state-dependent distribution. One of 
#' \itemize{
#'   \item \code{"normal"} (the normal distribution),
#'   \item \code{"lognormal"} (the log-normal distribution),
#'   \item \code{"t"} (the t-distribution),
#'   \item \code{"gamma"} (the gamma distribution),
#'   \item \code{"poisson"} (the Poisson distribution).
#' }
#' 
#' The distribution parameters, i.e. the 
#' \itemize{
#'   \item mean \code{mu},
#'   \item standard deviation \code{sigma} (not for the Poisson distribution),
#'   \item degrees of freedom \code{df} (only for the t-distribution),
#' }
#' can be fixed via, e.g., \code{"t(df = 1)"} or 
#' \code{"gamma(mu = 0, sigma = 1)"}.
#' To fix different values of a parameter for different states, separate by
#' "|", e.g. \code{"poisson(mu = 1|2|3)"}. 
#' 
#' If \code{hierarchy = TRUE}, \code{sdds} must be a \code{vector} of length 2.
#' The first entry corresponds to the coarse-scale layer, while the second entry
#' corresponds to the fine-scale layer.
#' 
#' By default, \code{sdds = "normal"} if \code{hierarchy = FALSE} and 
#' \code{sdds = c("normal", "normal")} if \code{hierarchy = TRUE}.
#'
#' See also \code{\link{fHMM_sdds}} for more details.
#'
#' @param horizon
#' A \code{numeric}, specifying the length of the time horizon. 
#' 
#' If \code{hierarchy = TRUE}, \code{horizon} must be a \code{vector} of length 
#' 2. The first entry corresponds to the coarse-scale layer, while the second 
#' entry corresponds to the fine-scale layer.
#' 
#' By default, \code{horizon = 100} if \code{hierarchy = FALSE} and 
#' \code{horizon = c(100, 30)} if \code{hierarchy = TRUE}.
#' 
#' If \code{data} is specified (i.e., not \code{NA}), the first entry of 
#' \code{horizon} is ignored and the (coarse-scale) time horizon is defined by
#' available data.
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
#' By default, \code{period = "m"} if \code{hierarchy = TRUE} and 
#' \code{horizon[2] = NA}, and \code{NA} else.
#'
#' @param data
#' Either \code{NA}, in which case data is simulated (the default), or a 
#' \code{list} of controls specifying the empirical data set.
#' 
#' The \code{list} can contain the following elements, which are described 
#' in more detail below:
#' \itemize{
#'   \item \code{data_frame}, defines the data set,
#'   \item \code{date_column}, defines the date column,
#'   \item \code{data_column}, defines the data column,
#'   \item \code{from}, defines a lower date limit,
#'   \item \code{to}, defines an upper date limit,
#'   \item \code{logreturns}, defines a data transformation to log-returns,
#'   \item \code{merge}, defines the merging for coarse-scale observations.
#' }
#' Either none, all, or selected elements can be specified.
#' 
#' Unspecified parameters are set to their default values, see below.
#' 
#' Specifications in \code{data} override individual specifications.
#'
#' @param data_frame
#' A \code{data.frame} with data and dates for modeling.
#' 
#' @param date_column
#' A \code{character}, the name of the column in \code{data_frame} with dates. 
#' Can also be \code{NA} in which case consecutive integers are used 
#' as time points.
#' 
#' By default, \code{date_column = "Date"}.
#' 
#' @param data_column
#' A \code{character}, the name of the column in \code{data_frame} with observations. 
#'
#' If \code{hierarchy = TRUE}, \code{data_column} must be a \code{vector} of 
#' length 2. The first entry corresponds to the coarse-scale layer, while the 
#' second entry corresponds to the fine-scale layer.
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
#' If \code{hierarchy = TRUE}, \code{logreturns} must be a \code{vector} of 
#' length 2. The first entry corresponds to the coarse-scale layer, while the 
#' second entry corresponds to the fine-scale layer.
#' 
#' By default, \code{logreturns = FALSE} if \code{hierarchy = FALSE} and
#' \code{logreturns = c(FALSE, FALSE)} if \code{hierarchy = TRUE}. 
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
#'   \item \code{merge = function(x) (tail(x, 1) - head(x, 1)) / head(x, 1)} for
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
#' optimization runs of the model likelihood from which the best one is selected
#' as the final model.
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
#' numerical likelihood optimization, passed on to \code{\link[stats]{nlm}}.
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
#' parallel optimization runs, passed on to \code{\link[parallel]{makeCluster}}. 
#'
#' By default, \code{ncluster = 1}.
#'
#' @param seed
#' An \code{integer}, a seed for reproducibility, passed on to 
#' \code{\link{set.seed}}.
#'
#' By default, \code{seed = NULL} (i.e., no seed).
#'
#' @param verbose
#' A \code{logical}, if \code{TRUE} progress messages are printed.
#'
#' By default, \code{verbose = TRUE}.
#' 
#' @param x,object
#' An object of class \code{fHMM_controls}.
#' 
#' @param ...
#' Currently not used.
#' 
#' @return
#' An object of class \code{fHMM_controls}, which is a \code{list} that contains
#' model and estimation specifications.
#' 
#' @examples
#' # 2-state HMM with t-distributions for simulated data
#' set_controls(
#'   states = 2,   # the number of states
#'   sdds   = "t", # the state-dependent distribution
#'   runs   = 50   # the number of optimization runs
#' )
#' 
#' # 3-state HMM with normal distributions for the DAX closing prices
#' set_controls(
#'   states      = 3,
#'   sdds        = "normal",
#'   data_frame  = download_yahoo("^GDAXI"), # the data set
#'   date_column = "Date",                   # the column with the dates
#'   data_column = "Close"                   # the column with the data
#' )
#'
#' # hierarchical HMM with Gamma and Poisson state distributions
#' set_controls(
#'   hierarchy = TRUE,                  # defines a hierarchy
#'   states    = c(3, 2),               # coarse scale and fine scale states
#'   sdds      = c("gamma", "poisson"), # distributions for both layers
#'   horizon   = c(100, NA),            # 100 simulated coarse-scale data points 
#'   period    = "m"                    # monthly simulated fine-scale data
#' )
#' 
#' @export

set_controls <- function(
    controls = list(), 
    hierarchy = FALSE, 
    states = if (!hierarchy) 2 else c(2, 2), 
    sdds = if (!hierarchy) "normal" else c("normal", "normal"), 
    horizon = if (!hierarchy) 100 else c(100, 30),
    period = if (hierarchy && is.na(horizon[2])) "m" else NA, 
    data = NA,
    data_frame = NA, 
    date_column = "Date", 
    data_column = if (!hierarchy) "Close" else c("Close", "Close"), 
    from = NA, 
    to = NA, 
    logreturns = if (!hierarchy) FALSE else c(FALSE, FALSE), 
    merge = function(x) mean(x),
    fit = list(),
    runs = 100, 
    origin = FALSE, 
    accept = 1:3, 
    gradtol = 1e-6, 
    iterlim = 200,
    print.level = 0, 
    steptol = 1e-6, 
    ncluster = 1,
    seed = NULL, 
    verbose = TRUE
  ) {
  
  ### check that input 'controls' is a 'list'
  if (!is.list(controls)) {
    stop(
      "Input 'controls' must be a list or an object of class 'fHMM_controls'.", 
      call. = FALSE
    )
  }

  ### define names of all possible elements in 'controls'
  all_controls <- c(
    "hierarchy", "states", "sdds", "horizon", "period", "data", "fit", "seed",
    "verbose"
  )
  data_controls <- c(
    "data_frame", "date_column", "data_column", "from", "to", "logreturns", "merge"
  )
  fit_controls <- c(
    "runs", "origin", "accept", "gradtol", "iterlim", "print.level", 
    "steptol", "ncluster"
  )
    
  ### check for redundant controls
  redundant_controls <- setdiff(names(controls), all_controls)
  if (length(redundant_controls) > 0) {
    warning(
      "The following element(s) in 'controls' are ignored, did you misspll?\n", 
      paste(redundant_controls, collapse = ", "),
      call. = FALSE
    )
    controls[redundant_controls] <- NULL
  }
  if (!is.null(controls[["data"]]) && 
      !checkmate::test_scalar_na(controls[["data"]])) {
    if (!is.list(controls[["data"]])) {
      stop(
        "Element 'data' in input 'controls' must be a list.", 
        call. = FALSE
      )
    }
    redundant_data_controls <- setdiff(names(controls[["data"]]), data_controls)
    if (length(redundant_data_controls) > 0) {
      warning(
        "The following element(s) in 'controls$data' are ignored, did you misspll?\n", 
        paste(redundant_data_controls, collapse = ", "),
        call. = FALSE
      )
      controls[["data"]][redundant_data_controls] <- NULL
    }
  }
  if (is.list(data)) {
    redundant_data_controls <- setdiff(names(data), data_controls)
    if (length(redundant_data_controls) > 0) {
      warning(
        "The following element(s) in 'data' are ignored, did you misspll?\n", 
        paste(redundant_data_controls, collapse = ", "),
        call. = FALSE
      )
      data[redundant_data_controls] <- NULL
    }
  } else if (!is.na(data)) {
    stop(
      "Control 'data' must be a list or NA.",
      call. = FALSE
    )
  }
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
        "The following element(s) in 'controls$fit' are ignored, did you misspll?\n", 
        paste(redundant_fit_controls, collapse = ", "),
        call. = FALSE
      )
      controls[["fit"]][redundant_fit_controls] <- NULL
    }
  }
  if (is.list(fit)) {
    redundant_fit_controls <- setdiff(names(fit), fit_controls)
    if (length(redundant_fit_controls) > 0) {
      warning(
        "The following element(s) in 'fit' are ignored, did you misspll?\n", 
        paste(redundant_fit_controls, collapse = ", "),
        call. = FALSE
      )
      fit[redundant_fit_controls] <- NULL
    }
  } else {
    stop(
      "Control 'fit' must be a list.",
      call. = FALSE
    )
  }
  
  ### check 'hierarchy' control (because other controls depend on it)
  if (!is.null(controls[["hierarchy"]])) {
    hierarchy <- controls[["hierarchy"]]
  } else {
    controls[["hierarchy"]] <- hierarchy
  }
  if (!checkmate::test_flag(hierarchy)) {
    stop("The control 'hierarchy' must be TRUE or FALSE.", call. = FALSE)
  }
  
  ### set missing general controls
  if (!"states" %in% names(controls)) {
    controls[["states"]] <- states
  }
  if (!"sdds" %in% names(controls)) {
    controls[["sdds"]] <- sdds
  }
  if (!"horizon" %in% names(controls)) {
    controls[["horizon"]] <- horizon
  }
  if (!"period" %in% names(controls)) {
    controls[["period"]] <- period
  }
  if (!"seed" %in% names(controls)) {
    controls[["seed"]] <- seed
  }
  if (!"verbose" %in% names(controls)) {
    controls[["verbose"]] <- verbose
  }

  ### set missing 'data' controls
  if (!"data" %in% names(controls)) {
    controls[["data"]] <- data
  }
  simulated <- .simulated_data(controls) && identical(data_frame, NA)
  if (!simulated) {
    if (!is.list(controls[["data"]])) {
      controls[["data"]] <- list()
    }
    if (!"data_frame" %in% names(controls[["data"]])) {
      if (!"data_frame" %in% names(data)) {
        if (identical(data_frame, NA)) {
          stop("Please specify 'data_frame'.", call. = FALSE)
        }
        controls[["data"]][["data_frame"]] <- data_frame
      } else {
        controls[["data"]][["data_frame"]] <- data[["data_frame"]]
      }
    }
    if (!"date_column" %in% names(controls[["data"]])) {
      if (!"date_column" %in% names(data)) {
        controls[["data"]][["date_column"]] <- date_column
      } else {
        controls[["data"]][["date_column"]] <- data[["date_column"]]
      }
    }
    if (!"data_column" %in% names(controls[["data"]])) {
      if (!"data_column" %in% names(data)) {
        controls[["data"]][["data_column"]] <- data_column
      } else {
        controls[["data"]][["data_column"]] <- data[["data_column"]]
      }
    }
    if (!"from" %in% names(controls[["data"]])) {
      if (!"from" %in% names(data)) {
        controls[["data"]][["from"]] <- from
      } else {
        controls[["data"]][["from"]] <- data[["from"]]
      }
    }
    if (!"to" %in% names(controls[["data"]])) {
      if (!"to" %in% names(data)) {
        controls[["data"]][["to"]] <- to
      } else {
        controls[["data"]][["to"]] <- data[["to"]]
      }
    }
    if (!"logreturns" %in% names(controls[["data"]])) {
      if (!"logreturns" %in% names(data)) {
        controls[["data"]][["logreturns"]] <- logreturns
      } else {
        controls[["data"]][["logreturns"]] <- data[["logreturns"]]
      }
    }
    if (!"merge" %in% names(controls[["data"]])) {
      if (!"merge" %in% names(data)) {
        controls[["data"]][["merge"]] <- merge
      } else {
        controls[["data"]][["merge"]] <- data[["merge"]]
      }
    }
  }
  
  ### set missing 'fit' controls
  if (!"fit" %in% names(controls)) {
    controls[["fit"]] <- fit
  }
  if (!"runs" %in% names(controls[["fit"]])) {
    if (!"runs" %in% names(fit)) {
      controls[["fit"]][["runs"]] <- runs
    } else {
      controls[["fit"]][["runs"]] <- fit[["runs"]]
    }
  }
  if (!"origin" %in% names(controls[["fit"]])) {
    if (!"origin" %in% names(fit)) {
      controls[["fit"]][["origin"]] <- origin
    } else {
      controls[["fit"]][["origin"]] <- fit[["origin"]]
    }
  }
  if (!"accept" %in% names(controls[["fit"]])) {
    if (!"accept" %in% names(fit)) {
      controls[["fit"]][["accept"]] <- accept
    } else {
      controls[["fit"]][["accept"]] <- fit[["accept"]]
    }
  }  
  if (!"gradtol" %in% names(controls[["fit"]])) {
    if (!"gradtol" %in% names(fit)) {
      controls[["fit"]][["gradtol"]] <- gradtol
    } else {
      controls[["fit"]][["gradtol"]] <- fit[["gradtol"]]
    }
  }
  if (!"iterlim" %in% names(controls[["fit"]])) {
    if (!"iterlim" %in% names(fit)) {
      controls[["fit"]][["iterlim"]] <- iterlim
    } else {
      controls[["fit"]][["iterlim"]] <- fit[["iterlim"]]
    }
  }
  if (!"print.level" %in% names(controls[["fit"]])) {
    if (!"print.level" %in% names(fit)) {
      controls[["fit"]][["print.level"]] <- print.level
    } else {
      controls[["fit"]][["print.level"]] <- fit[["print.level"]]
    }
  }
  if (!"steptol" %in% names(controls[["fit"]])) {
    if (!"steptol" %in% names(fit)) {
      controls[["fit"]][["steptol"]] <- steptol
    } else {
      controls[["fit"]][["steptol"]] <- fit[["steptol"]]
    }
  }
  if (!"ncluster" %in% names(controls[["fit"]])) {
    if (!"ncluster" %in% names(fit)) {
      controls[["fit"]][["ncluster"]] <- ncluster
    } else {
      controls[["fit"]][["ncluster"]] <- fit[["ncluster"]]
    }
  }
  
  ### validate and return controls
  validate_controls(controls)
}

#' Helper function that checks if data was simulated
#' @inheritParams set_controls
#' @keywords internal

.simulated_data <- function(controls) {
  checkmate::assert_list(controls)
  length(controls[["data"]]) == 1 && 
    is.atomic(controls[["data"]]) && is.na(controls[["data"]])
}

#' Helper function that sets a seed if specified
#' @inheritParams set_controls
#' @keywords internal

fHMM_seed <- function(controls) {
  checkmate::assert_list(controls)
  if ("seed" %in% names(controls) && !is.null(controls$seed)) {
    set.seed(controls$seed)
  }
}

#' Helper function that checks if the model is hierarchical
#' @inheritParams set_controls
#' @keywords internal

fHMM_hierarchical <- function(controls) {
  checkmate::assert_list(controls)
  hierarchy <- controls[["hierarchy"]]
  if (!checkmate::test_flag(hierarchy)) {
    stop("The control 'hierarchy' must be TRUE or FALSE.", call. = FALSE)
  }
  hierarchy
}

#' @rdname set_controls

validate_controls <- function(controls) {
  
  ### check that 'controls' is a list
  if (!is.list(controls)) {
    stop("Input 'controls' must be a list.", call. = FALSE)
  }
  
  ### check general controls
  hierarchy <- controls[["hierarchy"]]
  if (!checkmate::test_flag(hierarchy)) {
    stop("The control 'hierarchy' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!checkmate::test_flag(controls[["verbose"]])) {
    stop("The control 'verbose' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(controls[["seed"]])) {
    if (!checkmate::test_int(controls[["seed"]])) {
      stop("The control 'seed' must be an integer.", call. = FALSE)
    }
  }
  if (hierarchy) {
    if (!checkmate::test_integerish(controls[["states"]], lower = 2, len = 2)) {
      stop(
        "The control 'states' must be a vector of integers greater or equal 2.",
        call. = FALSE
      )
    }
  } else {
    if (!checkmate::test_integerish(controls[["states"]], lower = 2, len = 1)) {
      stop(
        "The control 'states' must be an integer greater or equal 2.",
        call. = FALSE
      )
    }
  }
  controls[["sdds"]] <- fHMM_sdds(
    sdds = controls[["sdds"]], 
    states = controls[["states"]]
  )
  simulated <- .simulated_data(controls)
  if (hierarchy) {
    if (!simulated) {
      controls[["horizon"]][1] <- NA_integer_
    }
    if (is.na(controls[["horizon"]][1])) {
      controls[["horizon"]][1] <- NA_integer_
    }
    if (is.na(controls[["horizon"]][2])) {
      controls[["horizon"]][2] <- NA_integer_
    }
    if (length(controls[["horizon"]]) != 2) {
      stop("The control 'horizon' must be a vector of length 2.", call. = FALSE)
    } 
    if (!all(is.na(controls[["horizon"]]))) {
      if (!checkmate::test_integerish(controls[["horizon"]], lower = 1, len = 2)) {
        stop(
          "The control 'horizon' must be an integer vector of length 2.",
          call. = FALSE
        )
      }
    }
    if (!is.na(controls[["horizon"]][2])) {
      controls[["period"]] <- NA_character_
    }
    if (!is.na(controls[["period"]])) {
      if (!controls[["period"]] %in% c("w", "m", "q", "y")) {
        stop(
          "The control 'period' must be one of 'w', 'm', 'q', 'y'.",
          call. = FALSE
        )
      }
    }
  } else {
    if (!simulated) {
      controls[["horizon"]] <- NA_integer_
    } else {
      if (!checkmate::test_int(controls[["horizon"]], lower = 1)) {
        stop("The control 'horizon' must be a positive integer.",
             call. = FALSE)
      }
    }
    controls[["period"]] <- NA_character_
  }
  
  ### check 'data' controls
  if (simulated) {
    controls[["data"]] <- NA
  } else {
    if (!is.data.frame(controls[["data"]][["data_frame"]])) {
      stop(
        "The control 'data_frame' in 'data' must be a data.frame.",
        call. = FALSE
      )
    }
    if (!is.na(controls[["data"]][["from"]])) {
      controls[["data"]][["from"]] <- oeli::check_date(controls[["data"]][["from"]])
    }
    if (!is.na(controls[["data"]][["to"]])) {
      controls[["data"]][["to"]] <- oeli::check_date(controls[["data"]][["to"]])
    }
    if (!checkmate::test_string(
      controls[["data"]][["date_column"]], na.ok = TRUE)
    ) {
      stop(
        "'date_column' in 'data' must be a single character (or NA).",
        call. = FALSE
      )
    }
    if (hierarchy) {
      if (!checkmate::test_character(
        controls[["data"]][["data_column"]], len = 2
      )) {
        stop(
          "'data_column' in 'data' must be a character vector of length two.",
          call. = FALSE
        )
      }
      if (!checkmate::test_logical(controls[["data"]][["logreturns"]], len = 2)) {
        stop(
          "'logreturns' in 'data' must be a boolean vector of length two.",
          call. = FALSE
        )
      }
      if (!is.function(controls[["data"]][["merge"]])) {
        stop(
          "The control 'merge' in 'data' must be a function.",
          call. = FALSE
        )
      }
      try_merge <- try(controls[["data"]][["merge"]](-10:10), silent = TRUE)
      if (inherits(try_merge,"try-error") || !checkmate::test_number(try_merge)) {
        stop(
          "'merge' in 'data' should merge a vector into a single number.",
          call. = FALSE
        )
      }
    } else {
      if (!checkmate::test_string(controls[["data"]][["data_column"]])) {
        stop(
          "'data_column' in 'data' must be a single character.",
          call. = FALSE
        )
      }
      if (!checkmate::test_flag(controls[["data"]][["logreturns"]])) {
        stop(
          "The control 'logreturns' in 'data' must be a boolean.",
          call. = FALSE
        )
      }
      controls[["data"]][["merge"]] <- NA
    }
    data <- controls[["data"]][["data_frame"]]
    if (!is.na(controls[["data"]][["date_column"]])) {
      if (!controls[["data"]][["date_column"]] %in% colnames(data)) {
        stop(
          "Column '", controls[["data"]][["date_column"]], 
          "' not found in data.frame 'data_frame'.",
          call. = FALSE
        )
      }
    }
    for (i in if (hierarchy) 1:2 else 1) {
      if (!controls[["data"]][["data_column"]][i] %in% colnames(data)) {
        stop(
          "Column '", controls[["data"]][["data_column"]][i], 
          "' not found in data.frame 'data_frame'.",
          call. = FALSE
        )
      }
    }
  }
  
  ### check 'fit' controls
  if (!checkmate::test_int(controls[["fit"]][["runs"]], lower = 1)) {
    stop(
      "The control 'runs' in 'fit' must be an integer.", 
      call. = FALSE
    )
  }
  if (!checkmate::test_flag(controls[["fit"]][["origin"]])) {
    stop(
      "The control 'origin' in 'fit' must be a boolean.",
      call. = FALSE
    )
  }
  if (controls[["fit"]][["origin"]]) {
    controls[["fit"]][["runs"]] <- 1
    controls[["fit"]][["accept"]] <- 1:5
  }
  if (any(controls[["fit"]][["accept"]] == "all")) {
    controls[["fit"]][["accept"]] <- 1:5
  }
  if (!all(controls[["fit"]][["accept"]] %in% 1:5)) {
    stop(
      "The control 'accept' in 'fit' must be a vector of integers from 1 to 5.",
      call. = FALSE
    )
  }
  if (!checkmate::test_number(controls[["fit"]][["gradtol"]], lower = 0)) {
    stop(
      "The control 'gradtol' in 'fit' must be a positive number.",
      call. = FALSE
    )
  }
  if (!checkmate::test_int(controls[["fit"]][["iterlim"]], lower = 1)) {
    stop("The control 'iterlim' in 'fit' must be a positive integer.",
         call. = FALSE)
  }
  if (!checkmate::test_int(
    controls[["fit"]][["print.level"]], lower = 0, upper = 2
  )) {
    stop(
      "The control 'print.level' in 'fit' must be one of 0, 1, and 2.",
      call. = FALSE
    )
  }
  if (!checkmate::test_number(controls[["fit"]][["steptol"]], lower = 0)) {
    stop(
      "The control 'steptol' in 'fit' must be a positive number.",
      call. = FALSE
    )
  }
  
  ### return validated controls
  structure(controls, class = c("fHMM_controls", "list"))
}

#' @rdname set_controls
#' @exportS3Method 

print.fHMM_controls <- function(x, ...) {
  if (!inherits(x, "fHMM_controls")) {
    stop("Not an object of class 'fHMM_controls'.", call. = FALSE)
  }
  cat("Model:", ifelse(x[["hierarchy"]], "HHMM", "HMM"), "\n")
  cat("States:", x[["states"]], "\n")
  print(x[["sdds"]])
  cat("Data:", ifelse(.simulated_data(x), "simulated", "empirical"), "\n")
  invisible(x)
}

#' @rdname set_controls
#' @exportS3Method

summary.fHMM_controls <- function(object, ...) {
  if (!inherits(object, "fHMM_controls")) {
    stop("Not an object of class 'fHMM_controls'.", call. = FALSE)
  }
  str(object, give.attr = FALSE, give.length = FALSE, no.list = TRUE)
  invisible(object)
}
