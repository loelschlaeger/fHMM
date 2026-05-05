#' Define and validate model specifications
#' 
#' @description
#' This function defines and validates specifications for model estimation.
#' 
#' @details
#' See the [vignette on controls](https://loelschlaeger.de/fHMM/articles/) 
#' for more details.
#' 
#' @param controls \[`list()` | `fHMM_controls`\]\cr
#' Either a \code{list} or an object of class \code{fHMM_controls}.
#'
#' The \code{list} can contain the following elements, which are described 
#' in more detail below:
#' \itemize{
#'   \item \code{hierarchy}, defines a hierarchical HMM,
#'   \item \code{states}, defines the number of states,
#'   \item \code{sdds}, defines the state-dependent distributions,
#'   \item \code{horizon}, defines the time horizon,
#'   \item \code{period}, defines a flexible, periodic fine-scale time horizon,
#'   \item \code{data}, a \code{list} of controls that define the data,
#'   \item \code{fit}, a \code{list} of controls that define the model fitting
#' }
#' Either none, all, or selected elements can be specified.
#' 
#' Unspecified parameters are set to their default values.
#' 
#' Important: Specifications in \code{controls} always override individual 
#' specifications.
#' 
#' @param hierarchy \[`logical(1)`\]\cr
#' A \code{logical}, set to \code{TRUE} for a hierarchical HMM.
#'
#' If \code{hierarchy = TRUE}, some of the other controls must be specified for
#' the coarse-scale and the fine-scale layer.
#' 
#' By default, \code{hierarchy = FALSE}.
#' 
#' @param states \[`integer(1)` | `integer(2)`\]\cr
#' An \code{integer}, the number of states of the underlying Markov chain.
#' 
#' If \code{hierarchy = TRUE}, \code{states} must be a \code{vector} of length 
#' 2. The first entry corresponds to the coarse-scale layer, while the second 
#' entry corresponds to the fine-scale layer.
#' 
#' By default, \code{states = 2} if \code{hierarchy = FALSE} and 
#' \code{states = c(2, 2)} if \code{hierarchy = TRUE}.
#'
#' @param sdds \[`character(1)` | `character(2)`\]\cr
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
#' @param horizon \[`integer(1)` | `integer(2)`\]\cr
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
#' @param period \[`NA` | `character(1)`\]\cr
#' Only relevant if \code{hierarchy = TRUE}.
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
#' By default, \code{period = NA}. If \code{period} is not \code{NA}, it
#' overrules \code{horizon[2]}.
#'
#' @param data \[`NA` | `list()`\]\cr
#' Either \code{NA}, in which case data is simulated (the default), or a 
#' \code{list} of controls specifying the empirical data set.
#' 
#' The \code{list} can contain the following elements, which are described 
#' in more detail below:
#' \itemize{
#'   \item \code{file}, defines the data set,
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
#' @param file \[`data.frame` | `character()` | `list()`\]\cr
#' A \code{data.frame} with data and dates for modeling.
#' 
#' If \code{hierarchy = TRUE}, \code{file} can be a \code{list} of 
#' length 2. The first entry is a \code{data.frame} and provides the data for 
#' the coarse-scale layer, while the second entry corresponds to the fine-scale 
#' layer. If \code{file} is a single \code{data.frame}, then the same 
#' \code{data.frame} is used for both layers.
#' 
#' Alternatively, it can be a \code{character} (of length two), the path to a 
#' .csv-file with financial data.
#' 
#' @param date_column \[`character(1)` | `character(2)`\]\cr
#' A \code{character}, the name of the column in \code{file} with dates. 
#' 
#' If \code{hierarchy = TRUE} and \code{file} is a \code{list} of two
#' \code{data.frame}s, \code{data_column} must be a \code{vector} of 
#' length 2. The first entry corresponds to the coarse-scale layer, while the 
#' second entry corresponds to the fine-scale layer.
#' 
#' By default, \code{date_column = "Date"}.
#' 
#' @param data_column \[`character(1)` | `character(2)`\]\cr
#' A \code{character}, the name of the column in \code{file} with observations. 
#'
#' If \code{hierarchy = TRUE}, \code{data_column} must be a \code{vector} of 
#' length 2. The first entry corresponds to the coarse-scale layer, while the 
#' second entry corresponds to the fine-scale layer.
#' 
#' By default, \code{data_column = "Close"} if \code{hierarchy = FALSE} and
#' \code{data_column = c("Close", "Close")} if \code{hierarchy = TRUE}. 
#' 
#' @param from \[`NA` | `character(1)`\]\cr
#' A \code{character} of the format \code{"YYYY-MM-DD"}, setting a lower 
#' date limit. No lower limit if \code{from = NA} (default). 
#'
#' @param to \[`NA` | `character(1)`\]\cr
#' A \code{character} of the format \code{"YYYY-MM-DD"}, setting an upper
#' date limit. No lower limit if \code{to = NA} (default). 
#'
#' @param logreturns \[`logical(1)` | `logical(2)`\]\cr
#' A \code{logical}, if \code{TRUE} the data is transformed to log-returns. 
#'
#' If \code{hierarchy = TRUE}, \code{logreturns} must be a \code{vector} of 
#' length 2. The first entry corresponds to the coarse-scale layer, while the 
#' second entry corresponds to the fine-scale layer.
#' 
#' By default, \code{logreturns = FALSE} if \code{hierarchy = FALSE} and
#' \code{logreturns = c(FALSE, FALSE)} if \code{hierarchy = TRUE}. 
#'
#' @param merge \[`function`\]\cr
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
#' @param fit \[`list()`\]\cr
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
#' @param runs \[`integer(1)`\]\cr
#' An \code{integer}, setting the number of randomly initialized 
#' optimization runs of the model likelihood from which the best one is selected
#' as the final model.
#'
#' By default, \code{runs = 10}.
#'
#' @param origin \[`logical(1)`\]\cr
#' Only relevant for simulated data, i.e., if the \code{data} control is 
#' \code{NA}. 
#'
#' In this case, a \code{logical}. If \code{origin = TRUE} the optimization is 
#' initialized at the true parameter values. 
#' This sets \code{run = 1} and \code{accept = 1:5}.
#'
#' By default, \code{origin = FALSE}.
#'
#' @param accept \[`integer()` | `"all"`\]\cr
#' An \code{integer} (vector), specifying which optimization runs are 
#' accepted based on the output code of \code{\link[stats]{nlm}}. 
#' 
#' By default, \code{accept = 1:3}.
#'
#' @param gradtol \[`numeric(1)`\]\cr
#' A positive \code{numeric} value, specifying the gradient tolerance, passed
#' on to \code{\link[stats]{nlm}}.
#'
#' By default, \code{gradtol = 0.01}.
#'
#' @param iterlim \[`integer(1)`\]\cr
#' A positive \code{integer} value, specifying the iteration limit, passed on 
#' to \code{\link[stats]{nlm}}.
#'
#' By default, \code{iterlim = 100}. 
#'
#' @param print.level \[`0` | `1` | `2`\]\cr
#' One of \code{0}, \code{1}, and \code{2} to control the verbosity of the 
#' numerical likelihood optimization, passed on to \code{\link[stats]{nlm}}.
#'
#' By default, \code{print.level = 0}.
#'
#' @param steptol \[`numeric(1)`\]\cr
#' A positive \code{numeric} value, specifying the step tolerance, passed on
#' to \code{\link[stats]{nlm}}.
#'
#' By default, \code{steptol = 0.01}.
#' 
#' @param x,object \[`fHMM_controls`\]\cr
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
#'   file        = download_data("^GDAXI"), # the data set
#'   date_column = "Date",                   # the column with the dates
#'   data_column = "Close"                   # the column with the data
#' )
#'
#' # hierarchical HMM with Gamma and Poisson state distributions
#' set_controls(
#'   hierarchy = TRUE,                  # defines a hierarchy
#'   states    = c(3, 2),               # coarse scale and fine scale states
#'   sdds      = c("gamma", "poisson"), # distributions for both layers
#'   horizon   = c(100, NA),            # 100 coarse-scale data points
#'   period    = "m"                    # monthly simulated fine-scale data
#' )
#' 
#' # hierarchical HMM with data from .csv-file
#' set_controls(
#'   hierarchy = TRUE,
#'   states    = c(3, 2),
#'   sdds      = c("t", "t"),
#'   file      = c(               
#'     system.file("extdata", "dax.csv", package = "fHMM"),
#'     system.file("extdata", "dax.csv", package = "fHMM")
#'   ),
#'   date_column = c("Date", "Date"), 
#'   data_column = c("Close", "Close"),
#'   logreturns  = c(TRUE, TRUE)
#' )
#' 
#' @export

set_controls <- function(
    controls = list(), 
    hierarchy = FALSE, 
    states = if (!hierarchy) 2 else c(2, 2), 
    sdds = if (!hierarchy) "normal" else c("normal", "normal"), 
    horizon = if (!hierarchy) 100 else c(100, 30),
    period = NA, 
    data = NA,
    file = NA, 
    date_column = if (!hierarchy) "Date" else c("Date", "Date"), 
    data_column = if (!hierarchy) "Close" else c("Close", "Close"), 
    from = NA, 
    to = NA, 
    logreturns = if (!hierarchy) FALSE else c(FALSE, FALSE), 
    merge = function(x) mean(x),
    fit = list(),
    runs = 10, 
    origin = FALSE, 
    accept = 1:3, 
    gradtol = 0.01, 
    iterlim = 100,
    print.level = 0, 
    steptol = 0.01
) {
  
  ### check that input 'controls' is a 'list'
  oeli::input_check_response(
    check = if (is.list(controls)) {
      TRUE
    } else {
      paste(
        "Input 'controls' must be a list or an object of class",
        "'fHMM_controls'."
      )
    },
    var_name = "controls"
  )
  
  ### define names of all possible elements in 'controls'
  all_controls <- c(
    "hierarchy", "states", "sdds", "horizon", "period", "data", "fit"
  )
  data_controls <- c(
    "file", "date_column", "data_column", "from", "to", "logreturns", "merge"
  )
  fit_controls <- c(
    "runs", "origin", "accept", "gradtol", "iterlim", "print.level", "steptol"
  )
  
  ### check for redundant controls
  redundant_controls <- setdiff(names(controls), c(all_controls, "simulated"))
  if (length(redundant_controls) > 0) {
    warning(
      "The following element(s) in 'controls' are ignored, ",
      "did you misspell?\n",
      paste(redundant_controls, collapse = ", "),
      call. = FALSE
    )
    controls[redundant_controls] <- NULL
  }
  if (!is.null(controls[["data"]]) && 
      !checkmate::test_scalar_na(controls[["data"]])) {
    oeli::input_check_response(
      check = if (is.list(controls[["data"]])) {
        TRUE
      } else {
        "Element 'data' in input 'controls' must be a list."
      },
      var_name = "controls$data"
    )
    redundant_data_controls <- setdiff(
      names(controls[["data"]]), c(data_controls, "data_inside")
    )
    if (length(redundant_data_controls) > 0) {
      warning(
        "The following element(s) in 'controls$data' are ignored, ",
        "did you misspell?\n",
        paste(redundant_data_controls, collapse = ", "),
        call. = FALSE
      )
      controls[["data"]][redundant_data_controls] <- NULL
    }
  }
  if (is.list(data)) {
    redundant_data_controls <- setdiff(
      names(data), c(data_controls, "data_inside")
    )
    if (length(redundant_data_controls) > 0) {
      warning(
        "The following element(s) in 'data' are ignored, ",
        "did you misspell?\n",
        paste(redundant_data_controls, collapse = ", "),
        call. = FALSE
      )
      data[redundant_data_controls] <- NULL
    }
  } else if (
    !(length(data) == 1 && is.atomic(data) && is.na(data))
  ) {
    oeli::input_check_response(
      check = "Control 'data' must be a list or NA.",
      var_name = "data"
    )
  }
  if (!is.null(controls[["fit"]])) {
    oeli::input_check_response(
      check = if (is.list(controls[["fit"]])) {
        TRUE
      } else {
        "Element 'fit' in input 'controls' must be a list."
      },
      var_name = "controls$fit"
    )
    redundant_fit_controls <- setdiff(names(controls[["fit"]]), fit_controls)
    if (length(redundant_fit_controls) > 0) {
      warning(
        "The following element(s) in 'controls$fit' are ignored, ",
        "did you misspell?\n",
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
        "The following element(s) in 'fit' are ignored, ",
        "did you misspell?\n",
        paste(redundant_fit_controls, collapse = ", "),
        call. = FALSE
      )
      fit[redundant_fit_controls] <- NULL
    }
  } else {
    oeli::input_check_response(
      check = "Control 'fit' must be a list.",
      var_name = "fit"
    )
  }
  
  ### check 'hierarchy' control first (because other controls depend on it)
  if (!is.null(controls[["hierarchy"]])) {
    hierarchy <- controls[["hierarchy"]]
  } else {
    controls[["hierarchy"]] <- hierarchy
  }
  oeli::input_check_response(
    check = if (checkmate::test_flag(hierarchy)) {
      TRUE
    } else {
      "The control 'hierarchy' must be TRUE or FALSE."
    },
    var_name = "hierarchy"
  )
  
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

  ### set missing 'data' controls
  if (!"data" %in% names(controls)) {
    controls[["data"]] <- data
  }
  simulated <- length(controls[["data"]]) == 1 && 
    is.atomic(controls[["data"]]) && is.na(controls[["data"]]) && 
    identical(file, NA)
  controls[["simulated"]] <- simulated
  if (!simulated) {
    if (!is.list(controls[["data"]])) {
      controls[["data"]] <- list()
    }
    if (!"file" %in% names(controls[["data"]])) {
      if (!"file" %in% names(data)) {
        oeli::input_check_response(
          check = if (!identical(file, NA)) {
            TRUE
          } else {
            "Please specify 'file'."
          },
          var_name = "file"
        )
        controls[["data"]][["file"]] <- file
      } else {
        controls[["data"]][["file"]] <- data[["file"]]
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
  
  ### validate and return controls
  validate_controls(controls)
}

#' @rdname set_controls

validate_controls <- function(controls) {
  
  ### check that 'controls' is a list
  oeli::input_check_response(
    check = if (is.list(controls)) {
      TRUE
    } else {
      "Input 'controls' must be a list."
    },
    var_name = "controls"
  )
  
  ### check general controls
  hierarchy <- controls[["hierarchy"]]
  oeli::input_check_response(
    check = if (checkmate::test_flag(hierarchy)) {
      TRUE
    } else {
      "The control 'hierarchy' must be TRUE or FALSE."
    },
    var_name = "hierarchy"
  )
  if (hierarchy) {
    oeli::input_check_response(
      check = if (
        checkmate::test_integerish(controls[["states"]], lower = 2, len = 2)
      ) {
        TRUE
      } else {
        "The control 'states' must be a vector of integers greater or equal 2."
      },
      var_name = "states"
    )
  } else {
    oeli::input_check_response(
      check = if (
        checkmate::test_integerish(controls[["states"]], lower = 2, len = 1)
      ) {
        TRUE
      } else {
        "The control 'states' must be an integer greater or equal 2."
      },
      var_name = "states"
    )
  }
  controls[["sdds"]] <- fHMM_sdds(
    sdds = controls[["sdds"]], 
    states = controls[["states"]]
  )
  simulated <- if ("simulated" %in% names(controls)) {
    controls[["simulated"]]
  } else {
    length(controls[["data"]]) == 1 && 
      is.atomic(controls[["data"]]) && is.na(controls[["data"]])
  }
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
    oeli::input_check_response(
      check = if (length(controls[["horizon"]]) == 2) {
        TRUE
      } else {
        "The control 'horizon' must be a vector of length 2."
      },
      var_name = "horizon"
    )
    if (!all(is.na(controls[["horizon"]]))) {
      oeli::input_check_response(
        check = if (
          checkmate::test_integerish(
            controls[["horizon"]], lower = 1, len = 2
          )
        ) {
          TRUE
        } else {
          "The control 'horizon' must be an integer vector of length 2."
        },
        var_name = "horizon"
      )
    }
    if (!is.na(controls[["period"]])) {
      oeli::input_check_response(
        check = if (controls[["period"]] %in% c("w", "m", "q", "y")) {
          TRUE
        } else {
          "The control 'period' must be one of 'w', 'm', 'q', 'y'."
        },
        var_name = "period"
      )
      controls[["horizon"]][2] <- NA_integer_
    }
  } else {
    if (!simulated) {
      controls[["horizon"]] <- NA_integer_
    } else {
      oeli::input_check_response(
        check = if (checkmate::test_int(controls[["horizon"]], lower = 1)) {
          TRUE
        } else {
          "The control 'horizon' must be a positive integer."
        },
        var_name = "horizon"
      )
    }
    controls[["period"]] <- NA_character_
  }
  
  ### check 'data' controls
  if (simulated) {
    controls[["data"]] <- NA
  } else {
    if (!is.na(controls[["data"]][["from"]])) {
      controls[["data"]][["from"]] <- check_date(controls[["data"]][["from"]])
    }
    if (!is.na(controls[["data"]][["to"]])) {
      controls[["data"]][["to"]] <- check_date(controls[["data"]][["to"]])
    }
    if (hierarchy) {
      if (is.data.frame(controls[["data"]][["file"]])) {
        controls[["data"]][["file"]] <- list(
          controls[["data"]][["file"]],
          controls[["data"]][["file"]]
        )
        controls[["data"]][["data_inside"]] <- TRUE
      } else if (checkmate::test_list(controls[["data"]][["file"]])) {
        oeli::input_check_response(
          check = if (
            checkmate::test_list(
              controls[["data"]][["file"]], types = "data.frame", len = 2
            )
          ) {
            TRUE
          } else {
            paste(
              "The control 'file' in 'data' must be a data.frame.\n",
              "It can also be a list with 2 'data.frame's."
            )
          },
          var_name = "data$file"
        )
        controls[["data"]][["data_inside"]] <- TRUE
      } else {
        controls[["data"]][["data_inside"]] <- FALSE
        oeli::input_check_response(
          check = if (
            checkmate::test_character(
              controls[["data"]][["file"]], len = 2, any.missing = FALSE
            )
          ) {
            TRUE
          } else {
            paste(
              "The control 'file' in 'data' must be a character vector",
              "of length two."
            )
          },
          var_name = "data$file"
        )
      }
      oeli::input_check_response(
        check = if (
          checkmate::test_character(
            controls[["data"]][["date_column"]],
            len = 2,
            any.missing = TRUE
          )
        ) {
          TRUE
        } else {
          "'date_column' in 'data' must be a character vector of length two."
        },
        var_name = "data$date_column"
      )
      oeli::input_check_response(
        check = if (
          checkmate::test_character(
            controls[["data"]][["data_column"]], len = 2
          )
        ) {
          TRUE
        } else {
          "'data_column' in 'data' must be a character vector of length two."
        },
        var_name = "data$data_column"
      )
      oeli::input_check_response(
        check = if (
          checkmate::test_logical(
            controls[["data"]][["logreturns"]], len = 2
          )
        ) {
          TRUE
        } else {
          "'logreturns' in 'data' must be a boolean vector of length two."
        },
        var_name = "data$logreturns"
      )
      oeli::input_check_response(
        check = if (is.function(controls[["data"]][["merge"]])) {
          TRUE
        } else {
          "The control 'merge' in 'data' must be a function."
        },
        var_name = "data$merge"
      )
      try_merge <- try(controls[["data"]][["merge"]](-10:10), silent = TRUE)
      oeli::input_check_response(
        check = if (
          !inherits(try_merge, "try-error") &&
            checkmate::test_number(try_merge)
        ) {
          TRUE
        } else {
          "'merge' in 'data' should merge a vector into a single number."
        },
        var_name = "data$merge"
      )
    } else {
      if (is.data.frame(controls[["data"]][["file"]])) {
        controls[["data"]][["file"]] <- list(controls[["data"]][["file"]])
        controls[["data"]][["data_inside"]] <- TRUE
      } else if (checkmate::test_string(controls[["data"]][["file"]])){
        controls[["data"]][["data_inside"]] <- FALSE
      } else if (checkmate::test_list(
        controls[["data"]][["file"]], types = "data.frame", len = 1
      )) {
        controls[["data"]][["data_inside"]] <- TRUE
      } else {
        oeli::input_check_response(
          check = paste(
            "The control 'file' in 'data' must be a 'data.frame' or a",
            "character."
          ),
          var_name = "data$file"
        )
      }
      oeli::input_check_response(
        check = if (
          checkmate::test_string(
            controls[["data"]][["date_column"]], na.ok = TRUE
          )
        ) {
          TRUE
        } else {
          "'date_column' in 'data' must be a single character."
        },
        var_name = "data$date_column"
      )
      oeli::input_check_response(
        check = if (
          checkmate::test_string(controls[["data"]][["data_column"]])
        ) {
          TRUE
        } else {
          "'data_column' in 'data' must be a single character."
        },
        var_name = "data$data_column"
      )
      oeli::input_check_response(
        check = if (checkmate::test_flag(controls[["data"]][["logreturns"]])) {
          TRUE
        } else {
          "The control 'logreturns' in 'data' must be a boolean."
        },
        var_name = "data$logreturns"
      )
      controls[["data"]][["merge"]] <- NA
    }
    for (i in if (hierarchy) 1:2 else 1) {
      if (controls[["data"]][["data_inside"]]) {
        data <- controls[["data"]][["file"]][[i]]
        if (!is.na(controls[["data"]][["date_column"]][i])) {
          if (!controls[["data"]][["date_column"]][i] %in% colnames(data)) {
            oeli::input_check_response(
              check = paste0(
                "Date column '", controls[["data"]][["date_column"]][i],
                "' not found in supplied data.frame."
              ),
              var_name = "data$date_column"
            )
          }
        }
        if (!controls[["data"]][["data_column"]][i] %in% colnames(data)) {
          oeli::input_check_response(
            check = paste0(
              "Data column '", controls[["data"]][["data_column"]][i],
              "' not found in supplied data.frame."
            ),
            var_name = "data$data_column"
          )
        }
      } else {
        controls[["data"]][["file"]][i] <- suppressWarnings(
          normalizePath(controls[["data"]][["file"]][i])
        )
        oeli::input_check_response(
          check = if (file.exists(controls[["data"]][["file"]][i])) {
            TRUE
          } else {
            paste0("File '", controls[["data"]][["file"]][i], "' not found.")
          },
          var_name = "data$file"
        )
        read_try <- suppressWarnings(
          try(
            utils::read.csv(file = controls[["data"]][["file"]][i]), 
            silent = TRUE
          )
        )
        oeli::input_check_response(
          check = if (!inherits(read_try, "try-error")) {
            TRUE
          } else {
            paste0("Unable to read '", controls[["data"]][["file"]][i], "'.")
          },
          var_name = "data$file"
        )
        if (!is.na(controls[["data"]][["date_column"]][i])) {
          if (!controls[["data"]][["date_column"]][i] %in% colnames(read_try)) {
            oeli::input_check_response(
              check = paste0(
                "Date column '", controls[["data"]][["date_column"]][i],
                "' not found in '", controls[["data"]][["file"]][i], "'."
              ),
              var_name = "data$date_column"
            )
          }
        }
        if (!controls[["data"]][["data_column"]][i] %in% colnames(read_try)) {
          oeli::input_check_response(
            check = paste0(
              "Data column '", controls[["data"]][["data_column"]][i],
              "' not found in '", controls[["data"]][["file"]][i], "'."
            ),
            var_name = "data$data_column"
          )
        }
      }
    }
  }
  
  ### check 'fit' controls
  oeli::input_check_response(
    check = if (checkmate::test_int(controls[["fit"]][["runs"]], lower = 1)) {
      TRUE
    } else {
      "The control 'runs' in 'fit' must be an integer."
    },
    var_name = "fit$runs"
  )
  oeli::input_check_response(
    check = if (checkmate::test_flag(controls[["fit"]][["origin"]])) {
      TRUE
    } else {
      "The control 'origin' in 'fit' must be a boolean."
    },
    var_name = "fit$origin"
  )
  if (controls[["fit"]][["origin"]]) {
    controls[["fit"]][["runs"]] <- 1
    controls[["fit"]][["accept"]] <- 1:5
  }
  if (any(controls[["fit"]][["accept"]] == "all")) {
    controls[["fit"]][["accept"]] <- 1:5
  }
  oeli::input_check_response(
    check = if (all(controls[["fit"]][["accept"]] %in% 1:5)) {
      TRUE
    } else {
      paste(
        "The control 'accept' in 'fit' must be a vector of integers",
        "from 1 to 5."
      )
    },
    var_name = "fit$accept"
  )
  oeli::input_check_response(
    check = if (
      checkmate::test_number(controls[["fit"]][["gradtol"]], lower = 0)
    ) {
      TRUE
    } else {
      "The control 'gradtol' in 'fit' must be a positive number."
    },
    var_name = "fit$gradtol"
  )
  oeli::input_check_response(
    check = if (
      checkmate::test_int(controls[["fit"]][["iterlim"]], lower = 1)
    ) {
      TRUE
    } else {
      "The control 'iterlim' in 'fit' must be a positive integer."
    },
    var_name = "fit$iterlim"
  )
  oeli::input_check_response(
    check = if (
      checkmate::test_int(
        controls[["fit"]][["print.level"]], lower = 0, upper = 2
      )
    ) {
      TRUE
    } else {
      "The control 'print.level' in 'fit' must be one of 0, 1, and 2."
    },
    var_name = "fit$print.level"
  )
  oeli::input_check_response(
    check = if (
      checkmate::test_number(controls[["fit"]][["steptol"]], lower = 0)
    ) {
      TRUE
    } else {
      "The control 'steptol' in 'fit' must be a positive number."
    },
    var_name = "fit$steptol"
  )
  
  ### return validated controls
  structure(controls, class = c("fHMM_controls", "list"))
}

#' @rdname set_controls
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
    ifelse(x[["fit"]][["at_true"]], "(initialized at true values)", ""), "\n"
  )
  invisible(x)
}

#' @rdname set_controls
#' @exportS3Method

summary.fHMM_controls <- function(object, ...) {
  oeli::input_check_response(
    check = if (inherits(object, "fHMM_controls")) {
      TRUE
    } else {
      "Not an object of class 'fHMM_controls'."
    },
    var_name = "object"
  )
  utils::str(object, give.attr = FALSE, give.length = FALSE, no.list = TRUE)
  invisible(object)
}
