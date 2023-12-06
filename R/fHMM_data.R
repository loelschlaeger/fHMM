#' Prepare data
#'
#' @description
#' This function prepares the data for modeling.
#'
#' @inheritParams set_controls
#' @param true_parameters
#' TODO
#'
#' @return
#' An object of class \code{\link{fHMM_data}}, which is ... TODO
#'
#' @examples
#' ### simulate data
#' prepare_data(states = 2, sdds = "normal", horizon = 100)
#' 
#' ### use empirical data
#' prepare_data(
#'   data_frame = download_yahoo("^GDAXI"),
#'   date_column = "Date",
#'   data_column = "Close",
#'   from = "2000-01-01",
#'   to = "2019-12-31"
#' )
#' 
#' @export

prepare_data <- function(
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
  verbose = TRUE,
  true_parameters = fHMM_parameters(
    controls = controls, hierarchy = hierarchy, states = states, sdds = sdds
  ), 
  seed = NULL
) {

  ### define controls
  controls <- set_controls(
    controls = controls, hierarchy = hierarchy, states = states, sdds = sdds,
    horizon = horizon, period = period, data = data, data_frame = data_frame,
    date_column = date_column, data_column = data_column, from = from, to = to,
    logreturns = logreturns, merge = merge, verbose = verbose, seed = seed
  )
  
  ### process data
  if (.simulated_data(controls)) {
    
    ### simulate data
    simulation <- simulate_hmm(
      controls = controls, true_parameters = true_parameters
    )
    if (fHMM_hierarchical(controls)) {
      observations <- list(
        "observations" = simulation$observations,
        "observations_fs" = simulation$observations_fs
      )
      x <- seq_along(unlist(simulation$observations_fs))
      pos <- cumsum(sapply(simulation$observations_fs, length)) + 1
      time_points <- unname(split(x, cumsum(seq_along(x) %in% pos)))
      true_states <- list(
        "states" = simulation$states,
        "states_fs" = simulation$states_fs
      )
    } else {
      observations <- simulation$observations
      time_points <- seq_along(observations) 
      true_states <- simulation$states
    }
    time_series <- NULL
    true_parameters <- simulation$parameter
    
  } else {

    ### read data and filter for relevant columns
    data_frame <- controls[["data"]][["data_frame"]]
    date_column <- controls[["data"]][["date_column"]]
    data_column <- controls[["data"]][["data_column"]]
    data_frame <- if (is.na(date_column)) {
      data_frame[, data_column]
    } else {
      data_frame[, c(date_column, data_column)]
    }
    
    ### check time points
    if (is.na(date_column)) {
      
      ### no dates available, use consecutive integers
      date_column <- ".dates"
      data_frame[, date_column] <- seq_len(nrow(data_frame))
      
    } else {
      
      ### check for NA dates (not allowed)
      if (anyNA(data_frame[[date_column]])) {
        stop(
          "The date column contains NA's, this is not allowed.",
          call. = FALSE
        )
      }
      
      ### sort by dates
      data_frame <- data_frame[order(data_frame[, date_column]), ]
      
    }
    
    ### replace NA values by neighbor means
    for (i in 1:ifelse(fHMM_hierarchical(controls), 2, 1)) {
      
      na_ids <- which(is.na(data_frame[, data_column[i]]))
      
      if (length(na_ids) > 0) {
        
        if (controls[["verbose"]]) {
          message(
            "Replacing ", length(na_ids), " NAs by neighbor means."
          )
        }
      
        for (na_value in na_ids) {
          incr <- 1
          while (TRUE) {
            range <- unique(abs(c((na_value - incr):(na_value - 1), (na_value + 1):(na_value + incr))))
            replace <- mean(data_frame[, data_column[i]][range], na.rm = TRUE)
            if (!is.nan(replace)) {
              data_frame[, data_column[i]][na_value] <- replace
              break
            }
            incr <- incr + 1
          }
        }
      }
    }
    
    ### truncate data (only if dates are available)
    if (!is.na(date_column)) {
      
      ### function to find exact or nearest position of 'date' in 'date_seq'
      ### 'date_seq' is assumed to be sorted
      find_date <- function(date, date_seq) {
        if (date <= date_seq[1]) {
          return(1)
        }
        if (date >= rev(date_seq)[1]) {
          return(length(date_seq))
        }
        incr <- 0
        while (TRUE) {
          candidate <- which(date_seq == as.Date(date) + incr)
          if (length(candidate) == 1) {
            return(candidate)
          }
          candidate <- which(date_seq == as.Date(date) - incr)
          if (length(candidate) == 1) {
            return(candidate)
          }
          incr <- incr + 1
        }
      }
      
      ### truncate data based on 'controls$data$from' and 'controls$data$to'
      to <- controls[["data"]][["to"]]
      if (!is.na(to)) {
        t_max <- find_date(to, data_frame[, date_column])
        data_frame <- data_frame[seq_len(t_max), ]
      }
      from <- controls[["data"]][["from"]]
      if (!is.na(from)) {
        t_min <- find_date(from, data_frame[, date_column])
        if (t_min > 1) {
          data_frame <- data_frame[-seq_len(t_min - 1), ]
        }
      }
    }
    
    ### save time series before transformation
    if (fHMM_hierarchical(controls)) {
      time_series <- data_frame[, data_column[2]]
    } else {
      time_series <- data_frame[, data_column]
    }
    
    ### compute log-returns
    for (i in 1:ifelse(fHMM_hierarchical(controls), 2, 1)) {
      
      if (controls[["data"]][["logreturns"]][i]) {
      
        log_returns <- numeric(nrow(data_frame))
        for (t in seq_along(log_returns)[-1]) {
          log_returns[t] <- log(data_frame[t, data_column[i]] / data_frame[t - 1, data_column[i]])
        }
        
        ### remove zero log-returns in case of gamma distribution to avoid numerical conflicts
        if (controls[["sdds"]][[i]]$distr_class == "gamma") {
          for (t in seq_len(data_length)) {
            if (log_returns[t] == 0) {
              step <- 1
              cand <- 0
              while (cand == 0) {
                cand <- mean(log_returns[abs((t - step):(t + step))], na.rm = TRUE)
                step <- step + 1
              }
              log_returns[t] <- cand
            }
          }
        }
        
        ### overwrite observations with log-returns
        data_frame[, data_column[i]] <- log_returns
      }
    }
    
    ### build output
    if (fHMM_hierarchical(controls)) {
      
      ### compute fine-scale dimension
      T_star <- compute_T_star(
        horizon = controls[["horizon"]],
        period = controls[["period"]],
        dates = as.Date(data_frame[, date_column])
      )
      
      ### build 'observations' list
      
      ### build 'time_points' list
      
      
      
      
      observations <- data_frame[, data_column]
      time_points <- data_frame[, date_column]
      
    } else {
    
      observations <- data_frame[, data_column]
      time_points <- data_frame[, date_column]
      
    }
    
    time_series <- time_series
    true_states <- NULL
    true_parameters <- NULL
    
    ### build 'data' and 'time_series' matrix
    if (fHMM_hierarchical(controls)) {
      data <- matrix(NA_real_, nrow = T, ncol = max(T_star) + 1)
      time_series <- matrix(NA_real_, nrow = T, ncol = max(T_star) + 1)
      col_name <- if (controls[["data"]][["logreturns"]][2]) "logreturns" else data_column[2]
      for (t in seq_len(T)) {
        data[t, -1] <- c(
          data_raw[[2]][[col_name]][(sum(T_star[seq_len(t - 1)]) + 1):sum(T_star[seq_len(t)])],
          rep(NA_real_, max(T_star) - T_star[t])
        )
        time_series[t, -1] <- c(
          data_raw[[2]][[data_column[2]]][(sum(T_star[seq_len(t - 1)]) + 1):sum(T_star[seq_len(t)])],
          rep(NA_real_, max(T_star) - T_star[t])
        )
      }
      col_name <- if (controls[["data"]][["logreturns"]][1]) "logreturns" else data_column[1]
      for (t in seq_len(T)) {
        cs_data_raw_t <- data_raw[[1]][[col_name]][(sum(T_star[seq_len(t - 1)]) + 1):sum(T_star[seq_len(t)])]
        data[t, 1] <- controls[["data"]][["merge"]](cs_data_raw_t)
        cs_data_raw_t <- data_raw[[1]][[data_column[1]]][(sum(T_star[seq_len(t - 1)]) + 1):sum(T_star[seq_len(t)])]
        time_series[t, 1] <- controls[["data"]][["merge"]](cs_data_raw_t)
      }
    }
    # 
    # ### build 'dates' or 'time_points' matrix
    # if (!all(is.na(date_column))) {
    #   time_points <- NA
    #   if (controls[["hierarchy"]]) {
    #     dates <- matrix(NA_real_, nrow = T, ncol = max(T_star) + 1)
    #     for (t in seq_len(T)) {
    #       dates[t, -1] <- c(
    #         data_raw[[2]][[date_column[2]]][(sum(T_star[seq_len(t - 1)]) + 1):sum(T_star[seq_len(t)])],
    #         rep(NA_real_, max(T_star) - T_star[t])
    #       )
    #     }
    #     dates[, 1] <- dates[, 2]
    #   } else {
    #     dates <- data_raw[[1]][, date_column[1]]
    #   }
    # } else {
    #   dates <- NA
    #   if (controls[["hierarchy"]]) {
    #     time_points <- matrix(NA_real_, nrow = T, ncol = max(T_star) + 1)
    #     time_points[, 1] <- utils::head(c(1, cumsum(T_star) + 1), -1)
    #     for (t in seq_len(T)) {
    #       time_points[t, -1] <- c(
    #         time_points[t, 1] - 1 + (1:T_star[t]),
    #         rep(NA_real_, max(T_star) - T_star[t])
    #       )
    #     }
    #   } else {
    #     time_points <- 1:length(data)
    #   }
    # }
    
  }
  
  ### build and return object of class 'fHMM_data'
  structure(
    list(
      "observations" = observations,
      "time_points" = time_points,
      "time_series" = time_series,
      "true_states" = true_states,
      "true_parameters" = true_parameters,
      "controls" = controls
    ),
    class = "fHMM_data"
  )
}

#' @rdname prepare_data
#' @param x
#' An object of class \code{fHMM_data}.
#' @param ...
#' Currently not used.
#' @exportS3Method 

print.fHMM_data <- function(x, ...) {
  cat(
    "fHMM", 
    ifelse(.simulated_data(x$controls), "simulated", "empirical"), 
    "data\n"
  )
  return(invisible(x))
}

#' @rdname prepare_data
#' @param object
#' An object of class \code{fHMM_data}.
#' @param ...
#' Currently not used.
#' @exportS3Method 

summary.fHMM_data <- function(object, ...) {
  
  ### meta data
  simulated <- .simulated_data(object$controls)
  hierarchy <- fHMM_hierarchical(object$controls)
  
  ### data dimensionality
  data_size <- if (!hierarchy) {
    length(object[["observations"]])
  } else {
    c(
      length(object[["observations"]][[1]]),
      length(unlist(object[["observations"]][[2]]))
    )
  }
  fs_dim <- if (hierarchy) {
    if (!is.na(object$controls$horizon[2])) {
      object[["controls"]][["horizon"]][2]
    } else {
      object[["controls"]][["period"]]
    }
  } else {
    NULL
  }
  
  ### data origin
  data_column <- if (simulated) {
    NULL 
  } else {
    object[["controls"]][["data"]][["data_column"]]
  }
  date_column <- if (simulated) {
    NULL 
  } else {
    object[["controls"]][["data"]][["date_column"]]
  }
  
  ### data transformations
  log_returns <- if (!simulated) {
    object$controls$data$logreturns
  } else {
    NULL
  }
  cs_merge <- if (!simulated & hierarchy) {
    object$controls$data$merge 
  } else {
    NULL
  }
  
  ### build and return summary
  out <- list(
    "simulated" = simulated,
    "hierarchy" = hierarchy,
    "data_size" = data_size,
    "fs_dim" = fs_dim,
    "date_column" = date_column,
    "data_column" = data_column,
    "log_returns" = log_returns,
    "cs_merge" = cs_merge
  )
  class(out) <- "summary.fHMM_data"
  return(out)
}

#' @noRd
#' @exportS3Method 

print.summary.fHMM_data <- function(x, ...) {
  cat(
    "Summary of fHMM", ifelse(x$simulated, "simulated", "empirical"), "data\n"
  )
  cat("* number of observations:", x$data_size, "\n")
  if (x$hierarchy) {
    cat("* fine-scale dimension:", x$fs_dim, "\n")
  }
  if (!x$simulated) {
    cat("* date column:", x$date_column, "\n")
    cat("* data column:", x$data_column, "\n")
    cat("* log returns:", x$log_returns, "\n")
    if (x$hierarchy) {
      cat("* coarse-scale merge:", oeli::function_body(x$cs_merge))
    }
  }
  return(invisible(x))
}

#' @rdname prepare_data
#' @param x
#' An object of class \code{fHMM_data}.
#' @inheritParams plot.fHMM_model
#' @exportS3Method 

plot.fHMM_data <- function(
    x, events = NULL, title = NULL, from = NULL, to = NULL, ...
) {
  
  ### check input
  if (!inherits(x, "fHMM_data")) {
    stop("'x' is not of class 'fHMM_data'.", call. = FALSE)
  }
  if (!is.null(events)) {
    if (!inherits(events, "fHMM_events")) {
      stop("'events' is not of class 'fHMM_events'.", call. = FALSE)
    }
    if (.simulated_data(x$controls)) {
      events <- NULL
      warning("Can't have 'events' for simulated data.", call. = FALSE)
    }
  }
  if (!checkmate::test_string(title, null.ok = TRUE)) {
    stop("'title' must be a single 'character' (or 'NULL').", call. = FALSE)
  }
  
  ### visualization
  plot_ts(
    data = x, decoding = NULL, colors = NULL, events = events, title = title, 
    from = from, to = to
  )
}

