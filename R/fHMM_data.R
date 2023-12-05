#' Prepare data
#'
#' @description
#' This function prepares the data for modeling.
#'
#' @inheritParams set_controls
#'
#' @return
#' An object of class \code{\link{fHMM_data}}, which is ... TODO
#'
#' @examples
#' ### simulate data
#' controls <- set_controls()
#' prepare_data(controls)
#' 
#' ### use empirical data
#' data_frame <- download_yahoo("^GDAXI")
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
  true_parameters = NULL, 
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
    data <- simulate_hmm(controls = controls, true_parameters = true_parameters)
    
  } else {
    
    # TODO: 
    # - data_frame is a single data.frame, need to update code from here
    # - if no dates are given, add column with consecutive integers
    # - else, sort by dates (no NA allowed)
    
    
    ### read data from data.frame
    data_frame <- controls[["data"]][["data_frame"]]
    
    
    ### remove NA dates
    for (i in 1:ifelse(controls[["hierarchy"]], 2, 1)) {
      if (!is.na(date_column[i])) {
        data_raw[[i]] <- data_raw[[i]][!is.na(data_raw[[i]][[date_column[i]]]), ]
      }
    }
    
    ### replace NA values by neighbor means
    for (i in 1:ifelse(controls[["hierarchy"]], 2, 1)) {
      for (na_value in which(is.na(data_raw[[i]][[data_column[i]]]))) {
        incr <- 1
        while (TRUE) {
          range <- unique(abs(c((na_value - incr):(na_value - 1), (na_value + 1):(na_value + incr))))
          replace <- mean(data_raw[[i]][[data_column[i]]][range], na.rm = TRUE)
          if (!is.nan(replace)) {
            data_raw[[i]][[data_column[i]]][na_value] <- replace
            break
          }
          incr <- incr + 1
        }
      }
    }
    
    ### compute log-returns
    for (i in 1:ifelse(controls[["hierarchy"]], 2, 1)) {
      if (controls[["data"]][["logreturns"]][i]) {
        data_length <- length(data_raw[[i]][[data_column[i]]])
        data_raw[[i]][["logreturns"]] <- numeric(data_length)
        for (t in seq_len(data_length)[-1]) {
          data_raw[[i]][["logreturns"]][t] <- log(data_raw[[i]][[data_column[i]]][t] / data_raw[[i]][[data_column[i]]][t - 1])
        }
        
        ### remove 0 log-returns in case of gamma sdd to avoid numerical conflicts
        if (controls[["sdds"]][[i]]$name == "gamma") {
          for (t in seq_len(data_length)) {
            if (data_raw[[i]][["logreturns"]][t] == 0) {
              step <- 1
              cand <- 0
              while (cand == 0) {
                cand <- mean(data_raw[[i]][["logreturns"]][abs((t - step):(t + step))], na.rm = TRUE)
                step <- step + 1
              }
              data_raw[[i]][["logreturns"]][t] <- cand
            }
          }
        }
      }
    }
    
    if (!all(is.na(date_column))) {
      
      ### remove data points that do not occur in both files based on dates
      if (controls[["hierarchy"]]) {
        data_raw[[1]] <- data_raw[[1]][data_raw[[1]][[date_column[1]]] %in% intersect(data_raw[[1]][[date_column[1]]], data_raw[[2]][[date_column[2]]]), ]
        data_raw[[2]] <- data_raw[[2]][data_raw[[2]][[date_column[2]]] %in% intersect(data_raw[[2]][[date_column[2]]], data_raw[[1]][[date_column[1]]]), ]
      }
      
      ### function to find exact or nearest position of 'date' in 'data'
      find_date <- function(date, data) {
        incr <- 0
        while (TRUE) {
          candidate <- which(data[[date_column[i]]] == as.Date(date) + incr)
          if (length(candidate) == 1) {
            return(candidate)
          }
          candidate <- which(data[[date_column[i]]] == as.Date(date) - incr)
          if (length(candidate) == 1) {
            return(candidate)
          }
          incr <- incr + 1
        }
      }
      
      ### truncate data based on 'controls$data$from' and 'controls$data$to'
      for (i in 1:ifelse(controls[["hierarchy"]], 2, 1)) {
        t_max <- controls[["data"]][["to"]]
        if (!is.na(t_max)) {
          data_raw[[i]] <- data_raw[[i]][seq_len(find_date(t_max, data_raw[[i]])), ]
        }
        t_min <- controls[["data"]][["from"]]
        if (!is.na(t_min)) {
          temp <- seq_len(find_date(t_min, data_raw[[i]]) - 1)
          if (length(temp) > 0) {
            data_raw[[i]] <- data_raw[[i]][-temp, ]
          }
        }
      }
    }
    
    ### compute 'T_star'
    if (controls[["hierarchy"]]) {
      T_star <- compute_T_star(
        horizon = controls[["horizon"]],
        period = controls[["period"]],
        dates = as.Date(data_raw[[2]][[date_column[2]]])
      )
      T <- length(T_star)
      for (i in 1:2) {
        data_raw[[i]] <- data_raw[[i]][seq_len(sum(T_star)), ]
      }
    }
    
    ### build 'data' and 'time_series' matrix
    if (controls[["hierarchy"]]) {
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
    } else {
      data <- data_raw[[1]][, ifelse(controls[["data"]][["logreturns"]][1], "logreturns", data_column[i])]
      time_series <- data_raw[[1]][, data_column[1]]
    }
    
    ### build 'dates' or 'time_points' matrix
    if (!all(is.na(date_column))) {
      time_points <- NA
      if (controls[["hierarchy"]]) {
        dates <- matrix(NA_real_, nrow = T, ncol = max(T_star) + 1)
        for (t in seq_len(T)) {
          dates[t, -1] <- c(
            data_raw[[2]][[date_column[2]]][(sum(T_star[seq_len(t - 1)]) + 1):sum(T_star[seq_len(t)])],
            rep(NA_real_, max(T_star) - T_star[t])
          )
        }
        dates[, 1] <- dates[, 2]
      } else {
        dates <- data_raw[[1]][, date_column[1]]
      }
    } else {
      dates <- NA
      if (controls[["hierarchy"]]) {
        time_points <- matrix(NA_real_, nrow = T, ncol = max(T_star) + 1)
        time_points[, 1] <- utils::head(c(1, cumsum(T_star) + 1), -1)
        for (t in seq_len(T)) {
          time_points[t, -1] <- c(
            time_points[t, 1] - 1 + (1:T_star[t]),
            rep(NA_real_, max(T_star) - T_star[t])
          )
        }
      } else {
        time_points <- 1:length(data)
      }
    }
    
    ### return
    out <- list(
      "dates" = dates,
      "time_points" = time_points,
      "data" = data,
      "time_series" = time_series,
      "T_star" = if (controls[["hierarchy"]]) T_star else NULL
    )
    
  }
  
  ### build and return object of class 'fHMM_data'
  structure(
    list(
      "dates" = dates,
      "time_points" = time_points,
      "markov_chain" = markov_chain,
      "data" = data,
      "time_series" = time_series,
      "controls" = controls,
      "true_parameters" = true_parameters
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
  cat("fHMM", ifelse(x$controls$simulated, "simulated", "empirical"), "data\n")
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
  simulated <- object$controls[["simulated"]]
  hierarchy <- object$controls[["hierarchy"]]
  
  ### data dimensionality
  data_size <- if (!hierarchy) {
    length(object[["data"]])
  } else {
    c(
      length(object[["data"]][, 1]),
      length(object[["data"]][, -1][!is.na(object[["data"]][, -1])])
    )
  }
  fs_dim <- if (hierarchy) {
    if (!is.na(object$controls$horizon[2])) {
      object$controls$horizon[2]
    } else {
      object$controls$period
    }
  } else {
    NULL
  }
  
  ### data origin
  data_source <- if (simulated) {
    NULL
  } else if (object$controls$data$data_inside) {
    "data.frame"
  } else {
    basename(object$controls$data$file)
  }
  data_column <- if (simulated) NULL else object$controls$data$date_column
  
  ### data transformations
  log_returns <- if (!simulated) object$controls$data$logreturns else NULL
  cs_merge <- if (!simulated & hierarchy) object$controls$data$merge else NULL
  
  ### build and return summary
  out <- list(
    "simulated" = simulated,
    "hierarchy" = hierarchy,
    "data_size" = data_size,
    "fs_dim" = fs_dim,
    "data_source" = data_source,
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
  cat("Summary of fHMM", ifelse(x$simulated, "simulated", "empirical"), 
      "data\n")
  cat("* number of observations:", x$data_size, "\n")
  if (x$hierarchy) {
    cat("* fine-scale dimension:", x$fs_dim, "\n")
  }
  if (!x$simulated) {
    cat("* data source:", x$data_source, "\n")
    cat("* date column:", x$data_column, "\n")
    cat("* log returns:", x$log_returns, "\n")
    if (x$hierarchy) {
      cat("* coarse-scale merge:", deparse1(x$cs_merge, collapse = ""))
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
    if (x$controls$simulated) {
      events <- NULL
      warning("Can't have 'events' for simulated data.", call. = FALSE)
    }
  }
  if (!is.null(title)) {
    if (!(is.character(title) && length(title) == 1)) {
      stop("'title' must be a single 'character' (or 'NULL').", call. = FALSE)
    }
  }
  
  ### visualization
  plot_ts(
    data = x, decoding = NULL, colors = NULL, events = events, title = title, 
    from = from, to = to
  )
}

