#' Read data
#'
#' @description
#' This helper function reads financial data for the \{fHMM\} package.
#'
#' @inheritParams prepare_data
#' 
#' @return
#' A \code{list} containing the following elements:
#' \itemize{
#'  \item the \code{matrix} of the \code{dates} if \code{controls$simulated = FALSE}
#'        and \code{controls$data$data_column} is specified,
#'  \item the \code{matrix} of the \code{time_points} if \code{controls$simulated = TRUE}
#'        or \code{controls$data$data_column} is not specified,
#'  \item the \code{matrix} of the empirical \code{data} used for estimation,
#'  \item the \code{matrix} named \code{time_series} of empirical data before 
#'        the transformation to log-returns,
#'  \item the \code{vector} of fine-scale chunk sizes \code{T_star} if
#'        \code{controls$hierarchy = TRUE}.
#' }
#' 
#' @keywords internal

read_data <- function(controls) {

  ### check inputs
  if (!inherits(controls, "fHMM_controls")) {
    stop("'controls' is not of class 'fHMM_controls'.", call. = FALSE)
  }
  if (controls$simulated) {
    stop("'controls$simulated' is not 'FALSE'.", call. = FALSE)
  }

  ### read data
  data_raw <- list()
  if (controls[["data"]][["data_inside"]]) {
    for (i in 1:ifelse(controls[["hierarchy"]], 2, 1)) {
      data_raw[[i]] <- controls[["data"]][["file"]][[i]]
    }
  } else {
    for (i in 1:ifelse(controls[["hierarchy"]], 2, 1)) {
      data_raw[[i]] <- utils::read.csv(
        file = controls[["data"]][["file"]][i],
        header = TRUE, sep = ",", na.strings = "null"
      )
    }
  }

  ### check columns in data
  date_column <- controls[["data"]][["date_column"]]
  data_column <- controls[["data"]][["data_column"]]

  ### remove NA dates
  for (i in 1:ifelse(controls[["hierarchy"]], 2, 1)) {
    if (!is.na(date_column[i])) {
      data_raw[[i]] <- data_raw[[i]][!is.na(data_raw[[i]][[date_column[i]]]), ]
    }
  }
  
  ### transform dates to characters
  for (i in 1:ifelse(controls[["hierarchy"]], 2, 1)) {
    if (!is.na(date_column[i])) {
      data_raw[[i]][[date_column[i]]] <- as.character(
        check_date(data_raw[[i]][[date_column[i]]])
      )
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
  return(out)
}
