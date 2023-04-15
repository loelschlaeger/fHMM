#' Constructor of an \code{fHMM_data} object
#'
#' @description
#' This function constructs an object of class \code{fHMM_data}, which contains
#' the financial data for modeling.
#' 
#' @param dates
#' The dates in the empirical case.
#' @param time_points
#' The time points in the simulated case.
#' @param markov_chain
#' The states in the simulated case.
#' @param data
#' The data for modeling.
#' @param time_series
#' The data before transformation.
#' @param T_star
#' The fine-scale chunk sizes.
#' @param controls
#' The \code{fHMM_controls} object.
#' @param true_parameters
#' The \code{fHMM_parameters} object in the simulated case.
#' 
#' @return 
#' An object of class \code{fHMM_data}, which is a \code{list} containing 
#' the following elements:
#' \itemize{
#'  \item The \code{matrix} of the \code{dates} if \code{simulated = FALSE} and
#'        \code{controls$data$data_column} is specified,
#'  \item the \code{matrix} of the \code{time_points} if \code{simulated = TRUE} 
#'        or \code{controls$data$data_column} is not specified,
#'  \item the \code{matrix} of the simulated \code{markov_chain} if
#'        \code{simulated = TRUE},
#'  \item the \code{matrix} of the simulated or empirical \code{data} used for 
#'        estimation,
#'  \item the \code{matrix} \code{time_series} of empirical data before the 
#'        transformation to log-returns if \code{simulated = FALSE},
#'  \item the \code{vector} of fine-scale chunk sizes \code{T_star} if
#'        \code{controls$hierarchy = TRUE},
#'  \item the input \code{controls},
#'  \item the \code{true_parameters}.
#' }

fHMM_data <- function(
    dates, time_points, markov_chain, data, time_series, T_star, controls, 
    true_parameters
) {
  structure(
    list(
      "dates" = dates,
      "time_points" = time_points,
      "markov_chain" = markov_chain,
      "data" = data,
      "time_series" = time_series,
      "T_star" = T_star,
      "controls" = controls,
      "true_parameters" = true_parameters
    ),
    class = "fHMM_data"
  )
}

#' Prepare data
#'
#' @description
#' This function simulates or reads financial data for the {fHMM} package.
#'
#' @param controls
#' An object of class \code{fHMM_controls}.
#' @param true_parameters
#' An object of class \code{fHMM_parameters}, used as simulation parameters.
#' By default, \code{true_parameters = NULL}, i.e., sampled true parameters.
#' @param seed
#' Set a seed for the data simulation.
#' No seed per default.
#'
#' @return
#' An object of class \code{\link{fHMM_data}}.
#'
#' @examples
#' controls <- set_controls()
#' prepare_data(controls)
#' 
#' @export

prepare_data <- function(controls, true_parameters = NULL, seed = NULL) {

  ### check inputs
  if (!inherits(controls,"fHMM_controls")) {
    stop("'controls' is not of class 'fHMM_controls'.", call. = FALSE)
  }

  ### process data
  if (controls[["simulated"]]) {
    if (is.null(true_parameters)) {
      true_parameters <- fHMM_parameters(controls, seed = seed)
    }
    if (!inherits(true_parameters,"fHMM_parameters")) {
      stop("'true_parameters' is not of class 'fHMM_parameters'.", 
           call. = FALSE)
    }
    data <- simulate_data(
      controls = controls, true_parameters = true_parameters, seed = seed
    )
  } else {
    data <- read_data(controls)
  }

  ### build and return object of class 'fHMM_data'
  fHMM_data(
    dates = data$dates,
    time_points = data$time_points,
    markov_chain = data$markov_chain,
    data = data$data,
    time_series = data$time_series,
    T_star = data$T_star,
    controls = controls,
    true_parameters = true_parameters
  )
}

#' @rdname fHMM_data
#' @param x
#' An object of class \code{fHMM_data}.
#' @param ...
#' Currently not used.
#' @exportS3Method 

print.fHMM_data <- function(x, ...) {
  cat("fHMM", ifelse(x$controls$simulated, "simulated", "empirical"), "data\n")
  return(invisible(x))
}

#' @rdname fHMM_data
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
#'
#' @importFrom utils read.csv head

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

#' Simulate data
#'
#' @description
#' This helper function simulates data from a hidden Markov model.
#'
#' @inheritParams prepare_data
#'
#' @return
#' A \code{list} containing the following elements:
#' \itemize{
#'  \item the \code{matrix} of \code{time_points},
#'  \item the \code{matrix} of the simulated \code{markov_chain},
#'  \item the \code{matrix} of the simulated \code{data},
#'  \item the \code{numeric} vector of fine-scale chunk sizes \code{T_star} if
#'        \code{controls$hierarchy = TRUE}.
#' }
#'
#' @keywords internal
#'
#' @importFrom utils head

simulate_data <- function(controls, true_parameters, seed = NULL) {
  
  ### check inputs
  if (!inherits(controls, "fHMM_controls")) {
    stop("'controls' is not of class 'fHMM_controls'.", call. = FALSE)
  }
  if (!inherits(true_parameters, "fHMM_parameters")) {
    stop("'true_parameters' is not of class 'fHMM_parameters'.", call. = FALSE)
  }
  if (!controls$simulated) {
    stop("'controls$simulated' is not 'TRUE'.", call. = FALSE)
  }
  
  ### simulate data
  if (!controls[["hierarchy"]]) {
    markov_chain <- simulate_markov_chain(
      Gamma = true_parameters$Gamma,
      T = controls[["horizon"]][1],
      seed = seed
    )
    data <- simulate_observations(
      markov_chain = markov_chain,
      sdd = controls[["sdds"]][[1]]$name,
      mus = true_parameters$mus,
      sigmas = true_parameters$sigmas,
      dfs = true_parameters$dfs,
      seed = seed
    )
    time_points <- 1:controls[["horizon"]][1]
  } else {
    T_star <- compute_T_star(
      horizon = controls[["horizon"]],
      period = controls[["period"]]
    )
    markov_chain <- matrix(NA_real_,
                           nrow = controls[["horizon"]][1],
                           ncol = max(T_star) + 1
    )
    data <- matrix(NA_real_,
                   nrow = controls[["horizon"]][1],
                   ncol = max(T_star) + 1
    )
    time_points <- matrix(NA_real_,
                          nrow = controls[["horizon"]][1],
                          ncol = max(T_star) + 1
    )
    markov_chain[, 1] <- simulate_markov_chain(
      Gamma = true_parameters$Gamma,
      T = controls[["horizon"]][1],
      seed = seed
    )
    data[, 1] <- simulate_observations(
      markov_chain = markov_chain[, 1],
      sdd = controls[["sdds"]][[1]]$name,
      mus = true_parameters$mus,
      sigmas = true_parameters$sigmas,
      dfs = true_parameters$dfs,
      seed = seed
    )
    time_points[, 1] <- utils::head(c(1, cumsum(T_star) + 1), -1)
    for (t in 1:controls[["horizon"]][1]) {
      S_t <- markov_chain[t, 1]
      markov_chain[t, -1] <- simulate_markov_chain(
        Gamma = true_parameters$Gammas_star[[S_t]],
        T = T_star[t],
        seed = seed + t,
        total_length = max(T_star)
      )
      data[t, -1] <- simulate_observations(
        markov_chain = markov_chain[t, -1][!is.na(markov_chain[t, -1])],
        sdd = controls[["sdds"]][[2]]$name,
        mus = true_parameters$mus_star[[S_t]],
        sigmas = true_parameters$sigmas_star[[S_t]],
        dfs = true_parameters$dfs_star[[S_t]],
        seed = seed + t,
        total_length = max(T_star)
      )
      time_points[t, -1] <- c(
        time_points[t, 1] - 1 + (1:T_star[t]),
        rep(NA_integer_, max(T_star) - T_star[t])
      )
    }
  }
  
  ### return simulated data
  out <- list(
    "time_points"  = time_points,
    "markov_chain" = markov_chain,
    "data"         = data,
    "T_star"       = if (controls[["hierarchy"]]) T_star else NULL
  )
  return(out)
}

#' Simulate state-dependent observations
#'
#' @description
#' This function simulates state-dependent observations.
#'
#' @param markov_chain
#' A \code{numeric} vector of states of a Markov chain.
#' @param sdd
#' A \code{character}, the name of the state-dependent distribution, one of 
#' \code{"t"}, \code{"gamma"}, and \code{"lnorm"}.
#' @param mus
#' A \code{numeric} vector of expected values.
#' @param sigmas
#' A \code{numeric} vector of standard deviations.
#' @param dfs
#' A \code{numeric} vector of degrees of freedom.
#' Only relevant if \code{sdd = "t"}.
#' @param seed
#' Set a seed.
#' @param total_length
#' An \code{integer}, the total length of the output vector.
#' Must be greater or equal than \code{length(markov_chain)}.
#'
#' @return
#' A \code{numeric} vector of length \code{total_length}, where the first
#' \code{length(markov_chain)} elements are numeric values and the last
#' \code{total_length - length(markov_chain)} elements are \code{NA_real_}.
#'
#' @keywords
#' internal
#'
#' @importFrom stats rt rgamma

simulate_observations <- function(
    markov_chain, sdd, mus, sigmas, dfs = NULL, seed = NULL,
    total_length = length(markov_chain)
) {
  
  ### check inputs
  if (!all(is_number(markov_chain, int = TRUE, pos = TRUE))) {
    stop("'markov_chain' must be a numberic vector of Markov chain states.",
         call. = FALSE
    )
  }
  if (!(length(sdd) == 1 && sdd %in% c("t", "gamma", "lnorm"))) {
    stop("'sdd' must be one of 't' or 'gamma'.", call. = FALSE)
  }
  if (!all(is_number(mus))) {
    stop("'mus' must be a numberic vector.", call. = FALSE)
  }
  if (!all(is_number(sigmas, pos = TRUE))) {
    stop("'sigmas' must be a positive numeric vector.", call. = FALSE)
  }
  if (!(length(mus) == length(sigmas))) {
    stop("'mus' and 'sigmas' must be of the same length.", call. = FALSE)
  }
  if (sdd != "t") {
    if (!is.null(dfs)) {
      stop("'dfs' must only be specified if 'sdd' = 't'.", call. = FALSE)
    }
  } else {
    if (is.null(dfs)) {
      stop("'dfs' must be specified if 'sdd' = 't'.", call. = FALSE)
    }
    if (!(all(is_number(dfs, pos = TRUE)) && length(dfs) == length(mus))) {
      stop(paste(
        "'dfs' must be a positive number vector of length equal to",
        "'mus' and 'sigmas'."
      ), call. = FALSE)
    }
  }
  if (!is_number(total_length) || length(total_length) != 1 ||
      total_length < length(markov_chain)) {
    stop(paste(
      "'total_length' must be an integer greater or equal than",
      "'length(markov_chain)'."
    ), call. = FALSE)
  }
  
  ### set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  ### simulate observations
  T <- length(markov_chain)
  observations <- numeric(T)
  for (t in 1:T) {
    s <- markov_chain[t]
    if (sdd == "t") {
      observations[t] <- stats::rt(1, dfs[s]) * sigmas[s] + mus[s]
    }
    if (sdd == "gamma") {
      observations[t] <- stats::rgamma(1,
                                       shape = mus[s]^2 / sigmas[s]^2,
                                       scale = sigmas[s]^2 / mus[s]
      )
    }
    if (sdd == "lnorm") {
      observations[t] <- stats::rlnorm(1, meanlog = mus[s], sdlog = sigmas[s])
    }
  }
  
  ### append NA's
  observations <- c(observations, rep(NA_real_, total_length - T))
  
  ### return observations
  return(observations)
}


