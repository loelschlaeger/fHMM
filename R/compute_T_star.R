#' Compute lengths of fine-scale chunks
#'
#' @description
#' This helper function computes lengths of fine-scale chunks in the 
#' hierarchical case.
#'
#' @param horizon
#' The element \code{controls$horizon}, i.e., an integer vector of length 2,
#' where the second entry can be \code{NA_integer_}.
#' @param period
#' The element \code{controls$period}, i.e. one of \code{"w"},
#' \code{"m"}, \code{"q"}, or \code{"y"}.
#' @param dates
#' A \code{character} vector of dates of empirical fine-scale data (if any).
#' By default, \code{dates = NULL}.
#' @param seed
#' Set a seed for the simulation of flexible chunk lengths.
#' By default, \code{seed = NULL} (i.e., no seed).
#'
#' @return
#' An \code{integer} vector of fine-scale chunk sizes.
#' 
#' @keywords internal

compute_T_star <- function(horizon, period, dates = NULL, seed = NULL) {
  if (is.null(dates)) {
    if (!is.null(seed)) {
      set.seed(seed)
    }
    if (!is.na(horizon[2])) {
      T_star <- rep(horizon[2], horizon[1])
    } else {
      if (period == "w") {
        size <- 5
      }
      if (period == "m") {
        size <- 25
      }
      if (period == "q") {
        size <- 70
      }
      if (period == "y") {
        size <- 260
      }
      T_star <- sample(1:size, horizon[1],
        replace = TRUE,
        prob = stats::dbinom(1:size, size, 0.9)
      )
    }
  } else {
    dates_overview <- data.frame(
      "w" = as.numeric(strftime(dates, format = "%W")),
      "m" = as.numeric(strftime(dates, format = "%m")),
      "q" = as.numeric(substr(quarters(dates), 2, 2)),
      "y" = as.numeric(strftime(dates, format = "%Y"))
    )
    if (!is.na(horizon[2])) {
      T_star <- rep(horizon[2], floor(length(dates) / horizon[2]))
    } else {
      if (period == "w") {
        T_star <- vector()
        for (y in unique(dates_overview[["y"]])) {
          dates_overview_subset <- dates_overview[dates_overview[["y"]] == y, ]
          T_star <- c(T_star, as.vector(table(dates_overview_subset[["w"]])))
        }
      }
      if (period == "m") {
        T_star <- vector()
        for (y in unique(dates_overview[["y"]])) {
          dates_overview_subset <- dates_overview[dates_overview[["y"]] == y, ]
          T_star <- c(T_star, as.vector(table(dates_overview_subset[["m"]])))
        }
      }
      if (period == "q") {
        T_star <- vector()
        for (y in unique(dates_overview[["y"]])) {
          dates_overview_subset <- dates_overview[dates_overview[["y"]] == y, ]
          T_star <- c(T_star, as.vector(table(dates_overview_subset[["q"]])))
        }
      }
      if (period == "y") {
        T_star <- as.vector(table(dates_overview[["y"]]))
      }
    }
  }
  as.integer(T_star)
}
