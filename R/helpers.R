#' Check date format
#'
#' @description
#' This function checks if the input \code{date} has the format
#' \code{"YYYY-MM-DD"}.
#'
#' @param date \[`character(1)`\]\cr
#' The date in format \code{"YYYY-MM-DD"}.
#'
#' @return
#' \code{as.Date(date)} if \code{date} has the format \code{"YYYY-MM-DD"}.
#' Otherwise, the function throws an error.
#'
#' @keywords internal

check_date <- function(date) {
  date <- try(as.Date(date, format = "%Y-%m-%d"), silent = TRUE)
  if (inherits(date, "try-error") || anyNA(date)) {
    stop("Date is not in required format 'YYYY-MM-DD'.", call. = FALSE)
  }
  return(date)
}

#' Find closest year
#'
#' @description
#' This function takes a \code{date} as input and returns the closest year.
#'
#' @param date \[`character(1)`\]\cr
#' The date in format \code{"YYYY-MM-DD"}.
#'
#' @return
#' An \code{integer}, the closest year to the input date.
#'
#' @keywords internal

find_closest_year <- function(date) {
  year <- as.numeric(format(date, "%Y"))
  ifelse(
    date <= as.Date(paste0(year, "-06-30")),
    year,
    year + 1
  )
}

#' List to vector
#' 
#' @description
#' This function produces a \code{vector} from a \code{list} structure and 
#' replaces \code{NULL} elements by \code{NA}.
#' 
#' @param x \[`list()`\]\cr
#' A \code{list}.
#' 
#' @return 
#' A \code{numeric}.
#' 
#' @keywords internal

list_to_vector <- function(x) {
  stopifnot(is.list(x))
  unlist(lapply(x, function(m) ifelse(is.null(m), NA, m)))
}