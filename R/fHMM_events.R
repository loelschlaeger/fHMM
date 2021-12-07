#' Check events.
#' @description
#' This function checks the input \code{events}.
#' @param events
#' A list of two elements.
#' The first element is named \code{"dates"} and contains characters in format
#' "YYYY-MM-DD".
#' The second element is named \code{"labels"} and is a character vector of the
#' same length as \code{"dates"}.
#' @return
#' An object of class \code{fHMM_data}.
#' @export
#' @keywords
#' s3

fHMM_events <- function(events) {
  if (class(events) == "fHMM_events") {
    warning()
  } else {
    if (class(events) != "list") {
      stop()
    }
    if (length(events) != 2) {
      stop()
    }
    if (!identical(names(events), c("dates", "labels"))) {
      stop()
    }
    events$dates <- check_date(events$dates)
    class(events) <- "fHMM_events"
  }
  return(events)
}

#' Print method for \code{fHMM_events}.
#' @description
#' This function is the print method for an object of class \code{fHMM_events}.
#' @param x
#' An object of class \code{fHMM_events}.
#' @param ...
#' Ignored.
#' @return
#' Returns \code{x} invisibly.
#' @export

print.fHMM_events <- function(x, ...) {
  print(data.frame("dates" = x$dates, "labels" = x$labels))
  return(invisible(x))
}
