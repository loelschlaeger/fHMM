#' Check the correct date format "YYYY-MM-DD".
#' @description 
#' This function checks if the input \code{date} has the format "YYYY-MM-DD".
#' @param date
#' A character, specifying a date in format "YYYY-MM-DD".
#' @return
#' \code{as.date(date)} if \code{date} has the correct format. Otherwise, the
#' function throws an error.

check_date = function(date) {
  date = try(as.Date(date), silent = TRUE)
  if(class(date) == "try-error" || is.na(as.Date(date, format="%Y/%m/%d")))
    stop("C7")
  return(date)
}
