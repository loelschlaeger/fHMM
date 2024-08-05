#' Download financial data from Yahoo Finance
#'
#' @description
#' This function downloads financial data from <https://finance.yahoo.com/>
#' and returns it as a \code{data.frame}.
#'
#' @details
#' Yahoo Finance provides historical daily data for stocks or indices.
#' The following data columns are available:
#' \itemize{
#'   \item \code{Date}: The date.
#'   \item \code{Open}: Opening price.
#'   \item \code{High}: Highest price.
#'   \item \code{Low}: Lowest price.
#'   \item \code{Close}: Close price adjusted for splits.
#'   \item \code{Adj.Close}: Close price adjusted for dividends and splits.
#'   \item \code{Volume}: Trade volume.
#' }
#'
#' @param symbol
#' A \code{character}, the stock's symbol. 
#' 
#' It must match the identifier on <https://finance.yahoo.com/>.
#' @param from
#' A \code{character} in the format \code{"YYYY-MM-DD"}, setting the lower 
#' data bound. 
#' 
#' Must not be earlier than \code{"1902-01-01"} (default).
#' @param to
#' A \code{character} in the format \code{"YYYY-MM-DD"}, setting the upper 
#' data bound. 
#' 
#' Default is the current date \code{Sys.date()}.
#' @param fill_dates
#' Set to \code{TRUE} to fill missing dates (e.g., days at which the stock 
#' market is closed) with \code{NA}'s.
#' 
#' By default, \code{fill_dates = FALSE}.
#' @param columns
#' A \code{character} of requested data columns, see the details.
#' 
#' By default, all columns are returned.
#'
#' @return
#' A \code{data.frame}.
#'
#' @examples
#' ### 21st century DAX closing prices
#' data <- download_data(
#'   symbol = "^GDAXI", from = "2000-01-01", columns = c("Date", "Close"),
#'   fill_dates = TRUE
#' )
#' head(data)
#' 
#' @export

download_data <- function(
    symbol, from = "1902-01-01", to = Sys.Date(), fill_dates = FALSE,
    columns = c("Date", "Open", "High", "Low", "Close", "Adj.Close", "Volume")
) {
  if (missing(symbol) || !is.character(symbol) || length(symbol) != 1) {
    stop("'symbol' must be a single character.", call. = FALSE)
  }
  from <- check_date(from)
  min_date <- as.Date("1902-01-01")
  if (from < min_date) {
    warning("'from' is set to lower bound of '1902-01-01'.", call. = FALSE)
    from <- min_date
  }
  to <- check_date(to)
  if (to < from) {
    stop("'to' must not be earlier than 'from'.", call. = FALSE)
  }
  if (!isTRUE(fill_dates) && !isFALSE(fill_dates)) {
    stop("'fill_dates' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(columns)) {
    stop("'columns' must be a character vector.", call. = FALSE)
  }
  columns <- match.arg(columns, several.ok = TRUE)
  create_url <- function(symbol, from, to) {
    t1 <- as.integer(ISOdate(as.numeric(format(from, format = "%Y")),
                             as.numeric(format(from, format = "%m")),
                             as.numeric(format(from, format = "%d")),
                             hour = 0
    ))
    t2 <- as.integer(ISOdate(as.numeric(format(to, format = "%Y")),
                             as.numeric(format(to, format = "%m")),
                             as.numeric(format(to, format = "%d")),
                             hour = 24
    ))
    paste("https://query1.finance.yahoo.com/v7/finance/download/",
          symbol, "?period1=", t1, "&period2=", t2,
          "&interval=1d&events=history",
          sep = ""
    )
  }
  destfile <- tempfile()
  download_try <- suppressWarnings(
    try(utils::download.file(
      url = create_url(symbol, from, to), destfile = destfile, quiet = TRUE),
      silent = TRUE
    )
  )
  if (inherits(download_try, "try-error")) {
    stop(
      "Download failed. This can have different reasons:\n",
      "- Maybe 'symbol' is unknown.\n",
      "- Or there is no data for the specified time interval.", 
      call. = FALSE
    )
  } 
  data <- utils::read.csv(
    file = destfile, header = TRUE, sep = ",", na.strings = "null"
  )
  if (fill_dates) {
    data$Date <- as.Date(data$Date)
    data <- padr::pad(data, interval = "day", start_val = from, end_val = to)
    data$Date <- as.character(data$Date)
  }
  data[, columns, drop = FALSE]
}
