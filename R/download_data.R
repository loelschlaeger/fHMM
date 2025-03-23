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
  ### check inputs
  if (!curl::has_internet()) {
    stop("This function requires an internet connection.", call. = FALSE)
  }
  if (missing(symbol) || !is.character(symbol) || length(symbol) != 1) {
    stop("'symbol' must be a single character.", call. = FALSE)
  }
  if (!isTRUE(fill_dates) && !isFALSE(fill_dates)) {
    stop("'fill_dates' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(columns)) {
    stop("'columns' must be a character vector.", call. = FALSE)
  }
  columns <- match.arg(columns, several.ok = TRUE)
  
  ### check range
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
  if (to > Sys.Date()) {
    stop("'to' cannot be in the future.", call. = FALSE)
  }
  
  ### API request
  url <- paste0("https://query2.finance.yahoo.com/v8/finance/chart/", symbol)
  resp <- oeli::try_silent(
    httr::GET(
      url = url, 
      query = list(
        period1 = as.numeric(as.POSIXct(from, tz = "UTC")), 
        period2 = as.numeric(as.POSIXct(to + 1, tz = "UTC")), 
        interval = "1d"
      )
    )
  )
  if (inherits(resp, "fail") || httr::http_error(resp)) {
    message(
      "Yahoo Finance API request failed. This can have different reasons:\n",
      "- Maybe 'symbol' is unknown.\n",
      "- Or there is no data for the specified time interval.", 
      call. = FALSE
    )
    return(data.frame())
  }
  parsed <- jsonlite::fromJSON(
    httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE
  )
  
  ### shape data
  data <- parsed[["chart"]][["result"]][[1]]
  date <- format(as.POSIXct(unlist(data$timestamp)), format = "%Y-%m-%d")
  if (length(date) == 0) {
    data <- data.frame(
      Date = seq(from, to, by = 1),
      Open = NA,
      High = NA,
      Low = NA,
      Close = NA,
      Adj.Close = NA,
      Volume = NA
    )
  } else {
    indicators <- data$indicators$quote[[1]]
    adjclose <- data$indicators$adjclose[[1]][["adjclose"]]
    data <- data.frame(
      Date = date,
      Open = list_to_vector(indicators$open),
      High = list_to_vector(indicators$high),
      Low  = list_to_vector(indicators$low),
      Close = list_to_vector(indicators$close),
      Adj.Close = list_to_vector(adjclose),
      Volume = list_to_vector(indicators$volume)
    ) 
  }
  
  ### fill dates (if requested)
  if (fill_dates) {
    data$Date <- as.Date(data$Date)
    data <- suppressWarnings(
      padr::pad(data, interval = "day", start_val = from, end_val = to)
    )
    data$Date <- as.character(data$Date)
  }
  
  ### select columns
  data[, columns, drop = FALSE]
}
