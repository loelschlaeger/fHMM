#' Download financial data from Yahoo Finance
#'
#' @description
#' This function downloads stock data from <https://finance.yahoo.com/>.
#'
#' @details
#' The downloaded data has the following columns:
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
#' A \code{character}, the stock's symbol. It must match the identifier on
#' <https://finance.yahoo.com/>.
#' @param from
#' A \code{character}, a date in format \code{"YYYY-MM-DD"}, setting the lower 
#' data bound. Must not be earlier than \code{"1902-01-01"} (default).
#' @param to
#' A \code{character}, a date in format \code{"YYYY-MM-DD"}, setting the upper 
#' data bound. Default is the current date \code{Sys.date()}.
#' @param file
#' Either
#' * \code{NULL} to return the data as a \code{data.frame},
#' * or a \code{character}, the name of the file where the data is saved as a 
#'   \code{.csv}-file. 
#' By default, the data is saved in the current working directory with the name 
#' "\code{symbol}.csv".
#' @param verbose
#' Set to \code{TRUE} to return information about download success.
#'
#' @return
#' A \code{data.frame} if \code{file = NULL}.
#'
#' @examples
#' ### download 21st century DAX data
#' data <- download_data(symbol = "^GDAXI", from = "2000-01-03", file = NULL)
#' head(data)
#' 
#' @export
#'
#' @importFrom utils download.file read.csv head tail

download_data <- function(
    symbol, from = "1902-01-01", to = Sys.Date(), file = paste0(symbol, ".csv"), 
    verbose = TRUE
  ) {

  ### check input
  if (!is.character(symbol) || length(symbol) != 1) {
    stop("'symbol' must be a single character.", call. = FALSE)
  }
  from <- check_date(from)
  to <- check_date(to)
  if (is.null(file)) {
    save_file <- FALSE
  } else {
    save_file <- TRUE
    if (!is.character(file) || length(file) != 1 || nchar(file) == 0) {
      stop("'file' is invalid.", call. = FALSE)
    }
  }
  if (length(verbose) != 1 || (!isTRUE(verbose) && !isFALSE(verbose))) {
    stop("'verbose' must be either TRUE or FALSE.", call. = FALSE)
  }

  ### check 'from' and 'to'
  from <- as.Date(from)
  to <- as.Date(to)
  min_date <- as.Date("1902-01-01")
  if (from < min_date) {
    warning("'from' is set to lower bound of '1902-01-01'.", call. = FALSE)
    from <- min_date
  }
  if (to < from) {
    stop("'to' must not be earlier than 'from'.", call. = FALSE)
  }

  ### function to create finance.yahoo.com-URL
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
    url <- paste("https://query1.finance.yahoo.com/v7/finance/download/",
      symbol, "?period1=", t1, "&period2=", t2,
      "&interval=1d&events=history",
      sep = ""
    )
    return(url)
  }

  ### try to download data
  data_url <- create_url(symbol, from, to)
  destfile <- ifelse(save_file, file, tempfile())
  download_try <- suppressWarnings(
    try(utils::download.file(data_url, destfile = destfile, quiet = TRUE),
      silent = TRUE
    )
  )

  ### check 'download_try'
  if (inherits(download_try, "try-error")) {
    stop(
      "Download failed.\n",
      "Either 'symbol' is unknown or there is no data for the specified time interval.", 
      call. = FALSE
    )
  } 
  data <- utils::read.csv(file = destfile, header = TRUE, sep = ",", na.strings = "null")
  if (save_file) {
    if (verbose) {
      ### print summary of new data
      message(
        "Download successful.\n",
        "* symbol: ", symbol, "\n",
        "* from: ", utils::head(data$Date, n = 1), "\n",
        "* to: ", utils::tail(data$Date, n = 1), "\n",
        "* path: ", normalizePath(destfile)
      )
    }
  } else {
    return(data)
  }
}
