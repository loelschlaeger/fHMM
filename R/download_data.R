#' Downloading financial data
#'
#' @description
#' This function downloads stock data from <https://finance.yahoo.com/> and 
#' saves it as a .csv-file.
#'
#' @details
#' The downloaded data is a .csv-file with the following columns:
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
#' A character, the stock's symbol. It must match the identifier on
#' <https://finance.yahoo.com/>.
#' @param from
#' A date in format "YYYY-MM-DD", setting the lower data bound. Must not be
#' earlier than \code{"1902-01-01"}.
#' @param to
#' A date in format "YYYY-MM-DD", setting the upper data bound. Default is the
#' current date \code{Sys.date()}.
#' @param file
#' The name of the file where the .csv-file is saved. Per default, it is saved
#' in the current working directory with the name "\code{symbol}.csv".
#' @param verbose
#' If \code{TRUE} returns information about download success.
#'
#' @return
#' No return value.
#'
#' @examples
#' ### download 21st century DAX data
#' download_data(
#'   symbol = "^GDAXI", from = "2000-01-03",
#'   file = paste0(tempfile(), ".csv")
#' )
#' @export
#'
#' @importFrom utils download.file read.csv head tail

download_data <- function(symbol, from = "1902-01-01", to = Sys.Date(),
                          file = paste0(symbol, ".csv"), verbose = TRUE) {

  ### check input
  if (!is.character(symbol) || length(symbol) != 1) {
    stop("'symbol' must be a single character.")
  }
  from <- check_date(from)
  to <- check_date(to)
  if (!is.character(file) || length(file) != 1 || nchar(file) == 0) {
    stop("'file' is invalid.")
  }
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be either TRUE or FALSE.")
  }

  ### check 'from' and 'to'
  from <- as.Date(from)
  to <- as.Date(to)
  min_date <- as.Date("1902-01-01")
  if (from < min_date) {
    warning("'from' is set to lower bound of '1902-01-01'.")
    from <- min_date
  }
  if (to < from) {
    stop("'to' must not be earlier than 'from'.")
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
  download_try <- suppressWarnings(
    try(utils::download.file(data_url, destfile = file, quiet = TRUE),
      silent = TRUE
    )
  )

  ### check 'download_try'
  if (inherits(download_try, "try-error")) {
    stop("'symbol' is unknown on https://finance.yahoo.com/, please check spelling.")
  } else if (verbose) {
    ### print summary of new data
    data <- utils::read.csv(file = file, header = TRUE, sep = ",", na.strings = "null")
    cat("Download successful.\n")
    cat("* symbol:", symbol, "\n")
    cat("* from:", utils::head(data$Date, n = 1), "\n")
    cat("* to:", utils::tail(data$Date, n = 1), "\n")
    cat("* path:", normalizePath(file))
  }
}
