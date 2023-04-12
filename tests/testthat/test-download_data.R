test_that("input checks for data download work", {
  expect_error(
    download_data(symbol = c("two", "symbols")),
    "'symbol' must be a single character."
  )
  expect_error(
    download_data(symbol = "^GDAXI", file = c("two", "filess")),
    "'file' is invalid."
  )
  expect_error(
    download_data(symbol = "^GDAXI", columns = 1),
    "'columns' must be a character vector"
  )
  expect_warning(
    download_data(symbol = "^GDAXI", columns = "not_a_column_name"),
    "'columns' is misspecified, no columns selected."
  )
  expect_error(
    download_data(
      symbol = "^GDAXI", 
      file = paste0(tempfile(), ".csv"), 
      verbose = "not_TRUE_or_FALSE"
    ),
    "'verbose' must be either TRUE or FALSE."
  )
  expect_error(
    download_data(
      symbol = "^GDAXI", 
      fill_dates = "not_TRUE_or_FALSE"
    ),
    "'fill_dates' must be either TRUE or FALSE."
  )
  expect_message(
    download_data(
      symbol = "^GDAXI", file = paste0(tempfile(), ".csv"), verbose = TRUE
    )
  )
})

test_that("data download returns expected data", {
  skip_if_offline()
  symbol <- "^GDAXI"
  from <- "2000-01-01"
  to <- "2000-01-10"
  data_direct <- download_data(
    symbol = symbol, from = from, to = to, verbose = FALSE
  )
  expect_true(is.data.frame(data_direct))
  file <- paste0(tempfile(), ".csv")
  download_data(
    symbol = symbol, from = from, to = to, file = file, verbose = FALSE
  )
  data_file <- read.csv(
    file = file, header = TRUE, sep = ",", na.strings = "null"
  )
  expect_identical(data_file, data_direct)
  data_small <- download_data(
    symbol = symbol, from = from, to = to, columns = "Close", verbose = FALSE,
    fill_dates = TRUE
  )
  expect_equal(
    data_small,
    structure(
      list(
        Close = c(NA, NA, 6750.759766, 6586.950195, 6502.069824, 6474.919922, 
                  6780.959961, NA, NA, 6925.52002)
      ), 
      class = "data.frame", 
      row.names = c(NA, -10L)
    )
  )
  wrong_from <- "1901-01-01"
  expect_warning(
    download_data(
      symbol = symbol, from = wrong_from, to = to, file = file, verbose = FALSE
    ),
    "'from' is set to lower bound of '1902-01-01'."
  )
  expect_error(
    download_data(
      symbol = symbol, from = from, to = as.Date(from) - 1, file = file, 
      verbose = FALSE
    ),
    "'to' must not be earlier than 'from'."
  )
  expect_error(
    download_data(symbol = "wrongsymbol", from = from, to = to, file = file),
    "Download failed."
  )
})
