test_that("input checks for download_data() work", {
  expect_error(
    download_data(symbol = c("two", "symbols")),
    "'symbol' must be a single character"
  )
  expect_error(
    download_data(symbol = "^GDAXI", from = "yesterday"),
    "Date is not in required format"
  )
  expect_error(
    download_data(symbol = "^GDAXI", to = "today"),
    "Date is not in required format"
  )
  expect_error(
    download_data(symbol = "^GDAXI", from = "2000-01-02", to = "2000-01-01"),
    "'to' must not be earlier than 'from'."
  )
  expect_warning(
    download_data(symbol = "^GDAXI", from = "1901-01-01"),
    "'from' is set to lower bound of '1902-01-01'."
  )
  expect_error(
    download_data(symbol = "^GDAXI", columns = 1),
    "'columns' must be a character vector"
  )
  expect_error(
    download_data(symbol = "^GDAXI", columns = "not_valid"),
    "should be one of"
  )
  expect_error(
    download_data(symbol = "^GDAXI", fill_dates = "not_TRUE_or_FALSE"),
    "'fill_dates' must be TRUE or FALSE"
  )
  expect_error(
    download_data(symbol = "wrong_symbol"),
    "Download failed."
  )
})

test_that("data download returns expected data", {
  skip_if_offline()
  data <- download_data(
    symbol = "^GDAXI", from = "2000-01-01", to = "2000-01-10",
    columns = c("Date", "Close"), fill_dates = TRUE
  )
  expect_true(is.data.frame(data))
  expect_equal(
    data,
    structure(
      list(
        Date = c("2000-01-01", "2000-01-02", "2000-01-03", "2000-01-04", 
                 "2000-01-05", "2000-01-06", "2000-01-07", "2000-01-08", 
                 "2000-01-09", "2000-01-10"), 
        Close = c(NA, NA, 6750.759766, 6586.950195, 6502.069824, 6474.919922, 
                  6780.959961, NA, NA, 6925.52002)
      ), 
      class = "data.frame", 
      row.names = c(NA, -10L)
    )
  )
})
