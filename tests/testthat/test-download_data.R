test_that("data download works", {
  skip_if_offline()
  symbol <- "^GDAXI"
  from <- "2000-01-03"
  to <- "2021-01-01"
  file <- paste0(tempfile(), ".csv")
  download_data(symbol = symbol, from = from, to = to, file = file, verbose = FALSE)
  expect_snapshot(read.csv(file = file, header = TRUE, sep = ",", na.strings = "null"))
  wrong_from <- "1901-01-01"
  expect_warning(download_data(symbol = symbol, from = wrong_from, to = to, file = file, verbose = FALSE))
  expect_error(download_data(symbol = symbol, from = from, to = as.Date(from) - 1, file = file, verbose = FALSE))
  expect_error(download_data(symbol = "wrongsymbol", from = from, to = to, file = file))
})
