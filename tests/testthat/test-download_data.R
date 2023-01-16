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
    download_data(symbol = "^GDAXI", file = paste0(tempfile(), ".csv"), verbose = "not_TRUE_or_FALSE"),
    "'verbose' must be either TRUE or FALSE."
  )
  expect_message(
    download_data(symbol = "^GDAXI", file = paste0(tempfile(), ".csv"), verbose = TRUE)
  )
})

test_that("data download works", {
  skip_if_offline()
  symbol <- "^GDAXI"
  from <- "2000-01-03"
  to <- "2000-01-10"
  data2 <- download_data(symbol = symbol, from = from, to = to, file = NULL, verbose = FALSE)
  expect_true(is.data.frame(data2))
  file <- paste0(tempfile(), ".csv")
  download_data(symbol = symbol, from = from, to = to, file = file, verbose = FALSE)
  data <- read.csv(file = file, header = TRUE, sep = ",", na.strings = "null")
  expect_identical(data, data2)
  expect_equal(
    data,
    structure(
      list(Date = c("2000-01-03", "2000-01-04", "2000-01-05", 
                    "2000-01-06", "2000-01-07", "2000-01-10"), 
           Open = c(6961.720215, 6747.240234, 6585.850098, 6501.450195, 
                    6489.939941, 6785.470215), 
           High = c(7159.330078, 6755.359863, 6585.850098, 6539.310059, 
                    6791.529785, 6975.259766), 
           Low = c(6720.870117, 6510.459961, 6388.910156, 6402.629883, 
                   6470.140137, 6785.470215), 
           Close = c(6750.759766, 6586.950195, 6502.069824, 6474.919922, 
                     6780.959961, 6925.52002), 
           Adj.Close = c(6750.759766, 6586.950195, 6502.069824, 6474.919922, 
                         6780.959961, 6925.52002), 
           Volume = c(43072500L, 46678400L, 52682800L, 41180600L, 
                      56058900L, 42006200L)), 
      class = "data.frame", 
      row.names = c(NA, -6L)
    )
  )
  wrong_from <- "1901-01-01"
  expect_warning(
    download_data(symbol = symbol, from = wrong_from, to = to, file = file, verbose = FALSE),
    "'from' is set to lower bound of '1902-01-01'."
  )
  expect_error(
    download_data(symbol = symbol, from = from, to = as.Date(from) - 1, file = file, verbose = FALSE),
    "'to' must not be earlier than 'from'."
  )
  expect_error(
    download_data(symbol = "wrongsymbol", from = from, to = to, file = file),
    "Download failed."
  )
})
