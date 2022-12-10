test_that("checks of controls for simulated HMM work", {
  controls <- list(
    states  = 2,
    sdds    = "t",
    horizon = 400,
    fit     = list("runs" = 50)
  )
  controls <- set_controls(controls)
  expect_s3_class(controls, "fHMM_controls")
})

test_that("checks of controls for empirical HMM work", {
  skip_if_offline()
  file <- paste0(tempdir(), "/dax.csv")
  sink(tempfile())
  download_data(symbol = "^GDAXI", file = file, verbose = FALSE)
  sink()
  controls <- list(
    states = 2,
    sdds = "t",
    horizon = 400,
    data = list(
      file = file,
      date_column = "Date",
      data_column = "Close"
    )
  )
  expect_snapshot(set_controls(controls))
})

test_that("checks of controls for simulated HHMM work", {
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA)
  )
  expect_snapshot(set_controls(controls))
  expect_snapshot(unlist(set_controls(controls)))
  controls <- list(
    hierarchy = TRUE,
    period = "w"
  )
  expect_snapshot(set_controls(controls))
  expect_snapshot(unlist(set_controls(controls)))
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w"
  )
  expect_snapshot(set_controls(controls))
  expect_snapshot(unlist(set_controls(controls)))
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, 30),
    period = "w"
  )
  expect_snapshot(set_controls(controls))
  expect_snapshot(unlist(set_controls(controls)))
})

test_that("checks of controls for empirical HHMM work", {
  skip_if_offline()
  file <- paste0(tempdir(), "/dax.csv")
  sink(tempfile())
  download_data(symbol = "^GDAXI", file = file, verbose = FALSE)
  sink()
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c(file, file),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close")
    )
  )
  expect_snapshot(set_controls(controls))
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c(file, file),
      data_column = c("Close", "Close")
    )
  )
  expect_snapshot(set_controls(controls))
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c(file, file),
      date_column = c("Date", NA),
      data_column = c("Close", "Close")
    )
  )
  expect_error(set_controls(controls))
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c(file, file),
      date_column = c("Date", "wrong_name"),
      data_column = c("Close", "wrong_name")
    )
  )
  expect_error(set_controls(controls))
})
