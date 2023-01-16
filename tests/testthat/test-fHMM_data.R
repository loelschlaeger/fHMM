test_that("input checks work", {
  expect_error(
    prepare_data("controls"),
    "'controls' is not of class 'fHMM_controls'."
  )
  expect_error(
    prepare_data(set_controls(), true_parameters = "true"),
    "'true_parameters' is not of class 'fHMM_parameters'."
  )
})

test_that("data preparation for simulated HMM works", {
  controls <- set_controls()
  out <- prepare_data(controls, seed = 1)
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})

test_that("data preparation for empirical HMM works", {
  controls <- set_controls(list(data = list(file = dax)))
  out <- prepare_data(controls, seed = 1)
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})

test_that("data preparation for empirical HHMM works", {
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c(system.file("extdata", "dax.csv", package = "fHMM"),
               system.file("extdata", "dax.csv", package = "fHMM")),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close")
    )
  )
  controls <- set_controls(controls)
  out <- prepare_data(controls, seed = 1)
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, 10),
    data = list(
      file = c(system.file("extdata", "dax.csv", package = "fHMM"),
               system.file("extdata", "dax.csv", package = "fHMM")),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close")
    )
  )
  controls <- set_controls(controls)
  out <- prepare_data(controls, seed = 1)
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})
