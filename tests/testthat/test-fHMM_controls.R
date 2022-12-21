test_that("empty controls work", {
  expect_s3_class(set_controls(), "fHMM_controls")
})

test_that("printing controls work", {
  sink(tempfile())
  expect_s3_class(print(set_controls()), "fHMM_controls")
  sink()
})

test_that("warning for redundant controls work", {
  expect_warning(
    set_controls(
      list(
        "not_a_valid_control" = 1
      )
    )
  )
  expect_warning(
    set_controls(
      list(
        "data" = list(
          "not_a_valid_control" = 1,
          "file" = system.file("extdata", "dax.csv", package = "fHMM")
        )
      )
    )
  )
  expect_warning(
    set_controls(
      list(
        "fit" = list(
          "not_a_valid_control" = 1
        )
      )
    )
  )
})

test_that("input checks for setting controls work", {
  expect_error(
    set_controls(
      list(
        "data" = list()
      )
    )
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = FALSE,
        "states" = 1
      )
    )
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = FALSE,
        "horizon" = 10.5
      )
    )
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = FALSE,
        "sdds" = 1
      )
    )
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = FALSE,
        "sdds" = "t",
        "data" = list(
          "file" = 1
        )
      )
    )
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = FALSE,
        "sdds" = "t",
        "data" = list(
          "data_column" = 1
        )
      )
    )
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = FALSE,
        "sdds" = "t",
        "data" = list(
          "logreturns" = "not_a_boolean"
        )
      )
    )
  )
  
  expect_error(
    set_controls(
      list(
        "hierarchy" = TRUE,
        "data" = list()
      )
    )
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = "not_a_boolean"
      )
    )
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = TRUE,
        "states" = 1:3
      )
    )
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = TRUE,
        "horizon" = 1:3
      )
    )
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = TRUE,
        "horizon" = c(10, NA),
        "period" = "wrong_symbol"
      )
    )
  )
})

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
  controls <- list(
    states = 2,
    sdds = "t",
    horizon = 400,
    data = list(
      file = system.file("extdata", "dax.csv", package = "fHMM"),
      date_column = "Date",
      data_column = "Close"
    )
  )
  expect_s3_class(set_controls(controls), "fHMM_controls")
})

test_that("checks of controls for simulated HHMM work", {
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA)
  )
  expect_s3_class(set_controls(controls), "fHMM_controls")
  controls <- list(
    hierarchy = TRUE,
    period = "w"
  )
  expect_s3_class(set_controls(controls), "fHMM_controls")
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w"
  )
  expect_s3_class(set_controls(controls), "fHMM_controls")
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, 30),
    period = "w"
  )
  expect_s3_class(set_controls(controls), "fHMM_controls")
})

test_that("checks of controls for empirical HHMM work", {
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
  expect_s3_class(set_controls(controls), "fHMM_controls")
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c(system.file("extdata", "dax.csv", package = "fHMM"), 
               system.file("extdata", "dax.csv", package = "fHMM")),
      data_column = c("Close", "Close")
    )
  )
  expect_s3_class(set_controls(controls), "fHMM_controls")
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c(system.file("extdata", "dax.csv", package = "fHMM"), 
               system.file("extdata", "dax.csv", package = "fHMM")),
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
      file = c(system.file("extdata", "dax.csv", package = "fHMM"), 
               system.file("extdata", "dax.csv", package = "fHMM")),
      date_column = c("Date", "wrong_name"),
      data_column = c("Close", "wrong_name")
    )
  )
  expect_error(set_controls(controls))
})
