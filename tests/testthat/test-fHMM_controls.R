test_that("empty controls work", {
  expect_s3_class(
    set_controls(), 
    "fHMM_controls"
  )
  expect_error(
    set_controls(
      controls = "not_a_list"
    ),
    "Input 'controls' must be a list."
  )
})

test_that("warning for redundant controls work", {
  expect_warning(
    set_controls(
      controls = list(
        "not_a_valid_control" = 1
      )
    ),
    "not_a_valid_control in 'controls' ignored."
  )
  expect_warning(
    set_controls(
      controls = list(
        "data" = list(
          "not_a_valid_control" = 1
        )
      )
    ),
    "not_a_valid_control"
  )
  expect_warning(
    set_controls(
      list(
        "fit" = list(
          "not_a_valid_control" = 1
        )
      )
    ),
    "not_a_valid_control"
  )
})

test_that("input checks for setting controls work", {
  expect_error(
    set_controls(
      list(
        "hierarchy" = FALSE,
        "states" = 1
      )
    ),
    "'states' must be an integer greater or equal 2."
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = FALSE,
        "horizon" = 10.5
      )
    ),
    "The control 'horizon' must be an integer."
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = FALSE,
        "sdds" = 1
      )
    ),
    "The control 'sdds' must be a character of length 1."
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
    ),
    "The control 'file' in 'data' must be a character."
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = "not_a_boolean"
      )
    ),
    "The control 'hierarchy' must be a TRUE or FALSE."
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = TRUE,
        "states" = 1:3
      )
    ),
    "The control 'states' must be a vector of length 2"
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = TRUE,
        "horizon" = 1:3
      )
    ),
    "The control 'horizon' must be an integer vector of length 2."
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = TRUE,
        "horizon" = c(10, NA),
        "period" = "wrong_symbol"
      )
    ),
    "The control 'period' must be eiter 'NA' or one of 'w', 'm', 'q', 'y'."
  )
})

test_that("checks for `fit` controls work", {
  controls <- list(
    fit = list("runs" = "1")
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'runs' in 'fit' must be an integer."
  )
  controls <- list(
    fit = list("origin" = "not_a_boolean")
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'origin' in 'fit' must be a boolean."
  )
  controls <- list(
    fit = list("origin" = TRUE)
  )
  controls <- set_controls(controls)
  expect_equal(controls$fit$runs, 1)
  expect_equal(controls$fit$accept, 1:5)
  controls <- list(
    fit = list("accept" = "all")
  )
  controls <- set_controls(controls)
  expect_equal(controls$fit$accept, 1:5)
  controls <- list(
    fit = list("accept" = "bad_name")
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'accept' in 'fit' must be vector of integers from 1 to 5."
  )
  controls <- list(
    fit = list("gradtol" = -1)
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'gradtol' in 'fit' must be positive numeric value."
  )
  controls <- list(
    fit = list("iterlim" = -1)
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'iterlim' in 'fit' must be a positive integer."
  )
  controls <- list(
    fit = list("print.level" = 3)
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'print.level' in 'fit' must be one of 0, 1, and 2."
  )
  controls <- list(
    fit = list("steptol" = -1)
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'steptol' in 'fit' must be positive numeric value."
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
  expect_snapshot(controls)
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
  controls <- set_controls(controls)
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
  controls <- list(
    states = 2,
    sdds = "t",
    horizon = 400,
    data = list(
      file = dax,
      date_column = "Date",
      data_column = "Close"
    )
  )
  controls <- set_controls(controls)
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
  controls <- list(
    states = 2,
    sdds = "t",
    horizon = 400,
    data = list(
      file = dax,
      date_column = 1,
      data_column = "Close"
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'date_column' in 'data' must be a character or NA."
  )
  controls <- list(
    states = 2,
    sdds = "t",
    horizon = 400,
    data = list(
      file = dax,
      date_column = "Date",
      data_column = 1
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'data_column' in 'data' must be a character or NA."
  )
  controls <- list(
    states = 2,
    sdds = "t",
    horizon = 400,
    data = list(
      file = dax,
      date_column = "Date",
      data_column = "Close",
      logreturns = "not_a_boolean"
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'logreturns' in 'data' must be a boolean."
  )
  controls <- list(
    states = 2,
    sdds = "t",
    horizon = 400,
    data = list(
      file = dax,
      date_column = NA,
      data_column = "Close"
    )
  )
  controls <- set_controls(controls)
  expect_s3_class(controls, "fHMM_controls")
  controls <- list(
    states = 2,
    sdds = "t",
    horizon = 400,
    data = list(
      file = dax,
      date_column = "Date",
      data_column = "Close",
      from = "2020-01-01",
      to = "2020-02-02"
    )
  )
  controls <- set_controls(controls)
  expect_s3_class(controls, "fHMM_controls")
})

test_that("checks of controls for simulated HHMM work", {
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA)
  )
  controls <- set_controls(controls)
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
  controls <- list(
    hierarchy = TRUE,
    period = "w"
  )
  controls <- set_controls(controls)
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "m"
  )
  controls <- set_controls(controls)
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, 30),
    period = "y"
  )
  controls <- set_controls(controls)
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, 30),
    data = list("data_column" = "Date")
  )
  controls <- set_controls(controls)
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
})

test_that("checks of controls for empirical HHMM work", {
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = dax,
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close")
    )
  )
  controls <- set_controls(controls)
  expect_true(controls$data$data_inside)
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
  controls <- list(
    hierarchy = TRUE,
    horizon = c(NA, NA, NA),
    period = "w",
    data = list(
      file = dax,
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'horizon' must be a vector of length 2."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = list(dax, dax),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close")
    )
  )
  controls <- set_controls(controls)
  expect_true(controls$data$data_inside)
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = list(dax, dax, dax),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'file' in 'data' must be a list of length two."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = list(dax, matrix(1))
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'file' in 'data' must be a list of two data.frame."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c("one", "two", "three")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'file' in 'data' must be a character vector of length two."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = list(dax, dax),
      date_column = c("one", "two", "three"),
      data_column = c("Close", "Close")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'date_column' in 'data' must be a vector with two characters or two NA's."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = list(dax, dax),
      date_column = c("Date", "Date"),
      data_column = c("one", "two", "three")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'data_column' in 'data' must be a character vector of length two."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = list(dax, dax),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close"),
      logreturns = TRUE
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'logreturns' in 'data' must be a boolean vector of length two."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = list(dax, dax),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close"),
      merge = "not_a_function"
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'merge' in 'data' must be of class 'function'."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = list(dax, dax),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close"),
      merge = function(x) 1:2
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The controls 'merge' in 'data' must merge a numeric vector into a single numeric value."
  )
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
  expect_false(controls$data$data_inside)
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
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
  controls <- set_controls(controls)
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
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
  expect_s3_class(controls, "fHMM_controls")
  expect_snapshot(controls)
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = dax,
      date_column = c("bad_name", "Date"),
      data_column = c("Close", "Close")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "Date column 'bad_name' not found in supplied data.frame."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = dax,
      date_column = c("Date", "Date"),
      data_column = c("Close", "bad_name")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "Data column 'bad_name' not found in supplied data.frame."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c(system.file("extdata", "dax.csv", package = "fHMM"), 
               system.file("extdata", "dax.csv", package = "fHMM")),
      date_column = c("bad_name", "Date"),
      data_column = c("Close", "Close")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "Date column 'bad_name' not found"
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c(system.file("extdata", "dax.csv", package = "fHMM"), 
               system.file("extdata", "dax.csv", package = "fHMM")),
      date_column = c("Date", "Date"),
      data_column = c("Close", "bad_name")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "Data column 'bad_name' not found"
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c("bad_path",
               system.file("extdata", "dax.csv", package = "fHMM")),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "not found"
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c(".",
               system.file("extdata", "dax.csv", package = "fHMM")),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "Unable to read"
  )
})
