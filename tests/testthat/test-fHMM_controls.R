test_that("defining empty controls work", {
  expect_s3_class(
    set_controls(), 
    "fHMM_controls"
  )
  expect_error(
    set_controls(controls = "not_a_list"),
    "Input 'controls' must be a list."
  )
})

test_that("checks for redundant controls work", {
  expect_warning(
    set_controls(
      controls = list(
        "not_a_valid_control" = 1
      )
    ),
    "ignored"
  )
  expect_warning(
    set_controls(
      controls = list(
        "data" = list(
          "file" = data.frame("Date" = "2000-01-01", "Close" = 1),
          "not_a_valid_control" = 1
        )
      )
    ),
    "ignored"
  )
  expect_warning(
    set_controls(
      data = list(
        "file" = data.frame("Date" = "2000-01-01", "Close" = 1),
        "not_a_valid_control" = 1
      )
    ),
    "ignored"
  )
  expect_warning(
    set_controls(
      list(
        "fit" = list(
          "not_a_valid_control" = 1
        )
      )
    ),
    "ignored"
  )
  expect_warning(
    set_controls(
      fit = list(
        "not_a_valid_control" = 1
      )
    ),
    "ignored"
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
    "The control 'horizon' must be a positive integer."
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
      controls = list("data" = "not_a_list")
    ),
    "Element 'data' in input 'controls' must be a list."
  )
  expect_error(
    set_controls(
      data = "not_a_list"
    ),
    "Control 'data' must be a list or NA."
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
    "The control 'file' in 'data' must be a data.frame"
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = "not_a_boolean"
      )
    ),
    "The control 'hierarchy' must be TRUE or FALSE."
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = TRUE,
        "states" = 1:3
      )
    ),
    "The control 'states' must be a vector of length 1 or 2"
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = TRUE,
        "horizon" = 1:3
      )
    ),
    "The control 'horizon' must be a vector of length 2."
  )
  expect_error(
    set_controls(
      list(
        "hierarchy" = TRUE,
        "horizon" = c(10, NA),
        "period" = "wrong_symbol"
      )
    ),
    "The control 'period' must be one of 'w', 'm', 'q', 'y'."
  )
  expect_error(
    set_controls(
      data = list(logreturns = TRUE)
    ),
    "Please specify 'file'"
  )
})

test_that("missing data controls can be set from individual controls", {
  expect_s3_class(
    set_controls(
      file = data.frame("Close" = 1),
      date_column = NA
    ),
    "fHMM_controls"
  )
  expect_s3_class(
    set_controls(
      data = list(
        "file" = data.frame("Close" = 1, "Date" = "2020-01-01"),
        "date_column" = "Date",
        "data_column" = "Close",
        "from" = "2020-01-01",
        "to" = "2020-01-01",
        "logreturns" = TRUE,
        "merge" = function(x) median(x)
      )
    ),
    "fHMM_controls"
  )
  expect_s3_class(
    set_controls(
      controls = list(
        data = list()
      ),
      data = list(
        "file" = data.frame("Close" = 1, "Date" = "2020-01-01"),
        "date_column" = "Date",
        "data_column" = "Close",
        "from" = "2020-01-01",
        "to" = "2020-01-01",
        "logreturns" = TRUE,
        "merge" = function(x) median(x)
      )
    ),
    "fHMM_controls"
  )
  expect_s3_class(
    set_controls(
      file = data.frame("Close" = 1, "Date" = "2020-01-01")
    ),
    "fHMM_controls"
  )
})

test_that("missing fit controls can be set from individual controls", {
  expect_s3_class(
    set_controls(
      controls = list(
        fit = list()
      ),
      fit = list(
        "runs" = 7,
        "origin" = TRUE,
        "accept" = 1,
        "gradtol" = 1,
        "iterlim" = 123,
        "print.level" = 2,
        "steptol" = 0.5,
        "ncluster" = 2
      )
    ),
    "fHMM_controls"
  )
  expect_s3_class(
    set_controls(
      fit = list(
        "runs" = 7,
        "origin" = TRUE,
        "accept" = 1,
        "gradtol" = 1,
        "iterlim" = 123,
        "print.level" = 2,
        "steptol" = 0.5,
        "ncluster" = 2
      )
    ),
    "fHMM_controls"
  )
})

test_that("control validation works", {
  expect_error(
    validate_controls("controls"),
    "must be a list"
  )
  expect_error(
    validate_controls(
      controls = list("hierarchy" = "FALSE")
    ),
    "'hierarchy' must be TRUE or FALSE"
  )
  expect_error(
    validate_controls(
      controls = list(
        "hierarchy" = FALSE, "verbose" = "FALSE"
      )
    ),
    "'verbose' must be TRUE or FALSE"
  )
  expect_error(
    validate_controls(
      controls = list(
        "hierarchy" = FALSE, "verbose" = FALSE, seed = pi
      )
    ),
    "'seed' must be an integer"
  )
})

test_that("checks for 'fit' controls work", {
  expect_error(
    set_controls(
      controls = list("fit" = "not_a_list")
    ),
    "Element 'fit' in input 'controls' must be a list."
  )
  expect_error(
    set_controls(
      fit = "not_a_list"
    ),
    "Control 'fit' must be a list."
  )
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
    "The control 'accept' in 'fit' must be a vector of integers from 1 to 5."
  )
  controls <- list(
    fit = list("gradtol" = -1)
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'gradtol' in 'fit' must be a positive number."
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
    "The control 'steptol' in 'fit' must be a positive number."
  )
})

test_that("checks of controls for simulated HMM work", {
  controls <- list(
    states  = 2,
    sdds    = "t",
    horizon = 400,
    fit     = list("runs" = 50)
  )
  expect_s3_class(
    set_controls(controls), 
    "fHMM_controls"
  )
})

test_that("checks of controls for empirical HMM work", {
  skip_if_offline()
  dax <- download_yahoo("^GDAXI")
  controls <- list(
    states = 2,
    sdds = "t",
    data = list(
      file = dax,
      date_column = "Date",
      data_column = "Close"
    )
  )
  expect_s3_class(
    set_controls(controls), 
    "fHMM_controls"
  )
  controls <- list(
    states = 2,
    sdds = "t",
    data = list(
      file = dax,
      date_column = 1,
      data_column = "Close"
    )
  )
  expect_error(
    set_controls(controls),
    "'date_column' in 'data' must be a single character"
  )
  controls <- list(
    states = 2,
    sdds = "t",
    data = list(
      file = dax,
      date_column = "Date",
      data_column = 1
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "'data_column' in 'data' must be a single character"
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
  expect_s3_class(
    set_controls(controls), 
    "fHMM_controls"
  )
  controls <- list(
    hierarchy = TRUE,
    period = "w"
  )
  controls <- set_controls(controls)
  expect_s3_class(
    set_controls(controls), 
    "fHMM_controls"
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "m"
  )
  expect_s3_class(
    set_controls(controls), 
    "fHMM_controls"
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, 30),
    period = "y"
  )
  expect_s3_class(
    set_controls(controls), 
    "fHMM_controls"
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, 30)
  )
  expect_s3_class(
    set_controls(controls), 
    "fHMM_controls"
  )
})

test_that("checks of controls for empirical HHMM work", {
  skip_if_offline()
  dax <- download_yahoo("^GDAXI")
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = dax,
      data_column = c("Close", "Close"),
      from = "2020-01-01",
      to = "2020-02-02"
    )
  )
  expect_s3_class(
    set_controls(controls), 
    "fHMM_controls"
  )
  expect_error(
    set_controls(
      list(
        hierarchy = TRUE,
        horizon = c(NA, NA, NA),
        period = "w",
        data = list(file = dax)
      )
    ),
    "'horizon' must be a vector of length 2"
  )
  expect_error(
    set_controls(
      controls = list(
        hierarchy = TRUE,
        horizon = c("A", "B"),
        period = "w",
        data = list(file = dax)
      )
    ),
    "'horizon' must be an integer vector of length 2"
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = dax,
      data_column = c("one", "two", "three")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "'data_column' in 'data' must be a character vector of length two."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = dax,
      logreturns = TRUE
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "'logreturns' in 'data' must be a boolean vector of length two."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = dax,
      merge = "not_a_function"
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "The control 'merge' in 'data' must be a function."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = dax,
      merge = function(x) 1:2
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "'merge' in 'data' should merge a vector into a single number"
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = dax,
      date_column = "bad_name",
      data_column = c("Close", "Close")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "Column 'bad_name' not found in data.frame 'file'."
  )
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = dax,
      data_column = c("Close", "bad_name")
    )
  )
  expect_error(
    controls <- set_controls(controls),
    "Column 'bad_name' not found in data.frame 'file'."
  )
})

test_that("print method of controls works", {
  expect_error(
    print.fHMM_controls("not_fHMM_controls"),
    "Not an object of class 'fHMM_controls'."
  )
  expect_snapshot(
    set_controls()
  )
})

test_that("summary method of controls works", {
  expect_error(
    summary.fHMM_controls("not_fHMM_controls"),
    "Not an object of class 'fHMM_controls'."
  )
  expect_snapshot(
    summary(set_controls())
  )
})
