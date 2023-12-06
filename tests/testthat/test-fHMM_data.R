test_that("data preparation for simulated HMM works", {
  out <- prepare_data()
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
  pdf(file = NULL)
  expect_null(plot(out))
  dev.off()
})

test_that("data preparation for simulated HHMM works", {
  out <- prepare_data(hierarchy = TRUE)
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
  pdf(file = NULL)
  expect_null(plot(out))
  dev.off()
})

test_that("data preparation for empirical HMM works", {
  out <- prepare_data(
    data_frame = download_yahoo("^GDAXI"), 
    from = "2000-01-01",
    to = "2020-01-01",
    verbose = FALSE
  )
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
  events <- list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c(
      "9/11 terrorist attack", "Bankruptcy Lehman Brothers",
      "First COVID-19 case Germany"
    )
  )
  events <- fHMM_events(events)
  pdf(file = NULL)
  expect_null(plot(out, events = events))
  dev.off()
})

test_that("data preparation for empirical HHMM works", {
  out <- prepare_data(
    hierarchy = TRUE,
    sdds = c("t", "gamma"),
    data_frame = download_yahoo("^GDAXI"), 
    verbose = FALSE,
    logreturns = c(TRUE, TRUE),
    horizon = c(NA, NA)
  )
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
  events <- list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c(
      "9/11 terrorist attack", "Bankruptcy Lehman Brothers",
      "First COVID-19 case Germany"
    )
  )
  events <- fHMM_events(events)
  pdf(file = NULL)
  expect_null(plot(out, events = events))
  dev.off()
})

test_that("input checks for data plotting work", {
  expect_error(
    plot.fHMM_data(1),
    "'x' is not of class 'fHMM_data'"
  )
  expect_error(
    plot(prepare_data(), events = "event"),
    "'events' is not of class 'fHMM_events'."
  )
  events <- list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c(
      "9/11 terrorist attack", "Bankruptcy Lehman Brothers",
      "First COVID-19 case Germany"
    )
  )
  events <- fHMM_events(events)
  pdf(file = NULL)
  expect_warning(
    plot(prepare_data(), events = events),
    "Can't have 'events' for simulated data."
  )
  dev.off()
  expect_error(
    plot(prepare_data(), title = 1),
    "'title' must be a single 'character'"
  )
})
