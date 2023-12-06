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
  out <- prepare_data(data_frame = download_yahoo("^GDAXI"), verbose = FALSE)
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
    data_frame = download_yahoo("^GDAXI"), 
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





