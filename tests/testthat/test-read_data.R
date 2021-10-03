test_that("read of data works", {
  skip_if_offline()
  
  ### HMM with DAX data
  file = paste0(tempdir(),"/dax.csv")
  sink(tempfile())
  download_data(symbol = "^GDAXI", file = file)
  sink()
  controls = list(
    data = list(file = file,
                data_column = "Close")
  )
  controls = set_controls(controls)
  expect_snapshot(read_data(controls))
  
  ### HMM with setup data
  file = paste0(tempdir(),"/test.csv")
  data = data.frame("a" = c("2020-01-01","2020-01-02"), "b" = 1:2)
  write.csv(data, file)
  controls = list(
    data = list(file = file,
                date_column = "a",
                data_column = "b")
  )
  controls = set_controls(controls)
  out = prepare_data(controls)
  expect_equal(out$dates, data$a)
  expect_equal(out$time_points, NA)
  expect_null(out$markov_chain)
  expect_equal(out$data, data$b)
  expect_equal(out$time_series, data$b)
  expect_null(out$T_star)
  expect_equal(out$controls, controls)
  
  ### HHMM
  file = paste0(tempdir(),"/dax.csv")
  sink(tempfile())
  download_data(symbol = "^GDAXI", file = file)
  sink()
  controls = list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "m",
    data = list(file = c(file,file),
                data_column = c("Close","Close"))
  )
  controls = set_controls(controls)
  expect_snapshot(read_data(controls))

  
})
