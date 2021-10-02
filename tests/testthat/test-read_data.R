test_that("multiplication works", {
  skip_if_offline()
  file = paste0(tempdir(),"/dax.csv")
  sink(tempfile())
    download_data(symbol = "^GDAXI", file = file)
  sink()
  controls = list(
    states  = 2,
    sdds    = "t",
    horizon = 400,
    data    = list(file   = file,
                   column = "Close")
  )
  controls = set_controls(controls)
  expect_snapshot(read_data(controls))
  controls = list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "m",
    data    = list(file   = c(file,file),
                   column = c("Close","Close"))
  )
  controls = set_controls(controls)
  #expect_snapshot(read_data(controls))
})
