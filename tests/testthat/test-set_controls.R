test_that("checks of controls work", {
  controls = list(
    states  = 2,
    sdds    = "t",
    horizon = 400,
    fit     = list("runs" = 50)
  )
  expect_snapshot(set_controls(controls))
  expect_snapshot(unlist(set_controls(controls)))
  controls = list(
    hierarchy = TRUE,
    horizon = c(100, NA)
  )
  expect_snapshot(set_controls(controls))
  expect_snapshot(unlist(set_controls(controls)))
  controls = list(
    hierarchy = TRUE,
    period = "w"
  )
  expect_snapshot(set_controls(controls))
  expect_snapshot(unlist(set_controls(controls)))
  controls = list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w"
  )
  expect_snapshot(set_controls(controls))
  expect_snapshot(unlist(set_controls(controls)))
  controls = list(
    hierarchy = TRUE,
    horizon = c(100, 30),
    period = "w"
  )
  expect_snapshot(set_controls(controls))
  expect_snapshot(unlist(set_controls(controls)))
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
  expect_snapshot(set_controls(controls))
  expect_snapshot(unlist(set_controls(controls)))
})
