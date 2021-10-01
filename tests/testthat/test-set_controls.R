test_that("checks of controls work", {
  controls = list(
    states  = 2,
    sdds    = "t",
    horizon = 400,
    fit     = list("runs" = 50)
  )
  expect_snapshot(set_controls(controls))
  expect_snapshot(set_controls(controls))
})
