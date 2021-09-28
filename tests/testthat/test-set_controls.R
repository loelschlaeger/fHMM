test_that("checks of controls work", {
  controls = list(
    path    = ".",
    states  = 2,
    sdds    = "t",
    horizon = 400,
    fit     = list("runs" = 50)
  )
  controls = set_controls(controls)
  expect_snapshot(
    controls
  )
  expect_snapshot(
    unlist(controls)
  )
})
