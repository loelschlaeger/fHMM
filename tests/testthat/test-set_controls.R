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
})
