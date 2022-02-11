test_that("creation of parameter labels works", {
  expect_snapshot(parameter_labels(set_controls(), 8))
})
