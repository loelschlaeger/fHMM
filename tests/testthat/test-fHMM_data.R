test_that("data preparation for simulated HMM works", {
  controls <- set_controls()
  out <- prepare_data(controls, seed = 1)
  expect_null(out$dates)
  expect_equal(out$time_points, 1:100)
})
