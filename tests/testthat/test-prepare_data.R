test_that("data preparation for simulated HMM works", {
  controls <- set_controls()
  out <- prepare_data(controls, seed = 1)
  expect_snapshot(out)
  expect_snapshot(unlist(out))
})
