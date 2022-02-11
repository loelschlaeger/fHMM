test_that("ci computation works", {
  data(dax_model)
  expect_snapshot(compute_ci(x = dax_model, alpha = 0.05))
})
