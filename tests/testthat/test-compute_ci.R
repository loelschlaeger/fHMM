test_that("ci computation works", {
  data("dax_model_3t")
  expect_snapshot(compute_ci(x = dax_model_3t, alpha = 0.05))
})
