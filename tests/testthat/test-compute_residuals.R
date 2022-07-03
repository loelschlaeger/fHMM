test_that("residual computation works", {
  data("dax_model_3t")
  x <- compute_residuals(dax_model_3t, verbose = FALSE)
  expect_snapshot(x)
  expect_snapshot(unclass(x))
})
