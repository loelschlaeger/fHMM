test_that("residual computation works", {
  data(dax_model)
  xx <- compute_residuals(dax_model, verbose = FALSE)
  expect_snapshot(xx)
  expect_snapshot(unclass(xx))
})
