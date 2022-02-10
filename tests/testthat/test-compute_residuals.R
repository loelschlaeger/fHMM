test_that("residual computation works", {
  data(dax_model)
  x <- compute_residuals(dax_model, verbose = FALSE)
  expect_snapshot(x)
  expect_snapshot(unclass(x))
})
