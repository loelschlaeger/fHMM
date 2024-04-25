test_that("ci computation works", {
  expect_error(compute_ci("not_an_fHMM_model"))
  expect_error(compute_ci(dax_model_3t, "not_a_numeric"))
  expect_error(compute_ci(dax_model_3t, -1))
  expect_error(compute_ci(dax_model_3t, 2))
  dax_model_3t_tmp <- dax_model_3t
  dax_model_3t_tmp$inverse_fisher[3] <- -1
  suppressWarnings(expect_warning(compute_ci(dax_model_3t_tmp)))
  ci <- compute_ci(x = dax_model_3t, alpha = 0.05)
  expect_type(ci, "list")
  expect_length(ci, 3)
  expect_named(ci, c("lb", "estimate", "ub"))
})
