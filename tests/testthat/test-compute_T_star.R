test_that("computation of T_star works", {
  expect_vector(compute_T_star(horizon = c(100, 30), period = NA, seed = 1))
  expect_vector(compute_T_star(horizon = c(100, NA), period = "w", seed = 1))
  expect_vector(compute_T_star(horizon = c(100, NA), period = "m", seed = 1))
  expect_vector(compute_T_star(horizon = c(100, NA), period = "q", seed = 1))
  expect_vector(compute_T_star(horizon = c(100, NA), period = "y", seed = 1))
})
