test_that("computation of T_star works", {
  expect_snapshot(compute_T_star(horizon = c(100,30), seed = 1))
  expect_snapshot(compute_T_star(horizon = c(100,"w"), seed = 1))
  expect_snapshot(compute_T_star(horizon = c(100,"m"), seed = 1))
  expect_snapshot(compute_T_star(horizon = c(100,"q"), seed = 1))
  expect_snapshot(compute_T_star(horizon = c(100,"y"), seed = 1))
})
