test_that("computation of T_star works", {
  expect_equal(
    compute_T_star(horizon = c(10, 3), period = NA, seed = 1),
    c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L)
  )
  expect_equal(
    compute_T_star(horizon = c(5, NA), period = "w", seed = 1),
    c(5L, 5L, 5L, 4L, 5L)
  )
  expect_equal(
    compute_T_star(horizon = c(12, NA), period = "m", seed = 1),
    c(23L, 22L, 24L, 20L, 23L, 25L, 20L, 24L, 24L, 23L, 23L, 23L)
  )
  expect_equal(
    compute_T_star(horizon = c(2, NA), period = "q", seed = 1),
    c(64L, 62L)
  )
  expect_equal(
    compute_T_star(horizon = c(7, NA), period = "y", seed = 1),
    c(236L, 232L, 238L, 226L, 233L, 242L, 243L)
  )
  expect_equal(
    compute_T_star(horizon = c(5, 2), period = NA, dates = (as.Date("2022-01-01") - 1:10), seed = 1),
    c(2L, 2L, 2L, 2L, 2L)
  )
  expect_equal(
    compute_T_star(horizon = c(4, NA), period = "w", dates = (as.Date("2022-01-01") - 1:20), seed = 1),
    c(1L, 7L, 7L, 5L)
  )
  expect_equal(
    compute_T_star(horizon = c(6, NA), period = "m", dates = (as.Date("2022-01-01") - 1:100), seed = 1),
    c(8L, 31L, 30L, 31L)
  )
  expect_equal(
    compute_T_star(horizon = c(100, NA), period = "q", dates = (as.Date("2022-01-01") - 1:300), seed = 1),
    c(25L, 91L, 92L, 92L)
  )
  expect_equal(
    compute_T_star(horizon = c(100, NA), period = "y", dates = (as.Date("2022-01-01") - 1:3000), seed = 1),
    c(78L, 365L, 365L, 366L, 365L, 365L, 365L, 366L, 365L)
  )
})
