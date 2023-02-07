test_that("checks of format 'YYYY-MM-DD' for dates work", {
  expect_equal(check_date(date = "2000-01-01"), as.Date("2000-01-01"))
  expect_error(check_date(date = "2000-02-30"))
  expect_error(check_date(date = "2000-13-01"))
  expect_error(check_date(date = "01.01.2021"))
})

test_that("finding closest year works", {
  result <- find_closest_year(as.Date("2022-06-01"))
  expect_equal(result, 2022)
  result <- find_closest_year(as.Date("2022-06-30"))
  expect_equal(result, 2022)
  result <- find_closest_year(as.Date("2022-07-01"))
  expect_equal(result, 2023)
  result <- find_closest_year(as.Date("2022-12-31"))
  expect_equal(result, 2023)
})

test_that("check for number works", {
  expect_false(is_number("1"))
  expect_true(is_number(1))
  expect_false(is_number(1.1, int = TRUE))
  expect_false(is_number(x = numeric()))
  expect_equal(is_number(-2:2, neg = TRUE), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(is_number(-2:2, non_neg = TRUE), c(FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(is_number(-2:2, pos = TRUE), c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(is_number(-2:2, non_pos = TRUE), c(TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("check for tpm works", {
  expect_true(is_tpm(matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2)))
  expect_false(is_tpm(matrix(c(0.8, 0.2, 0.2, 0.9), 2, 2)))
  expect_false(is_tpm(matrix(c(0.8, -0.2, 0.2, 0.9), 2, 2)))
})

test_that("brute force matching works", {
  expect_equal(match_all(1:9, 9:1), 9:1)
})

test_that("sample tpm works", {
  sampled_tpm <- sample_tpm(3)
  expect_true(is_tpm(sampled_tpm))
})

test_that("simulation of Markov chain works", {
  expect_error(simulate_markov_chain(Gamma = matrix(1,2,2)))
  expect_error(simulate_markov_chain(Gamma = diag(2), T = -1))
  expect_error(simulate_markov_chain(Gamma = diag(2), T = 10, delta = -1))
  expect_error(simulate_markov_chain(Gamma = diag(2), T = 10, delta = c(0.5, 0.5), total_length = 9))
  Gamma <- rbind(c(0.8, 0.2), c(0.1, 0.9))
  expect_equal(
    simulate_markov_chain(Gamma = Gamma, T = 10, seed = 1, total_length = 11),
    c(2, 2, 2, 1, 1, 2, 1, 1, 1, 1, NA_integer_)
  )
})
