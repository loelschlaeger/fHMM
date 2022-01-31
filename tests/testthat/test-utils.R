test_that("checks of format 'YYYY-MM-DD' for dates work", {
  expect_equal(check_date(date = "2000-01-01"), as.Date("2000-01-01"))
  expect_error(check_date(date = "2000-02-30"))
  expect_error(check_date(date = "2000-13-01"))
  expect_error(check_date(date = "01.01.2021"))
})


test_that("check for number works", {
  expect_equal(is_number(c("1", 1)), c(FALSE, TRUE))
  expect_false(is_number(1.1, int = TRUE))
  expect_true(is_number(x = numeric()))
})

test_that("check for tpm works", {
  expect_true(is_tpm(matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2)))
  expect_false(is_tpm(matrix(c(0.8, 0.2, 0.2, 0.9), 2, 2)))
  expect_false(is_tpm(matrix(c(0.8, -0.2, 0.2, 0.9), 2, 2)))
})

test_that("brute force matching works", {
  expect_equal(match_all(1:9, 9:1), 9:1)
})