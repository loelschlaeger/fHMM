test_that("check for number works", {
  expect_equal(is_number(c("1", 1)), c(FALSE, TRUE))
  expect_false(is_number(1.1, int = TRUE))
  expect_true(is_number(x = numeric()))
})
