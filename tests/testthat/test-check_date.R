test_that("checks of format 'YYYY-MM-DD' for dates work", {
  error_code <- "C7"
  expect_equal(check_date(date = "2000-01-01"), as.Date("2000-01-01"))
  expect_error(check_date(date = "2000-02-30"), error_code)
  expect_error(check_date(date = "2000-13-01"), error_code)
  expect_error(check_date(date = "01.01.2021"), error_code)
})
