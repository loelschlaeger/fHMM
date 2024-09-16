test_that("checks of format 'YYYY-MM-DD' for dates work", {
  expect_equal(
    check_date(date = "2000-01-01"),
    as.Date("2000-01-01")
  )
  expect_error(
    check_date(date = "2000-02-30"),
    "Date is not in required format 'YYYY-MM-DD'."
  )
  expect_error(
    check_date(date = "2000-13-01"),
    "Date is not in required format 'YYYY-MM-DD'."
  )
  expect_error(
    check_date(date = "01.01.2021"),
    "Date is not in required format 'YYYY-MM-DD'."
  )
})

test_that("finding closest year works", {
  expect_equal(
    find_closest_year(as.Date("2022-06-01")),
    2022
  )
  expect_equal(
    find_closest_year(as.Date("2022-06-30")),
    2022
  )
  expect_equal(
    find_closest_year(as.Date("2022-07-01")),
    2023
  )
  expect_equal(
    find_closest_year(as.Date("2022-12-31")),
    2023
  )
})

test_that("list to vector works", {
  expect_equal(
    list_to_vector(list(1, 2, NA, NULL)),
    c(1, 2, NA, NA)
  )
})
