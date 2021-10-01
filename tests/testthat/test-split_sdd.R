test_that("splitting fixed parameters from a ssd works", {
  expect_equal(unclass(split_sdd(sdd = "t")), 
               list(name = "t", fixed = list()))
  expect_equal(unclass(split_sdd(sdd = "t(scale = 2, shape = 3)")), 
               list(name = "t", fixed = list()))
  expect_equal(unclass(split_sdd(sdd = "t(0)")), 
               list(name = "t", fixed = list()))
  expect_equal(unclass(split_sdd(sdd = "gamma")), 
               list(name = "gamma", fixed = list()))
  expect_equal(unclass(split_sdd(sdd = "t(mu = 1, sigma = 2, df = 3)")),
               list(name = "t", fixed = list("mu" = 1, "sigma" = 2, "df" = 3)))
  expect_error(split_sdd(sdd = "norm"), "C4")
  expect_error(split_sdd(sdd = "t(sigma = 0)"), "C5")
  expect_error(split_sdd(sdd = "t(df = -1)"), "C5")
})
