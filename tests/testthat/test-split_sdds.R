test_that("splitting fixed parameters from a ssd works", {
  expect_equal(unclass(split_sdds(sdds = "t")), 
               list(list(name = "t", fixed = list())))
  expect_equal(unclass(split_sdds(sdds = "t(scale = 2, shape = 3)")), 
               list(list(name = "t", fixed = list())))
  expect_equal(unclass(split_sdds(sdds = "t(0)")), 
               list(list(name = "t", fixed = list())))
  expect_equal(unclass(split_sdds(sdds = "gamma")), 
               list(list(name = "gamma", fixed = list())))
  expect_equal(unclass(split_sdds(sdds = "t(mu = 1, sigma = 2, df = 3)")),
               list(list(name = "t", fixed = list("mu" = 1, "sigma" = 2, "df" = 3))))
  expect_equal(unclass(split_sdds(sdds = c("t(mu = 1, sigma = 2, df = 3)","gamma(sigma = 1)"))),
               list(list(name = "t", fixed = list("mu" = 1, "sigma" = 2, "df" = 3)),
                    list(name = "gamma", fixed = list("sigma" = 1))))
  expect_error(split_sdds(sdds = "norm"), "C4")
  expect_error(split_sdds(sdds = "t(sigma = 0)"), "C5")
  expect_error(split_sdds(sdds = "t(df = -1)"), "C5")
})
