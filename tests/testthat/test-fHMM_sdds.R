test_that("splitting pars parameters from a ssd works", {
  expect_equal(unclass(fHMM_sdds(sdds = "t")), 
               list(list(name = "t", pars = list("mu" = NA, "sigma" = NA, "df" = NA))))
  expect_equal(unclass(fHMM_sdds(sdds = "t(scale = 2, shape = 3)")), 
               list(list(name = "t", pars = list("mu" = NA, "sigma" = NA, "df" = NA))))
  expect_equal(unclass(fHMM_sdds(sdds = "t(0)")), 
               list(list(name = "t", pars = list("mu" = NA, "sigma" = NA, "df" = NA))))
  expect_equal(unclass(fHMM_sdds(sdds = "gamma")), 
               list(list(name = "gamma", pars = list("mu" = NA, "sigma" = NA))))
  expect_equal(unclass(fHMM_sdds(sdds = "t(mu = 1, sigma = 2, df = 3)")),
               list(list(name = "t", pars = list("mu" = 1, "sigma" = 2, "df" = 3))))
  expect_equal(unclass(fHMM_sdds(sdds = c("t(mu = 1, sigma = 2, df = 3)","gamma(sigma = 1)"))),
               list(list(name = "t", pars = list("mu" = 1, "sigma" = 2, "df" = 3)),
                    list(name = "gamma", pars = list("sigma" = 1, "mu" = NA))))
  expect_error(fHMM_sdds(sdds = "norm"))
  expect_error(fHMM_sdds(sdds = "t(sigma = 0)"))
  expect_error(fHMM_sdds(sdds = "t(df = -1)"))
})
