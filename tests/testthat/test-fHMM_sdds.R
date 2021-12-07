test_that("splitting pars parameters from a ssd works", {
  expect_equal(
    unclass(fHMM_sdds(sdds = "t")),
    list(list(name = "t", pars = list()))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(scale = 2, shape = 3)")),
    list(list(name = "t", pars = list()))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(0)")),
    list(list(name = "t", pars = list()))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "gamma")),
    list(list(name = "gamma", pars = list()))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(mu = 1, sigma = 2, df = 3)")),
    list(list(name = "t", pars = list("mu" = 1, "sigma" = 2, "df" = 3)))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(mu = 1|2, sigma = 3, df = 4)")),
    list(list(name = "t", pars = list("mu" = 1:2, "sigma" = 3, "df" = 4)))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = c("t(mu = 1, sigma = 2, df = 3)", "gamma(sigma = 1)"))),
    list(
      list(name = "t", pars = list("mu" = 1, "sigma" = 2, "df" = 3)),
      list(name = "gamma", pars = list("sigma" = 1))
    )
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = c("t(mu = 1|2, sigma = 3|4, df = 5|6)", "gamma(sigma = 1|2|3)"))),
    list(
      list(name = "t", pars = list("mu" = 1:2, "sigma" = 3:4, "df" = 5:6)),
      list(name = "gamma", pars = list("sigma" = 1:3))
    )
  )
  expect_error(fHMM_sdds(sdds = "norm"))
  expect_error(fHMM_sdds(sdds = "t(sigma = 0)"))
  expect_error(fHMM_sdds(sdds = "t(df = -1)"))
})
