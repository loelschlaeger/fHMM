test_that("input checks for sdds work", {
  expect_error(
    fHMM_sdds(sdds = "normal", states = diag(2)),
    "The control 'states' must be a vector."
  )
  expect_error(
    fHMM_sdds(sdds = "normal", states = 1),
    "The control 'states' must be an integer greater or equal 2."
  )
  expect_error(
    fHMM_sdds(sdds = c("normal", "normal"), states = c(pi, pi)),
    "The control 'states' must be a vector of integers greater or equal 2."
  )
  expect_error(
    fHMM_sdds(sdds = "normal", states = 1:3),
    "The control 'states' must be a vector of length 1 or 2."
  )
  expect_equal(
    fHMM_sdds("normal", states = 2),
    fHMM_sdds(fHMM_sdds("normal", states = 2), states = 2)
  )
  expect_error(
    fHMM_sdds(sdds = 1, states = 2),
    "The control 'sdds' must be a character of length 1."
  )
  expect_error(
    fHMM_sdds(sdds = 1, states = c(2, 2)),
    "The control 'sdds' must be a character vector of length 2."
  )
})

test_that("splitting pars parameters from a ssd works", {
  sink(tempfile())
  expect_s3_class(
    print(fHMM_sdds(sdds = "t", states = 2)), 
    "fHMM_sdds"
  )
  sink()
  expect_equal(
    unclass(fHMM_sdds(sdds = "t", states = 2)),
    list(list(name = "t", pars = list()))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(scale = 2, shape = 3)", states = 2)),
    list(list(name = "t", pars = list()))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(0)", states = 2)),
    list(list(name = "t", pars = list()))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "gamma", states = 2)),
    list(list(name = "gamma", pars = list()))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "poisson", states = 2)),
    list(list(name = "poisson", pars = list()))
  )
  expect_error(
    fHMM_sdds(sdds = "poisson(mu = 1|2|3", states = 4),
    "Fixed values for the parameter 'mu' must be of length 1 or 4."
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(mu = 1, sigma = 2, df = 3)", states = 2)),
    list(list(name = "t", pars = list("mu" = 1, "sigma" = 2, "df" = 3)))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(mu = 1|2, sigma = 3, df = 4)", states = 2)),
    list(list(name = "t", pars = list("mu" = 1:2, "sigma" = 3, "df" = 4)))
  )
  expect_equal(
    unclass(fHMM_sdds(
      sdds = c("t(mu = 1, sigma = 2, df = 3)", "gamma(sigma = 1)"), 
      states = c(2, 3)
    )),
    list(
      list(name = "t", pars = list("mu" = 1, "sigma" = 2, "df" = 3)),
      list(name = "gamma", pars = list("sigma" = 1))
    )
  )
  expect_equal(
    unclass(fHMM_sdds(
      sdds = c("t(mu = 1|2, sigma = 3|4, df = 5|6)", "gamma(sigma = 1|2|3)"),
      states = c(2, 3)
    )),
    list(
      list(name = "t", pars = list("mu" = 1:2, "sigma" = 3:4, "df" = 5:6)),
      list(name = "gamma", pars = list("sigma" = 1:3))
    )
  )
  expect_error(
    fHMM_sdds(sdds = "norm", states = 2),
    "Currently, only the following distributions are implemented"
  )
  expect_error(
    fHMM_sdds(sdds = "t(sigma = 0)", states = 2),
    "'sigma' must be a positive numeric."
  )
  expect_error(
    fHMM_sdds(sdds = "t(df = -1)", states = 2),
    "'df' must be a positive numeric."
  )
  expect_error(
    fHMM_sdds(sdds = "gamma(mu = -1)", states = 2),
    "'mu' must be a positive numeric."
  )
})
