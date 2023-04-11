test_that("defining state-dependent distributions works", {
  expect_error(
    fHMM_sdds(sdds = "t", states = 1),
    "The control 'states' must be an integer greater or equal 2."
  )
  expect_error(
    fHMM_sdds(sdds = c("t", "t"), states = c(1, 2)),
    "The control 'states' must be a vector of integers greater or equal 2."
  )
  expect_error(
    fHMM_sdds(sdds = c("t", "t"), states = 1:3),
    "The control 'states' must be a vector of length 1 or 2."
  )
  expect_error(
    fHMM_sdds(sdds = c("t", "t"), states = 2),
    "The control 'sdds' must be a character of length 1."
  )
  expect_error(
    fHMM_sdds(sdds = c("t"), states = c(2, 2)),
    "The control 'sdds' must be a character vector of length 2."
  )
  expect_error(
    print.fHMM_sdds(x = "t"), 
    "Not of class 'fHMM_sdds'."
  )
  sink(tempfile())
  expect_s3_class(
    print(fHMM_sdds(sdds = "t", states = 2)), 
    "fHMM_sdds"
  )
  sdds <- fHMM_sdds(sdds = "t", states = 2)
  expect_s3_class(
    print(fHMM_sdds(sdds = sdds, states = 2)), 
    "fHMM_sdds"
  )
  expect_s3_class(
    print(fHMM_sdds(sdds = c("t", "t"), states = 2:3)), 
    "fHMM_sdds"
  )
  sdds <- fHMM_sdds(sdds = c("t", "t"), states = 2:3)
  expect_s3_class(
    print(fHMM_sdds(sdds = sdds, states = 2:3)), 
    "fHMM_sdds"
  )
  sink()
  expect_equal(
    unclass(fHMM_sdds(sdds = "t", states = 3)),
    list(list("name" = "t", "pars" = list(), "label" = "t()"))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(scale = 2, shape = 3)", states = 4)),
    list(list("name" = "t", "pars" = list(), "label" = "t()"))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(0)", states = 3)),
    list(list("name" = "t", "pars" = list(), "label" = "t()"))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "gamma", states = 2)),
    list(list("name" = "gamma", "pars" = list(), "label" = "gamma()"))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "poisson", states = 2)),
    list(list("name" = "poisson", "pars" = list(), "label" = "poisson()"))
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "poisson(mu = 1|2)", states = 2)),
    list(
      list(
        name = "poisson", pars = list(mu = c(1, 2)), 
        label = "poisson(mu = 1|2)"
      )
    )
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(mu = 1, sigma = 2, df = 3)", states = 2)),
    list(
      list(
        "name" = "t", "pars" = list(
          "mu" = c(1, 1), "sigma" = c(2, 2), "df" = c(3, 3)
        ),
        "label" = "t(mu = 1, sigma = 2, df = 3)"
      )
    )
  )
  expect_equal(
    unclass(fHMM_sdds(sdds = "t(mu = 1|2, sigma = 3, df = 4)", states = 2)),
    list(
      list(
        "name" = "t", "pars" = list(
          "mu" = 1:2, "sigma" = c(3, 3), "df" = c(4, 4)
        ),
        "label" = "t(mu = 1|2, sigma = 3, df = 4)"
      )
    )
  )
  expect_equal(
    unclass(fHMM_sdds(
      sdds = c("t(mu = 1, sigma = 2, df = 3)", "gamma(sigma = 1)"),
      states = c(2, 2)
    )),
    list(
      list(
        "name" = "t", "pars" = list(
          "mu" = c(1, 1), "sigma" = c(2, 2), "df" = c(3, 3)
        ),
        "label" = "t(mu = 1, sigma = 2, df = 3)"
      ),
      list(
        "name" = "gamma", "pars" = list("sigma" = c(1, 1)), 
        "label" = "gamma(sigma = 1)"
      )
    )
  )
  expect_equal(
    unclass(fHMM_sdds(
      sdds = c("t(mu = 1|2, sigma = 3|4, df = 5|6)", "gamma(sigma = 1|2|3)"),
      states = c(2, 3)
    )),
    list(
      list(
        "name" = "t", "pars" = list(
          "mu" = 1:2, "sigma" = 3:4, "df" = 5:6
        ),
        "label" = "t(mu = 1|2, sigma = 3|4, df = 5|6)"
      ),
      list(
        "name" = "gamma", "pars" = list(
          "sigma" = 1:3
        ),
        "label" = "gamma(sigma = 1|2|3)"
      )
    )
  )
  expect_error(
    fHMM_sdds(sdds = "poisson(mu = 1|2)", states = 3),
    "Number of fixed parameters"
  )
  expect_error(
    fHMM_sdds(sdds = "norm", states = 2),
    "Currently, only the following distributions are implemented:"
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
