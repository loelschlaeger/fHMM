test_that("defining state-dependent distributions works", {
  expect_error(
    fHMM_sdds(sdds = "t", states = list()),
    "The control 'states' must be a vector."
  )
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
  expect_s3_class(
    fHMM_sdds(sdds = "t", states = 2), 
    "fHMM_sdds"
  )
  expect_s3_class(
    fHMM_sdds(sdds = fHMM_sdds(sdds = "t", states = 2), states = 2), 
    "fHMM_sdds"
  )
  expect_s3_class(
    fHMM_sdds(sdds = c("t", "t"), states = 2:3), 
    "fHMM_sdds"
  )
  expect_s3_class(
    fHMM_sdds(sdds = fHMM_sdds(sdds = c("t", "t"), states = 2:3), states = 2:3), 
    "fHMM_sdds"
  )
  expect_error(
    fHMM_sdds(sdds = "poisson(mu = 1|2)", states = 3),
    "Number of fixed parameters"
  )
  expect_error(
    fHMM_sdds(sdds = "t(sigma = 0)", states = 2),
    "'sigma' must be positive"
  )
  expect_error(
    fHMM_sdds(sdds = "t(df = -1)", states = 2),
    "'df' must be positive"
  )
  expect_error(
    fHMM_sdds(sdds = "gamma(mu = -1)", states = 2),
    "'mu' must be positive"
  )
  expect_error(
    fHMM_sdds(sdds = "unknown", states = 2),
    "only the following distributions are implemented"
  )
  expect_warning(
    fHMM_sdds(sdds = "t(bad = -1)", states = 2),
    "ignored"
  )
  expect_s3_class(
    fHMM_sdds(sdds = "t(mu = 1, sigma = 2)", states = 4), 
    "fHMM_sdds"
  )
})

test_that("calling state-dependent distribution functions works", {
  x <- fHMM_sdds(sdds = "normal", states = 2)
  expect_error(
    x[[1]]$sample(),
    "Which state?"
  )
  expect_error(
    x[[1]]$sample(state = 1),
    "'mu' must be specified"
  )
  expect_equal(
    x[[1]]$density(x = 0, state = 1, mu = 0, sigma = 1),
    stats::dnorm(0)
  )
  x <- fHMM_sdds(sdds = "lognormal", states = 2)
  expect_type(
    x[[1]]$sample(state = 1, mu = 1, sigma = 1),
    "double"
  )
  x <- fHMM_sdds(sdds = "t(mu = 1, sigma = 2)", states = 4)
  expect_type(
    x[[1]]$sample(state = 1, mu = 1, sigma = 1, df = 1),
    "double"
  )
  x <- fHMM_sdds(sdds = "gamma", states = 2)
  expect_error(
    x[[1]]$sample(state = 1, mu = 1),
    "'sigma' must be specified"
  )
  expect_type(
    x[[1]]$sample(state = 1, mu = 1, sigma = 1),
    "double"
  )
  x <- fHMM_sdds(sdds = "poisson", states = 2)
  expect_error(
    x[[1]]$sample(state = 1),
    "'mu' must be specified"
  )
  expect_type(
    x[[1]]$sample(state = 1, mu = 1),
    "integer"
  )
})

test_that("print method works", {
  expect_error(
    print.fHMM_sdds(x = "t"), 
    "Not of class 'fHMM_sdds'."
  )
  expect_snapshot(
    fHMM_sdds(sdds = "t(mu = 1, sigma = 2)", states = 4)
  )
  expect_snapshot(
    fHMM_sdds(sdds = c("t", "t"), states = c(2, 3))
  )
})

test_that("allprobs matrix can be created", {
  observations <- 1:10
  sdd <- fHMM_sdds("normal", 2)[[1]]
  expect_identical(
    allprobs(observations, sdd, 2, "mu" = 1:2, "sigma" = 1:2),
    rbind(
      dnorm(observations, mean = 1, sd = 1), 
      dnorm(observations, mean = 2, sd = 2)
    )
  )
})
