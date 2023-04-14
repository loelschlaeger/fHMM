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
})
