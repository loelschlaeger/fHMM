test_that("HMM fitting works", {
  seed <- 1
  controls <- list(
    states  = 2,
    sdds    = "gamma",
    horizon = 100,
    fit     = list("runs" = 10)
  )
  controls <- set_controls(controls)
  data <- prepare_data(controls, seed = seed)
  model <- fit_model(data, ncluster = 1, seed = seed, verbose = FALSE)
  expect_snapshot(model)
})

test_that("HHMM fitting works", {
  seed <- 1
  controls <- list(
    hierarchy = TRUE,
    states = c(2, 2),
    sdds = c(
      "t(sigma = 0.1, df = Inf)",
      "gamma(sigma = 0.1)"
    ),
    horizon = c(10, 5),
    fit = list("runs" = 2)
  )
  controls <- set_controls(controls)
  data <- prepare_data(controls, seed = seed)
  model <- fit_model(data, ncluster = 1, seed = seed, verbose = FALSE)
  expect_snapshot(model)
})

test_that("log-normal sdds works", {
  seed <- 1
  controls <- list(
    states  = 2,
    sdds    = "lnorm(mu = 1|3)",
    horizon = 1000,
    fit     = list("runs" = 100)
  )
  controls <- set_controls(controls)
  data <- prepare_data(controls, seed = seed)
  model <- fit_model(data, ncluster = 1, seed = seed, verbose = FALSE)
  expect_snapshot(model)
})
