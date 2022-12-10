test_that("HMM fitting works", {
  expect_error(fit_model("not_an_fHMM_data_object"))
  controls <- list(
    states  = 2,
    sdds    = "gamma",
    horizon = 100,
    fit     = list("runs" = 10)
  )
  controls <- set_controls(controls)
  data <- prepare_data(controls, seed = 1)
  expect_error(fit_model(data, ncluster = 1.4))
  expect_error(fit_model(data, verbose = "not_TRUE_or_FALSE"))
  model <- fit_model(data, ncluster = 1, seed = 1, verbose = FALSE)
  expect_s3_class(model, "fHMM_model")
  expect_equal(
    round(model$estimate, 2),
    structure(c(2.04, -0.34, -0.6, -3.56, -0.2, -2.09), class = "parUncon")
  )
  model_refit <- fit_model(data, ncluster = 1, verbose = FALSE, init = model$estimate)
  expect_s3_class(model_refit, "fHMM_model")
  controls <- list(
    states  = 2,
    sdds    = "gamma",
    horizon = 100,
    fit     = list("runs" = 10, "origin" = TRUE)
  )
  controls <- set_controls(controls)
  data <- prepare_data(controls, seed = 1)
  model_origin <- fit_model(data, ncluster = 1, seed = 1, verbose = FALSE)
  expect_s3_class(model_origin, "fHMM_model")
})

test_that("printing progress works", {
  controls <- list(
    states  = 2,
    sdds    = "gamma",
    horizon = 100,
    fit     = list("runs" = 1)
  )
  controls <- set_controls(controls)
  data <- prepare_data(controls, seed = 1)
  suppressMessages(
    expect_s3_class(fit_model(data, verbose = TRUE), "fHMM_model")
  )
})

test_that("parallelization works", {
  skip_on_cran()
  controls <- list(
    states  = 2,
    sdds    = "t",
    horizon = 100,
    fit     = list("runs" = 10)
  )
  controls <- set_controls(controls)
  data <- prepare_data(controls, seed = 1)
  expect_s3_class(fit_model(data, ncluster = 2, verbose = FALSE), "fHMM_model")
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
  expect_equal(
    round(model$estimate, 2),
    structure(c(0.48, -30.95, -0.09, 0.03, -0.74, -0.43, -1.23, -0.18, 
                0.93, 0.02, -4.74, 3.02), class = "parUncon")
  )
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
  expect_equal(
    round(model$estimate, 2),
    structure(c(-0.79, 0.78, -0.74, -0.03), class = "parUncon")
  )
})
