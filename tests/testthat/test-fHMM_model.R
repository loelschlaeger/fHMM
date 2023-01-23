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
    horizon = 1000,
    fit     = list("runs" = 20, "accept" = "all")
  )
  controls <- set_controls(controls)
  data <- prepare_data(controls, seed = 1)
  expect_s3_class(fit_model(data, ncluster = 2, verbose = FALSE, seed = 1), "fHMM_model")
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
  model <- decode_states(model, verbose = FALSE)
})

test_that("coefficients can be extracted", {
  expect_true(is.data.frame(coef(dax_model_2n)))
  expect_true(is.data.frame(coef(sim_model_2gamma)))
})

test_that("AIC can be computed", {
  expect_equal(round(AIC(dax_model_2n)), -34795)
  expect_equal(round(AIC(dax_model_2n, dax_model_3t)), c(-34795, -35269))
})

test_that("BIC can be computed", {
  expect_equal(round(BIC(dax_model_2n)), -34755)
  expect_equal(round(BIC(dax_model_2n, dax_model_3t)), c(-34755, -35169))
})

test_that("number of observations can be computed", {
  expect_equal(nobs(dax_model_2n), 5882)
})

test_that("log-likelihood can be computed", {
  expect_equal(round(logLik(dax_model_2n)), 17404)
})

test_that("number of model parameters can be computed", {
  expect_equal(npar(dax_model_2n), 6)
  expect_equal(npar(dax_model_2n, dax_model_3t), c(6, 15))
})

test_that("input checks for prediction work", {
  expect_error(
    predict.fHMM_model("1"),
    "'object' must be of class 'fHMM_model'."
  )
  expect_error(
    predict.fHMM_model(dax_model_3t, ahead = -1),
    "'ahead' must be a positive integer."
  )
  expect_error(
    predict.fHMM_model(dax_model_3t, ahead = 5, alpha = 1.1),
    "'alpha' must be a numeric between 0 and 1."
  )
})

test_that("prediction works", {
  prediction <- predict(dax_model_3t)
  expect_s3_class(prediction, "fHMM_predict")
})

