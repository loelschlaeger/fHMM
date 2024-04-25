test_that("HMM fitting works", {
  expect_error(fit_model("not_an_fHMM_data_object"))
  controls <- list(
    states  = 2,
    sdds    = "normal",
    horizon = 100,
    fit     = list("runs" = 10)
  )
  controls <- set_controls(controls)
  data <- prepare_data(
    controls, 
    true_parameters = fHMM_parameters(
      controls = controls,
      Gamma = matrix(c(0.9, 0.1, 0.1, 0.9), nrow = 2),
      mu = c(1, 5) 
    ),
    seed = 1
  )
  expect_error(
    fit_model(data, ncluster = 1.4)
  )
  expect_error(
    fit_model(data, verbose = "not_TRUE_or_FALSE")
  )
  model <- fit_model(data, ncluster = 1, seed = 1, verbose = FALSE)
  expect_s3_class(model, "fHMM_model")
  model_refit <- fit_model(
    data, ncluster = 1, verbose = FALSE, init = model$estimate
  )
  expect_s3_class(model_refit, "fHMM_model")
  controls <- list(
    states  = 2,
    sdds    = "gamma",
    horizon = 500,
    fit     = list("runs" = 10, "origin" = TRUE)
  )
  controls <- set_controls(controls)
  data <- prepare_data(controls, seed = 1)
  model_origin <- fit_model(data, ncluster = 1, seed = 1, verbose = FALSE)
  expect_s3_class(model_origin, "fHMM_model")
})

test_that("HHMM fitting works", {
  skip_on_cran()
  controls <- list(
    hierarchy = TRUE,
    states  = c(2, 2),
    sdds    = c("normal", "normal"),
    horizon = c(50, 30),
    fit     = list("runs" = 3, "print.level" = 2)
  )
  controls <- set_controls(controls)
  data <- prepare_data(
    controls, 
    true_parameters = fHMM_parameters(controls = controls, seed = 1),
    seed = 1
  )
  model <- fit_model(data, ncluster = 2, seed = 1, verbose = FALSE)
  expect_s3_class(model, "fHMM_model")
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
    fit     = list("runs" = 10, "accept" = "all")
  )
  controls <- set_controls(controls)
  data <- prepare_data(controls, seed = 1)
  expect_s3_class(
    fit_model(data, ncluster = 2, verbose = FALSE, seed = 1), 
    "fHMM_model"
  )
})