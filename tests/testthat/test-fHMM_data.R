test_that("input checks work", {
  expect_error(
    prepare_data("controls"),
    "'controls' is not of class 'fHMM_controls'."
  )
  expect_error(
    prepare_data(set_controls(), true_parameters = "true"),
    "'true_parameters' is not of class 'fHMM_parameters'."
  )
})

test_that("data preparation for simulated HMM works", {
  controls <- set_controls()
  out <- prepare_data(controls, seed = 1)
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})

test_that("data preparation for empirical HMM works", {
  controls <- set_controls(list(data = list(file = dax)))
  out <- prepare_data(controls, seed = 1)
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})

test_that("data preparation for empirical HHMM works", {
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "w",
    data = list(
      file = c(system.file("extdata", "dax.csv", package = "fHMM"),
               system.file("extdata", "dax.csv", package = "fHMM")),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close")
    )
  )
  controls <- set_controls(controls)
  out <- prepare_data(controls, seed = 1)
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, 10),
    data = list(
      file = c(system.file("extdata", "dax.csv", package = "fHMM"),
               system.file("extdata", "dax.csv", package = "fHMM")),
      date_column = c("Date", "Date"),
      data_column = c("Close", "Close")
    )
  )
  controls <- set_controls(controls)
  out <- prepare_data(controls, seed = 1)
  expect_s3_class(out, "fHMM_data")
  expect_snapshot(print(out))
  expect_snapshot(summary(out))
})

test_that("read of data works", {
  file <- paste0(tempdir(), "/test.csv")
  data <- data.frame("a" = c("2020-01-01", "2020-01-02"), "b" = 1:2)
  write.csv(data, file)
  controls <- list(
    data = list(
      file = file,
      date_column = "a",
      data_column = "b"
    )
  )
  controls <- set_controls(controls)
  out <- prepare_data(controls)
  expect_equal(out$dates, data$a)
  expect_equal(out$time_points, NA)
  expect_null(out$markov_chain)
  expect_equal(out$data, data$b)
  expect_equal(out$time_series, data$b)
  expect_null(out$T_star)
  expect_equal(out$controls, controls)
})

test_that("input checks for simulation of data works", {
  expect_error(simulate_data(controls = "not_a_controls_object"))
  controls <- set_controls()
  expect_error(
    simulate_data(
      controls = controls, 
      true_parameter = "not_an_fHMM_parameter_object"
    )
  )
  controls$simulated <- FALSE
  expect_error(
    simulate_data(
      controls = controls,
      true_parameters = fHMM_parameters(controls)
    )
  )
})

test_that("simulation of data works", {
  controls <- list(
    states  = 2,
    sdds    = "t",
    horizon = 400,
    fit     = list("runs" = 50)
  )
  controls <- set_controls(controls)
  true_parameters <- fHMM_parameters(
    controls,
    Gamma = matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2), mus = c(-1, 1),
    sigmas = c(0.5, 2), dfs = c(1, Inf), seed = 1
  )
  data <- simulate_data(controls, true_parameters, seed = 1)
  expect_type(data, "list")
  expect_named(data, c("time_points", "markov_chain", "data", "T_star"))
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, 30)
  )
  controls <- set_controls(controls)
  true_parameters <- fHMM_parameters(controls, seed = 1)
  data <- simulate_data(controls, true_parameters, seed = 1)
  expect_type(data, "list")
  expect_named(data, c("time_points", "markov_chain", "data", "T_star"))
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "y"
  )
  controls <- set_controls(controls)
  true_parameters <- fHMM_parameters(controls, seed = 1)
  data <- simulate_data(controls, true_parameters, seed = 1)
  expect_type(data, "list")
  expect_named(data, c("time_points", "markov_chain", "data", "T_star"))
})

test_that("input checks for simulation of state-dependent observations works", {
  expect_error(simulate_observations(markov_chain = c(-1, 1.1)))
  markov_chain <- rep(1:2, each = 10)
  expect_error(
    simulate_observations(
      markov_chain = markov_chain,
      sdd = "not_a_valid_sdd"
    )
  )
  expect_error(
    simulate_observations(
      markov_chain = markov_chain,
      sdd = "t",
      mus = "not_a_numeric"
    )
  )
  expect_error(
    simulate_observations(
      markov_chain = markov_chain,
      sdd = "t",
      mus = c(-1, 1),
      sigmas = c(-1, 1)
    )
  )
  expect_error(
    simulate_observations(
      markov_chain = markov_chain,
      sdd = "t",
      mus = c(-1, 1),
      sigmas = c(1, 2, 3)
    )
  )
  expect_error(
    simulate_observations(
      markov_chain = markov_chain,
      sdd = "gamma",
      mus = c(-1, 1),
      sigmas = c(1, 2),
      dfs = c(1, 2)
    )
  )
  expect_error(
    simulate_observations(
      markov_chain = markov_chain,
      sdd = "t",
      mus = c(-1, 1),
      sigmas = c(1, 2),
      dfs = NULL
    )
  )
  expect_error(
    simulate_observations(
      markov_chain = markov_chain,
      sdd = "t",
      mus = c(-1, 1),
      sigmas = c(1, 2),
      dfs = c(-1, 1)
    )
  )
  expect_error(
    simulate_observations(
      markov_chain = markov_chain,
      sdd = "t",
      mus = c(-1, 1),
      sigmas = c(1, 2),
      dfs = c(1, 2, 3)
    )
  )
  expect_error(
    simulate_observations(
      markov_chain = markov_chain,
      sdd = "t",
      mus = c(-1, 1),
      sigmas = c(1, 2),
      dfs = c(1, 2),
      total_length = 9
    )
  )
})

test_that("simulation of state-dependent observations works", {
  Gamma <- rbind(c(0.8, 0.2), c(0.1, 0.9))
  markov_chain <- simulate_markov_chain(Gamma = Gamma, T = 10)
  observations <- simulate_observations(
    markov_chain = markov_chain, sdd = "t", mus = c(1, 2),
    sigmas = c(0.5, 2), dfs = c(1, Inf), seed = 1
  )
  expect_type(observations, "double")
  expect_length(observations, 10)
  observations <- simulate_observations(
    markov_chain = markov_chain, sdd = "gamma", mus = c(1, 2),
    sigmas = c(0.5, 2), seed = 1
  )
  expect_type(observations, "double")
  expect_length(observations, 10)
  observations <- simulate_observations(
    markov_chain = markov_chain, sdd = "lnorm", mus = c(1, 2),
    sigmas = c(0.5, 2), seed = 1
  )
  expect_type(observations, "double")
  expect_length(observations, 10)
})


