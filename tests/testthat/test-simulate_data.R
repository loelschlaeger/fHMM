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
