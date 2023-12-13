test_that("input checks for simulation of data works", {
  expect_error(simulate_hmm(controls = "not_a_controls_object"))
  controls <- set_controls()
  expect_error(
    simulate_hmm(
      controls = controls, 
      true_parameter = "not_an_fHMM_parameter_object"
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
    Gamma = matrix(c(0.8, 0.1, 0.2, 0.9), 2, 2), 
    mu = c(-1, 1),
    sigma = c(0.5, 2), 
    df = c(1, Inf), 
    seed = 1
  )
  data <- simulate_hmm(controls, true_parameters, seed = 1)
  expect_type(data, "list")
  expect_named(data, c("time_points", "markov_chain", "data", "T_star"))
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, 30)
  )
  controls <- set_controls(controls)
  true_parameters <- fHMM_parameters(controls, seed = 1)
  data <- simulate_hmm(controls, true_parameters, seed = 1)
  expect_type(data, "list")
  expect_named(data, c("time_points", "markov_chain", "data", "T_star"))
  controls <- list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "y"
  )
  controls <- set_controls(controls)
  true_parameters <- fHMM_parameters(controls, seed = 1)
  data <- simulate_hmm(controls, true_parameters, seed = 1)
  expect_type(data, "list")
  expect_named(data, c("time_points", "markov_chain", "data", "T_star"))
})
