test_that("get initial values for HMM works", {
  seed <- 1
  controls <- set_controls(states = 2, sdds = "normal", horizon = 80, runs = 5)
  parameters <- fHMM_parameters(controls, mu = c(-1, 1), seed = seed)
  data <- prepare_data(controls, true_parameter = parameters, seed = seed)
  initials <- get_initial_values(data = data, seed = seed, verbose = FALSE)
  checkmate::expect_list(initials, len = 5)
  initials <- get_initial_values(
    data = data, seed = seed, verbose = FALSE, initial_estimate = 1:6
  )
  checkmate::expect_list(initials, len = 5)
  controls <- set_controls(
    states = 2, sdds = "normal", horizon = 80, origin = TRUE
  )
  data <- prepare_data(controls, true_parameter = parameters, seed = seed)
  initials <- get_initial_values(data = data, seed = seed, verbose = FALSE)
  checkmate::expect_list(initials, len = 1)
})

test_that("get initial values for HHMM works", {
  seed <- 1
  controls <- set_controls(
    states = c(3, 2), sdds = c("gamma", "poisson"), hierarchy = TRUE, 
    horizon = c(100, 100), runs = 5
  )
  parameters <- fHMM_parameters(controls, seed = seed)
  data <- prepare_data(controls, true_parameter = parameters, seed = seed)
  initials <- get_initial_values(data = data, seed = seed, verbose = FALSE)
  checkmate::expect_list(initials, len = 5)
  initials <- get_initial_values(
    data = data, seed = seed, verbose = FALSE, initial_estimate = rep(1, 24)
  )
  checkmate::expect_list(initials, len = 5)
  controls <- set_controls(states = c(2, 2), hierarchy = TRUE, origin = TRUE)
  parameters <- fHMM_parameters(controls, seed = seed)
  data <- prepare_data(controls, true_parameter = parameters, seed = seed)
  initials <- get_initial_values(data = data, seed = seed, verbose = FALSE)
  checkmate::expect_list(initials, len = 1)
})

