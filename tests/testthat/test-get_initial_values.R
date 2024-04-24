test_that("get initial values for HMM works", {
  controls <- set_controls(states = 2, sdds = "normal", horizon = 80, runs = 10)
  parameters <- fHMM_parameters(controls)
  data <- prepare_data(controls, true_parameter = parameters)
  initials <- get_initial_values(data = data, verbose = FALSE)
  checkmate::expect_list(initials, len = 10)
  initials <- get_initial_values(
    data = data, verbose = FALSE, initial_estimate = 1:6
  )
  checkmate::expect_list(initials, len = 10)
  controls <- set_controls(
    states = 2, sdds = "normal", horizon = 80, origin = TRUE
  )
  data <- prepare_data(controls, true_parameter = parameters)
  initials <- get_initial_values(data = data, verbose = FALSE)
  checkmate::expect_list(initials, len = 1)
})

test_that("get initial values for HMM works in parallel", {
  skip_on_cran()
  controls <- set_controls(states = 2, sdds = "normal", horizon = 80, runs = 10)
  parameters <- fHMM_parameters(controls)
  data <- prepare_data(controls, true_parameter = parameters)
  initials <- get_initial_values(data = data, ncluster = 2, verbose = FALSE)
  checkmate::expect_list(initials, len = 10)
})

test_that("get initial values for HHMM works", {
  controls <- set_controls(
    states = c(3, 2), sdds = c("gamma", "poisson"), hierarchy = TRUE, 
    horizon = c(100, 50), runs = 10
  )
  parameters <- fHMM_parameters(controls)
  data <- prepare_data(controls, true_parameter = parameters)
  initials <- get_initial_values(data = data, verbose = FALSE)
  checkmate::expect_list(initials, len = 10)
  initials <- get_initial_values(
    data = data, verbose = FALSE, initial_estimate = rep(1, 24)
  )
  checkmate::expect_list(initials, len = 10)
  controls <- set_controls(states = c(2, 2), hierarchy = TRUE, origin = TRUE)
  parameters <- fHMM_parameters(controls)
  data <- prepare_data(controls, true_parameter = parameters)
  initials <- get_initial_values(data = data, verbose = FALSE)
  checkmate::expect_list(initials, len = 1)
})

test_that("get initial values for HHMM works in parallel", {
  skip_on_cran()
  controls <- set_controls(
    states = c(2, 3), sdds = c("t", "t"), hierarchy = TRUE, 
    horizon = c(100, 30), runs = 10
  )
  parameters <- fHMM_parameters(controls)
  data <- prepare_data(controls, true_parameter = parameters)
  initials <- get_initial_values(data = data, ncluster = 2, verbose = FALSE)
  checkmate::expect_list(initials, len = 10)
})

