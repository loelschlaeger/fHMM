test_that("get initial values for HMM works", {
  
  controls <- set_controls(states = 2, sdds = "normal", horizon = 80, runs = 5)
  parameters <- fHMM_parameters(controls, mu = c(-1, 1), seed = 1)
  data <- prepare_data(controls, true_parameter = parameters, seed = 1)
  
  
  
  ncluster <- 1
  seed <- NULL
  verbose <- TRUE
  initial_estimate <- NULL
  
  get_initial_values(data = data, ncluster = 1, seed = 2, verbose = TRUE)
  
  
})

test_that("get initial values for HHMM works", {
  
  
  
})

