test_that("simulation of data works", {
  controls = list(
    states  = 2,
    sdds    = "t",
    horizon = 400,
    fit     = list("runs" = 50)
  )
  controls = set_controls(controls)
  true_parameters = set_parameters(
    controls, Gamma = matrix(c(0.8,0.1,0.2,0.9),2,2), mus = c(-1,1), 
    sigmas = c(0.5,2), dfs = c(1,Inf), seed = 1
  )
  expect_snapshot(simulate_data(controls, true_parameters, seed = 1))
  controls = list(
    hierarchy = TRUE,
    horizon = c(100, 30)
  )
  controls = set_controls(controls)
  true_parameters = set_parameters(controls, seed = 1)
  expect_snapshot(simulate_data(controls, true_parameters, seed = 1))
  controls = list(
    hierarchy = TRUE,
    horizon = c(100, NA),
    period = "y"
  )
  controls = set_controls(controls)
  true_parameters = set_parameters(controls, seed = 1)
  expect_snapshot(simulate_data(controls, true_parameters, seed = 1))
})
