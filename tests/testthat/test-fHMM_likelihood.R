test_that("HMM likelihood can be computed", {
  controls <- set_controls(states = 2, sdds = "normal")
  parameters <- fHMM_parameters(controls)
  parUncon <- par2parUncon(parameters, controls)
  observations <- 1:10
  checkmate::expect_number(
    ll_hmm(parUncon, observations, controls) 
  )
})

