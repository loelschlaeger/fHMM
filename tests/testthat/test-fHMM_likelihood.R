test_that("HMM log-likelihood can be computed", {
  controls <- set_controls(states = 2, sdds = "poisson")
  parameters <- fHMM_parameters(controls)
  parUncon <- par2parUncon(parameters, controls)
  observations <- 1:10
  checkmate::expect_number(
    ll_hmm(parUncon, observations, controls) 
  )
  checkmate::expect_number(
    ll_hmm(parUncon, observations, controls, negative = TRUE) 
  )
  checkmate::expect_number(
    ll_hmm(parUncon, observations, controls, check_controls = FALSE) 
  )
})

test_that("HHMM log-likelihood can be computed", {
  controls <- set_controls(
    hierarchy = TRUE, states = c(2, 2), sdds = c("t", "normal")
  )
  parameters <- fHMM_parameters(controls)
  parUncon <- par2parUncon(parameters, controls)
  observations <- matrix(stats::rnorm(110), ncol = 11, nrow = 10)
  checkmate::expect_number(
    ll_hmm(parUncon, observations, controls)
  )
  checkmate::expect_number(
    ll_hmm(parUncon, observations, controls, negative = TRUE) 
  )
  checkmate::expect_number(
    ll_hmm(parUncon, observations, controls, check_controls = FALSE) 
  )
})
