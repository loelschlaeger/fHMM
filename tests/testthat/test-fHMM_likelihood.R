test_that("HMM likelihood can be computed", {
  controls <- set_controls(states = 2, sdds = "normal")
  parameters <- fHMM_parameters(controls)
  parUncon <- par2parUncon(parameters, controls)
  observations <- 1:10
  checkmate::expect_number(
    ll_hmm(parUncon, observations, controls) 
  )
})

test_that("HHMM likelihood can be computed", {
  controls <- set_controls(
    hierarchy = TRUE, states = c(4, 3), sdds = c("gamma", "t")
  )
  parameters <- fHMM_parameters(controls)
  parUncon <- par2parUncon(parameters, controls)
  observations <- list(
    1:10,                                  # fine-scale data
    replicate(10, 1:10, simplify = FALSE)  # coarse-scale data
  )
  checkmate::expect_number(
    ll_hmm(parUncon, observations, controls) 
  )
})
