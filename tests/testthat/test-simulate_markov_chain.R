test_that("simulation of Markov chain works", {
  Gamma <- rbind(c(0.8, 0.2), c(0.1, 0.9))
  expect_snapshot(simulate_markov_chain(Gamma = Gamma, T = 100, seed = 1))
})
