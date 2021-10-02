test_that("simulation of state-dependent observations works", {
  Gamma = rbind(c(0.8,0.2),c(0.1,0.9))
  markov_chain = simulate_markov_chain(Gamma = Gamma, T = 10)
  expect_snapshot(
    simulate_observations(markov_chain = markov_chain, sdd = "t", mus = c(-1,1),
                          sigmas = c(0.5,2), dfs = c(1,Inf), seed = 1)
  )
  expect_error(
    simulate_observations(markov_chain = markov_chain, sdd = "t", mus = c(-1,1),
                          sigmas = c(0.5,2), dfs = c(1,Inf), seed = 1,
                          total_length = 9)
  )
})
