test_that("parameter transformations for HMM work", {
  ### no fixed parameters
  controls = set_controls()
  par = fHMM_parameters(controls)
  parUncon = par2parUncon(par, controls)
  expect_snapshot(parUncon)
  parCon = parUncon2parCon(parUncon, controls)
  expect_snapshot(parCon)
  par2 = parCon2par(parCon, controls)
  expect_snapshot(par2)
  expect_equal(par, par2)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par,controls),controls),controls))
  ### fixed mu
  controls = set_controls(list("sdds" = "t(mu = 1)"))
  par = fHMM_parameters(controls)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par,controls),controls),controls))
  ### fixed sigma
  controls = set_controls(list("sdds" = "gamma(sigma = 1)"))
  par = fHMM_parameters(controls)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par,controls),controls),controls))
  ### fixed df
  controls = set_controls(list("sdds" = "t(df = Inf)"))
  par = fHMM_parameters(controls)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par,controls),controls),controls))
})

test_that("parameter transformations for HHMM work", {
  ### no fixed parameters
  controls = set_controls(list("hierarchy" = TRUE))
  par = fHMM_parameters(controls)
  parUncon = par2parUncon(par, controls)
  expect_snapshot(parUncon)
  parCon = parUncon2parCon(parUncon, controls)
  expect_snapshot(parCon)
  par2 = parCon2par(parCon, controls)
  expect_snapshot(par2)
  expect_equal(par, par2)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par,controls),controls),controls))
  ### fixed mu
  controls = set_controls(list("hierarchy" = TRUE,
                               "sdds" = c("t(mu = 1)","t")))
  par = fHMM_parameters(controls)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par,controls),controls),controls))
  ### fixed sigma
  controls = set_controls(list("hierarchy" = TRUE,
                               "sdds" = c("gamma","gamma(sigma = 1)")))
  par = fHMM_parameters(controls)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par,controls),controls),controls))
  ### fixed df
  controls = set_controls(list("hierarchy" = TRUE,
                               "sdds" = c("gamma","t(df = 5)")))
  par = fHMM_parameters(controls)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par,controls),controls),controls))
})

test_that("mu transformations work", {
  size = sample(10,1)
  muUncon = rnorm(size)
  link = sample(c(TRUE,FALSE),1)
  expect_equal(muUncon,
               muCon2muUncon(muUncon2muCon(muUncon, link = link), link = link))
  muCon = abs(rnorm(size))
  expect_equal(muCon,
               muUncon2muCon(muCon2muUncon(muCon, link = link), link = link))
})

test_that("sigma transformations work", {
  size = sample(10,1)
  sigmaUncon = rnorm(size)
  expect_equal(sigmaUncon,
               sigmaCon2sigmaUncon(sigmaUncon2sigmaCon(sigmaUncon)))
  sigmaCon = abs(rnorm(size))
  expect_equal(sigmaCon,
               sigmaUncon2sigmaCon(sigmaCon2sigmaUncon(sigmaCon)))
})

test_that("df transformations work", {
  size = sample(10,1)
  dfUncon = rnorm(size)
  expect_equal(dfUncon,
               dfCon2dfUncon(dfUncon2dfCon(dfUncon)))
  dfCon = abs(rnorm(size))
  expect_equal(dfCon,
               dfUncon2dfCon(dfCon2dfUncon(dfCon)))
})

test_that("Gamma transformations work", {
  dim = sample(10,1)
  Gamma = matrix(runif(dim^2),dim,dim)
  gammasUncon = Gamma[Gamma != diag(Gamma)]
  Gamma = Gamma / rowSums(Gamma)
  gammasCon = Gamma[Gamma != diag(Gamma)]
  expect_equal(Gamma, gammasCon2Gamma(Gamma2gammasCon(Gamma), dim = dim))
  expect_equal(gammasCon, Gamma2gammasCon(gammasCon2Gamma(gammasCon, dim = dim)))
  expect_equal(Gamma, gammasUncon2Gamma(Gamma2gammasUncon(Gamma), dim = dim))
  expect_equal(gammasUncon, Gamma2gammasUncon(gammasUncon2Gamma(gammasUncon, dim = dim)))
  expect_equal(gammasCon, gammasUncon2gammasCon(gammasCon2gammasUncon(gammasCon, dim = dim), dim = dim))
  expect_equal(gammasUncon, gammasCon2gammasUncon(gammasUncon2gammasCon(gammasUncon, dim = dim), dim = dim))
  expect_equal(Gamma, gammasUncon2Gamma(gammasCon2gammasUncon(Gamma2gammasCon(Gamma), dim = dim), dim = dim))
  expect_equal(gammasCon, Gamma2gammasCon(gammasUncon2Gamma(gammasCon2gammasUncon(gammasCon, dim = dim), dim = dim)))
  expect_equal(gammasUncon, gammasCon2gammasUncon(Gamma2gammasCon(gammasUncon2Gamma(gammasUncon, dim = dim)), dim = dim))
  Gamma2gammasUncon(Gamma)
  delta = Gamma2delta(Gamma)
  expect_equal(delta, as.numeric(delta %*% Gamma))
  expect_warning(Gamma2delta(diag(3)))
})
  