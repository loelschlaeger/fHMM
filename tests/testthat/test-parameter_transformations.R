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
  