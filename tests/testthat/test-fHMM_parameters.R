test_that("input checks for parameter transformations work", {
  expect_error(
    fHMM_parameters("not_a_controls_object"),
    "'controls' must be a list or an object of class 'fHMM_controls'"
  )
  expect_error(
    fHMM_parameters(scale = c(-1, 0)),
    "'scale_par' must be a positive numeric vector of length 2."
  )
  expect_error(
    fHMM_parameters(Gamma = matrix(c(1:4), 2, 2)),
    "Assertion on 'Gamma' failed: Must have values between 0 and 1."
  )
  expect_error(
    fHMM_parameters(mu = c("1", "2")),
    "'mu' must be a numeric vector of length 2"
  )
  expect_error(
    fHMM_parameters(sigma = c(-1, -2)),
    "'sigma' must be a positive numeric vector of length 2"
  )
  expect_error(
    fHMM_parameters(sdds = "t", df = c(-1, -2)),
    "'df' must be a positive numeric vector of length 2"
  )
  expect_error(
    fHMM_parameters(sdds = "gamma", mu = c(-1, -2)),
    "'mu' must be a positive numeric vector of length 2"
  )
  expect_error(
    fHMM_parameters(hierarchy = TRUE, Gamma_star = matrix(1:4,2,2)),
    "'Gamma_star' must be a list of length 2"
  )
  expect_error(
    fHMM_parameters(hierarchy = TRUE, Gamma_star = list(matrix(1:4,2,2), matrix(1:4,2,2))),
    "Must have values between 0 and 1."
  )
  expect_error(
    fHMM_parameters(hierarchy = TRUE, mu_star = c("1", "2")),
    "'mu_star' must be a list of length 2"
  )
  expect_error(
    fHMM_parameters(hierarchy = TRUE, mu_star = list(c(1, 2), c("1", "2"))),
    "Element 2 in 'mu_star' must be a numeric vector of length 2"
  )
  expect_error(
    fHMM_parameters(hierarchy = TRUE, sigma_star = c(-1, -2)),
    "'sigma_star' must be a list of length 2"
  )
  expect_error(
    fHMM_parameters(hierarchy = TRUE, sigma_star = list(c(-1, -2), c(-2, -2))),
    "Element 1 in 'sigma_star' must be a positive numeric vector of length 2"
  )
  expect_error(
    fHMM_parameters(hierarchy = TRUE, sdds = c("t", "t"), df_star = c(-1, -2)),
    "'df_star' must be a list of length 2"
  )
  expect_error(
    fHMM_parameters(hierarchy = TRUE, sdds = c("t", "t"), df_star = list(c(1, 1), c(1,-1))),
    "Element 2 in 'df_star' must be a positive numeric vector of length 2"
  )
  expect_error(
    fHMM_parameters(hierarchy = TRUE, sdds = c("gamma", "gamma"), mu_star = list(c(1, 1), c(1,-1))),
    "Element 2 in 'mu_star' must be a positive numeric vector of length 2"
  )
  
})

test_that("parameter printing works", {
  sink(tempfile())
  expect_s3_class(print(fHMM_parameters()), "fHMM_parameters")
  expect_snapshot(print(fHMM_parameters()))
  sink()
})

test_that("parameter transformations for HMM work", {
  ### no fixed parameters
  controls <- set_controls()
  par <- fHMM_parameters(controls, seed = 1)
  parUncon <- par2parUncon(par, controls)
  parCon <- parUncon2parCon(parUncon, controls)
  par2 <- parCon2par(parCon, controls)
  expect_equal(par, par2)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par, controls), controls), controls))
  ### fixed mu
  controls <- set_controls(list("sdds" = "t(mu = 1)"))
  par <- fHMM_parameters(controls, seed = 1)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par, controls), controls), controls))
  ### fixed sigma
  controls <- set_controls(list("sdds" = "gamma(sigma = 1)"))
  par <- fHMM_parameters(controls, seed = 1)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par, controls), controls), controls))
  ### fixed df
  controls <- set_controls(list("sdds" = "t(df = Inf)"))
  par <- fHMM_parameters(controls, seed = 1)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par, controls), controls), controls))
})

test_that("parameter transformations for HHMM work", {
  ### no fixed parameters
  controls <- set_controls(list("hierarchy" = TRUE))
  par <- fHMM_parameters(controls, seed = 1)
  parUncon <- par2parUncon(par, controls)
  parCon <- parUncon2parCon(parUncon, controls)
  par2 <- parCon2par(parCon, controls)
  expect_equal(par, par2)
  expect_equal(par, parUncon2par(parCon2parUncon(par2parCon(par, controls), controls), controls))
  ### fixed mu
  controls <- set_controls(list(
    "hierarchy" = TRUE,
    "sdds" = c("t(mu = 1)", "t(mu = 1)")
  ))
  par <- fHMM_parameters(controls, seed = 1)
  expect_equal(
    par, 
    parUncon2par(parCon2parUncon(par2parCon(par, controls), controls), controls)
  )
  ### fixed sigma
  controls <- set_controls(list(
    "hierarchy" = TRUE,
    "sdds" = c("gamma", "gamma(sigma = 1)")
  ))
  par <- fHMM_parameters(controls, seed = 1)
  expect_equal(
    par, 
    parUncon2par(parCon2parUncon(par2parCon(par, controls), controls), controls)
  )
  ### fixed df
  controls <- set_controls(list(
    "hierarchy" = TRUE,
    "sdds" = c("gamma", "t(mu = 1, df = 5)")
  ))
  par <- fHMM_parameters(controls, seed = 1)
  expect_equal(
    par, 
    parUncon2par(parCon2parUncon(par2parCon(par, controls), controls), controls)
  )
})

test_that("mu transformations work", {
  size <- sample(10, 1)
  muUncon <- rnorm(size)
  names(muUncon) <- paste0("muUncon_", 1:size)
  link <- sample(c(TRUE, FALSE), 1)
  expect_equal(
    muUncon,
    muCon2muUncon(muUncon2muCon(muUncon, link = link), link = link)
  )
  muCon <- abs(rnorm(size))
  names(muCon) <- paste0("muCon_", 1:size)
  expect_equal(
    muCon,
    muUncon2muCon(muCon2muUncon(muCon, link = link), link = link)
  )
})

test_that("sigma transformations work", {
  size <- sample(10, 1)
  sigmaUncon <- rnorm(size)
  names(sigmaUncon) <- paste0("sigmaUncon_", 1:size)
  expect_equal(
    sigmaUncon,
    sigmaCon2sigmaUncon(sigmaUncon2sigmaCon(sigmaUncon))
  )
  sigmaCon <- abs(rnorm(size))
  names(sigmaCon) <- paste0("sigmaCon_", 1:size)
  expect_equal(
    sigmaCon,
    sigmaUncon2sigmaCon(sigmaCon2sigmaUncon(sigmaCon))
  )
})

test_that("df transformations work", {
  size <- sample(10, 1)
  dfUncon <- rnorm(size)
  names(dfUncon) <- paste0("dfUncon_", 1:size)
  expect_equal(
    dfUncon,
    dfCon2dfUncon(dfUncon2dfCon(dfUncon))
  )
  dfCon <- abs(rnorm(size))
  names(dfCon) <- paste0("dfCon_", 1:size)
  expect_equal(
    dfCon,
    dfUncon2dfCon(dfCon2dfUncon(dfCon))
  )
})

test_that("Gamma transformations work", {
  dim <- sample(2:10, 1)
  Gamma <- matrix(runif(dim^2), dim, dim)
  gammasUncon <- Gamma[Gamma != diag(Gamma)]
  names(gammasUncon) <- oeli::matrix_indices(
    Gamma, prefix = "gammasUncon_", exclude_diagonal = TRUE
  )
  Gamma <- Gamma / rowSums(Gamma)
  gammasCon <- Gamma[Gamma != diag(Gamma)]
  names(gammasCon) <- oeli::matrix_indices(
    Gamma, prefix = "gammasCon_", exclude_diagonal = TRUE
  )
  colnames(Gamma) <- rownames(Gamma) <- paste0("state_", 1:dim)
  expect_equal(Gamma, gammasCon2Gamma(Gamma2gammasCon(Gamma), dim = dim))
  expect_equal(gammasCon, Gamma2gammasCon(gammasCon2Gamma(gammasCon, dim = dim)))
  expect_equal(Gamma, gammasUncon2Gamma(Gamma2gammasUncon(Gamma), dim = dim))
  expect_equal(gammasUncon, Gamma2gammasUncon(gammasUncon2Gamma(gammasUncon, dim = dim)))
  expect_equal(gammasCon, gammasUncon2gammasCon(gammasCon2gammasUncon(gammasCon, dim = dim), dim = dim))
  expect_equal(gammasUncon, gammasCon2gammasUncon(gammasUncon2gammasCon(gammasUncon, dim = dim), dim = dim))
  expect_equal(Gamma, gammasUncon2Gamma(gammasCon2gammasUncon(Gamma2gammasCon(Gamma), dim = dim), dim = dim))
  expect_equal(gammasCon, Gamma2gammasCon(gammasUncon2Gamma(gammasCon2gammasUncon(gammasCon, dim = dim), dim = dim)))
  expect_equal(gammasUncon, gammasCon2gammasUncon(Gamma2gammasCon(gammasUncon2Gamma(gammasUncon, dim = dim)), dim = dim))
  delta <- oeli::stationary_distribution(Gamma)
  expect_equal(unname(delta), as.numeric(delta %*% Gamma))
})
