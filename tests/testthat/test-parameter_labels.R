test_that("input checks work", {
  expect_error(
    parameter_labels("controls"),
    "'controls' is not of class 'fHMM_controls'."
  )
  expect_error(
    parameter_labels(set_controls(), pi),
    "'expected_length' must be a positive integer."
  )
})

test_that("creation of HMM parameter labels works", {
  expect_equal(
    parameter_labels(set_controls(), 6),
    c("Gamma_2.1", "Gamma_1.2", "mu_1", "mu_2", "sigma_1", "sigma_2")
  )
})

test_that("creation of HHMM parameter labels works", {
  expect_equal(
    parameter_labels(
      set_controls(
        hierarchy = TRUE,
        sdds = c("t", "t")
      ), 
      expected_length = 24
    ),
    c("Gamma_2.1", "Gamma_1.2", "mu_1", "mu_2", "sigma_1", "sigma_2", 
      "df_1", "df_2", "Gamma*1_2.1", "Gamma*1_1.2", "mu*1_1", "mu*1_2", 
      "sigma*1_1", "sigma*1_2", "df*1_1", "df*1_2", "Gamma*2_2.1", 
      "Gamma*2_1.2", "mu*2_1", "mu*2_2", "sigma*2_1", "sigma*2_2", 
      "df*2_1", "df*2_2")
  )
})
