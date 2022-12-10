test_that("creation of parameter labels works", {
  expect_equal(
    parameter_labels(set_controls(), 8),
    c("Gamma_2.1", "Gamma_1.2", "mu_1", "mu_2", "sigma_1", "sigma_2", 
      "df_1", "df_2")
  )
})
