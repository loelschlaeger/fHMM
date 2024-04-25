test_that("model comparison works", {
  comparison <- compare_models(dax_model_2n, dax_model_3t)
  expect_s3_class(comparison, "data.frame")
  expect_named(comparison, c("parameters", "loglikelihood", "AIC", "BIC"))
  values <- round(unlist(comparison))
  expect_equal(
    values,
    c(parameters1 = 6, parameters2 = 15, loglikelihood1 = 17404, 
      loglikelihood2 = 17650, AIC1 = -34795, AIC2 = -35270, BIC1 = -34755, 
      BIC2 = -35170)         
  )
  expect_error(
    compare_models("not_an_fHMM_model"),
    "Input 'not_an_fHMM_model' is not of class 'fHMM_model'."
  )
  expect_warning(
    compare_models(dax_model_2n, sim_model_2gamma),
    "are not estimated on the same data"
  )
})
