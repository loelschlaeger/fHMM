test_that("model comparison works", {
  comparison <- compare_models(dax_model_2n, dax_model_3t)
  expect_s3_class(comparison, "data.frame")
  expect_named(comparison, c("parameters", "loglikelihood", "AIC", "BIC"))
  expect_equal(
    round(unlist(comparison)),
    c(parameters1 = 6, parameters2 = 15, 
      loglikelihood1 = 16682, loglikelihood2 = 16913, 
      AIC1 = -33352, AIC2 = -33797, 
      BIC1 = -33312, BIC2 = -33697
    )           
  )
  expect_error(compare_models("not_an_fHMM_model"))
  expect_warning(
    compare_models(dax_model_2n, sim_model_2gamma),
    "are not estimated on the same data"
  )
})
