test_that("coefficients can be extracted", {
  expect_true(is.data.frame(coef(dax_model_2n)))
  expect_true(is.data.frame(coef(sim_model_2gamma)))
})

test_that("AIC can be computed", {
  expect_equal(round(AIC(dax_model_2n)), -34795)
  expect_equal(round(AIC(dax_model_2n, dax_model_3t)), c(-34795, -35270))
})

test_that("BIC can be computed", {
  expect_equal(round(BIC(dax_model_2n)), -34755)
  expect_equal(round(BIC(dax_model_2n, dax_model_3t)), c(-34755, -35170))
})

test_that("number of observations can be computed", {
  expect_equal(nobs(dax_model_2n), 5882)
})

test_that("log-likelihood can be computed", {
  expect_equal(round(logLik(dax_model_2n)), 17404)
})

test_that("number of model parameters can be computed", {
  expect_equal(npar(dax_model_2n), 6)
  expect_equal(npar(dax_model_2n, dax_model_3t), c(6, 15))
})

test_that("input checks for prediction work", {
  expect_error(
    predict.fHMM_model("1"),
    "'object' must be of class 'fHMM_model'."
  )
  expect_error(
    predict.fHMM_model(dax_model_3t, ahead = -1),
    "'ahead' must be a positive integer."
  )
  expect_error(
    predict.fHMM_model(dax_model_3t, ahead = 5, alpha = 1.1),
    "'alpha' must be a numeric between 0 and 1."
  )
})

test_that("prediction works", {
  prediction <- predict(dax_model_3t)
  expect_s3_class(prediction, "fHMM_predict")
})

