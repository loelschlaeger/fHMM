test_that("residual computation works", {
  expect_error(compute_residuals("not_an_fHMM_model"))
  expect_message(compute_residuals(dax_model_3t, verbose = TRUE))
  expect_error(compute_residuals(dax_model_3t, verbose = "not_TRUE_or_FALSE"))
  dax_model_3t_tmp <- dax_model_3t
  dax_model_3t_tmp$decoding <- NULL
  expect_warning(compute_residuals(dax_model_3t_tmp))
  x <- compute_residuals(dax_model_3t, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_s3_class(x$residuals, "fHMM_residuals")
  expect_equal(
    round(fivenum(x$residuals), 2), 
    c(-3.52, -0.66, 0.01, 0.67, 3.69)
  )
  sim_model_2gamma <- decode_states(sim_model_2gamma, verbose = FALSE)
  x <- compute_residuals(sim_model_2gamma, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_s3_class(x$residuals, "fHMM_residuals")
  dax_vw_model <- decode_states(dax_vw_model, verbose = FALSE)
  x <- compute_residuals(dax_vw_model, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_s3_class(x$residuals, "fHMM_residuals")
})
