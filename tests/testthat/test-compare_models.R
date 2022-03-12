test_that("model comparison works", {
  data(dax_model_2n)
  data(dax_model_3t)
  expect_snapshot(compare_models(dax_model_2n,dax_model_3t))
})
