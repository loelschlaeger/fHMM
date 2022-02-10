test_that("model comparison works", {
  data(dax_model)
  expect_snapshot(compare_models(dax_model))
})
