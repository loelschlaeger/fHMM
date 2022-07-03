test_that("prediction works", {
  data("dax_model_3t")
  expect_snapshot(predict(dax_model_3t))
})
