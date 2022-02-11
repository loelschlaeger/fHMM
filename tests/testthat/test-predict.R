test_that("prediction works", {
  data(dax_model)
  expect_snapshot(predict(dax_model))
})
