test_that("state decoding works", {
  data("dax_model_3t")
  x <- decode_states(dax_model_3t, verbose = FALSE)
  expect_snapshot(x)
  expect_snapshot(unclass(x))
})
