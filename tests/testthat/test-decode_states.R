test_that("state decoding works", {
  data(dax_model)
  x <- decode_states(dax_model, verbose = FALSE)
  expect_snapshot(x)
  expect_snapshot(unclass(x))
})
