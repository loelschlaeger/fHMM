test_that("state decoding input checks work", {
  expect_error(decode_states("not_an_fHMM_model_object"))
  expect_error(decode_states(dax_model_3t, "not_TRUE_or_FALSE"))
  expect_message(decode_states(dax_model_3t, TRUE))
})

test_that("state decoding for normal distribution works", {
  x <- decode_states(dax_model_2n, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_equal(fivenum(x$decoding), c(1, 1, 2, 2, 2))
})

test_that("state decoding for t distribution works", {
  x <- decode_states(dax_model_3t, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_equal(fivenum(x$decoding), c(1, 2, 2, 3, 3))
})

test_that("state decoding for gamma distribution works", {
  x <- decode_states(sim_model_2gamma, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_equal(fivenum(x$decoding), c(1, 1, 1, 2, 2))
})

test_that("state decoding for hierarchical case works", {
  x <- decode_states(dax_vw_model, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_equal(dim(x$decoding), c(157L, 24L))
  expect_equal(fivenum(x$decoding), c(1, 1, 2, 2, 2))
})
