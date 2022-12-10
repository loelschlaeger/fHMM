test_that("state decoding input checks work", {
  expect_error(decode_states("not_an_fHMM_model_object"))
  data("dax_model_3t")
  expect_error(decode_states(dax_model_3t, "not_TRUE_or_FALSE"))
  expect_message(decode_states(dax_model_3t, TRUE))
})

test_that("state decoding for normal distribution works", {
  data("dax_model_2n")
  x <- decode_states(dax_model_2n, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_equal(fivenum(x$decoding), c(1,1,1,2,2))
})

test_that("state decoding for t distribution works", {
  data("dax_model_3t")
  x <- decode_states(dax_model_3t, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_equal(fivenum(x$decoding), c(1,1,2,2,3))
})

test_that("state decoding for gamma distribution works", {
  data("sim_model_2gamma")
  x <- decode_states(sim_model_2gamma, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_equal(fivenum(x$decoding), c(1,1,1,2,2))
})

test_that("state decoding for log-normal distribution works", {
  data("sim_model_4lnorm")
  x <- decode_states(sim_model_4lnorm, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_equal(fivenum(x$decoding), c(1,2,2,4,4))
})

test_that("state decoding for hierarchical case works", {
  data("dax_vw_model")
  x <- decode_states(dax_vw_model, verbose = FALSE)
  expect_s3_class(x, "fHMM_model")
  expect_equal(dim(x$decoding), c(42L, 31L))
  expect_equal(fivenum(x$decoding), c(1,1,2,2,2))
})
