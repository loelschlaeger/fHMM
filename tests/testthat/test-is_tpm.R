test_that("check for tpm works", {
  expect_true(is_tpm(matrix(c(0.8,0.1,0.2,0.9),2,2)))
  expect_false(is_tpm(matrix(c(0.8,0.2,0.2,0.9),2,2)))
  expect_false(is_tpm(matrix(c(0.8,-0.2,0.2,0.9),2,2)))
})
