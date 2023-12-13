test_that("state reordering works", {
  expect_s3_class(
    reorder_states(dax_model_3t, state_order = 3:1), 
    "fHMM_model"
  )
})
