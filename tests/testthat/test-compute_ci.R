test_that("ci computation works", {
  data("dax_model_3t")
  expect_error(compute_ci("not_an_fHMM_model"))
  expect_error(compute_ci(dax_model_3t, "not_a_numeric"))
  expect_error(compute_ci(dax_model_3t, -1))
  expect_error(compute_ci(dax_model_3t, 2))
  dax_model_3t_tmp <- dax_model_3t
  dax_model_3t_tmp$hessian[1,1] <- Inf
  suppressWarnings(expect_warning(compute_ci(dax_model_3t_tmp)))
  ci <- compute_ci(x = dax_model_3t, alpha = 0.05)
  expect_type(ci, "list")
  expect_length(ci, 3)
  expect_named(ci, c("lb", "estimate", "ub"))
  expect_equal(
    round(unlist(ci), 2),
    c(lb1 = 0.01, lb2 = 0, lb3 = 0.02, lb4 = 0.01, lb5 = 0, lb6 = 0, 
      lb7 = 0, lb8 = 0, lb9 = 0, lb10 = 0.01, lb11 = 0.01, lb12 = 0.02, 
      lb13 = 3.96, lb14 = 75922.72, lb15 = 5.5, estimate1 = 0.02, estimate2 = 0, 
      estimate3 = 0.03, estimate4 = 0.02, estimate5 = 0, estimate6 = 0.01, 
      estimate7 = 0, estimate8 = 0, estimate9 = 0, estimate10 = 0.01, 
      estimate11 = 0.01, estimate12 = 0.03, estimate13 = 5.27, estimate14 = 75925.47, 
      estimate15 = 10.64, ub1 = 0.03, ub2 = 0, ub3 = 0.04, ub4 = 0.03, 
      ub5 = 0, ub6 = 0.01, ub7 = 0, ub8 = 0, ub9 = 0, ub10 = 0.01, 
      ub11 = 0.01, ub12 = 0.03, ub13 = 7.01, ub14 = 75928.21, ub15 = 20.58
    )
  )
})
