test_that("dax_model_2n", {
  expect_equal(
    round(coef(dax_model_2n)$estimate, 4),
    c(0.0121, 0.031, -0.0015, 7e-04, 0.0231, 0.0094)
  )
})

test_that("dax_model_3t", {
  expect_equal(
    round(coef(dax_model_3t)$estimate, 4),
    c(0.005, 0, 0.0184, 0.0245, 0, 0.019, -0.0018, -3e-04, 0.0013, 
      0.0259, 0.013, 0.0058, 10.8359, 48.6557, 5.2485)
  )
})

test_that("dax_vw_model", {
  expect_equal(
    round(coef(dax_vw_model)$estimate, 4),
    c(0.0745, 0.0572, 0, 5e-04, 0.0029, 0.0015, 11.9429, 132.9705, 
      0.0098, 0.0434, -5e-04, 3e-04, 0.0337, 0.019, 4.5436, 10.0628, 
      0.0706, 0.0212, 3e-04, 5e-04, 0.0151, 0.0075, 11.8387, 10.2038
    )
  )
})

test_that("unemp_spx_model_3_2", {
  expect_equal(
    suppressWarnings(
      round(coef(unemp_spx_model_3_2)$estimate, 4)
    ),
    c(0.0317, 0, 0.2268, 0.0694, 0, 0.0954, 0.1883, 0.0294, -0.0433, 
      0.2652, 0.1426, 0.1403, 157967.9547, 6.4653, 17.6603, 0.0414, 
      0.0087, 0, -0.0037, 0.018, 0.0369, 2361037.2816, 6.625, 0.0222, 
      0.0059, 5e-04, -0.0015, 0.0092, 0.0145, 19.6321, 11.1933, 0.02, 
      0.0222, 9e-04, 2e-04, 0.0043, 0.0068, 6.5193, 9.0921)
  )
})

test_that("sim_model_2gamma", {
  expect_equal(
    round(coef(sim_model_2gamma)$estimate, 4),
    c(0.1785, 0.0785, 0.5008, 0.9181)
  )
})
