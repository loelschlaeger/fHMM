test_that("prediction works", {
  data("dax_model_3t")
  prediction <- predict(dax_model_3t)
  expect_equal(
    round(unlist(prediction), 2),
    c(states1 = 0.02, states2 = 0.04, states3 = 0.06, states4 = 0.08, 
      states5 = 0.1, states6 = 0.97, states7 = 0.95, states8 = 0.92, 
      states9 = 0.9, states10 = 0.88, states11 = 0.01, states12 = 0.01, 
      states13 = 0.01, states14 = 0.02, states15 = 0.02, data1 = -0.02, 
      data2 = -0.02, data3 = -0.02, data4 = -0.02, data5 = -0.02, data6 = 0, 
      data7 = 0, data8 = 0, data9 = 0, data10 = 0, data11 = 0.02, data12 = 0.02, 
      data13 = 0.02, data14 = 0.02, data15 = 0.02
    )
  )
})
