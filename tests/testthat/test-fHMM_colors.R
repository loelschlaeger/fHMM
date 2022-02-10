test_that("color scheme setting works", {
  controls <- set_controls()
  expect_snapshot(fHMM_colors(controls, colors = c("red", "blue")))
})
