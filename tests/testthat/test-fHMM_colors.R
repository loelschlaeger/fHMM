test_that("input checks for color scheme setting works", {
  expect_error(fHMM_colors("not_an_fHMM_controls_object"))
  controls <- set_controls()
  expect_error(fHMM_colors(controls = controls, colors = 1))
  expect_error(fHMM_colors(controls = controls, colors = "not_a_valid_color"))
})

test_that("color scheme setting works", {
  controls <- set_controls()
  expect_equal(
    fHMM_colors(controls),
    structure(
      c("#8B000099", "#00640099"), class = "fHMM_colors"
    )
  )
  expect_equal(
    fHMM_colors(controls, colors = c("red", "blue")),
    structure(
      c("#FF000099", "#0000FF99"), class = "fHMM_colors"
    )
  )
  controls <- set_controls(controls = list(hierarchy = TRUE))
  expect_equal(
    fHMM_colors(controls),
    structure(
      list(cs = c("#8B000099", "#00640099"), 
           fs = structure(
             c("#B1555599", "#55975599", "#5C000099", "#00420099"), 
             dim = c(2L, 2L)
            )
          ), 
      class = "fHMM_colors"
    )
  )
})
