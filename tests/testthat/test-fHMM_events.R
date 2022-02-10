test_that("event check works", {
  events <- list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c(
      "9/11 terrorist attack", "Bankruptcy Lehman Brothers",
      "First COVID-19 case Germany"
    )
  )
  expect_snapshot(fHMM_events(events))
})
