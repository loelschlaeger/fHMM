test_that("event check works", {
  events <- list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c(
      "9/11 terrorist attack", "Bankruptcy Lehman Brothers",
      "First COVID-19 case Germany"
    )
  )
  events <- fHMM_events(events)
  expect_equal(
    events,
    structure(
      list(dates = structure(c(11576, 14137, 18288), class = "Date"), 
           labels = c("9/11 terrorist attack", "Bankruptcy Lehman Brothers", 
                      "First COVID-19 case Germany")
           ), 
      class = "fHMM_events"
    )
  )
  sink(tempfile())
  expect_s3_class(print(events), "fHMM_events")
  sink()
  expect_error(fHMM_events(list("more","than","two","elements")))
  expect_warning(fHMM_events(events))
  expect_error(fHMM_events("not_a_list"))
  expect_error(fHMM_events(list("more","than","two","elements")))
  expect_error(fHMM_events(list("list" = "contains", "wrong" = "elements")))
})
