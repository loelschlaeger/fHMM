test_that("plotting simulated fHMM_data works", {
  pdf(file = NULL)
  controls <- set_controls()
  data <- prepare_data(controls)
  expect_null(plot(data))
  expect_error(
    plot.fHMM_data(1),
    "'x' is not of class 'fHMM_data'."
  )
  expect_error(
    plot.fHMM_data(data, events = 1),
    "'events' is not of class 'fHMM_events'."
  )
  events <- list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c(
      "9/11 terrorist attack", "Bankruptcy Lehman Brothers",
      "First COVID-19 case Germany"
    )
  )
  events <- fHMM_events(events)
  expect_warning(
    plot(data, events),
    "Can't have 'events' for simulated data."
  )
  dev.off()
})

test_that("plotting simulated fHMM_data works", {
  controls <- set_controls(list(data = list(file = dax)))
  data <- prepare_data(controls)
  events <- list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c(
      "9/11 terrorist attack", "Bankruptcy Lehman Brothers",
      "First COVID-19 case Germany"
    )
  )
  events <- fHMM_events(events)
  pdf(file = NULL)
  expect_null(plot(data, events))
  dev.off()
})

test_that("plotting fHMM_model works", {
  expect_error(
    plot.fHMM_model(1),
    "'x' is not of class 'fHMM_model'."
  )
  pdf(file = NULL)
  events <- list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c(
      "9/11 terrorist attack", "Bankruptcy Lehman Brothers",
      "First COVID-19 case Germany"
    )
  )
  events <- fHMM_events(events)
  expect_error(
    plot(sim_model_2gamma, events = "events", plot_type = "ts"),
    "'events' is not of class 'fHMM_events'."
  )
  expect_warning(
    plot(sim_model_2gamma, events = events, plot_type = "ts"),
    "Can't have 'events' for simulated data."
  )
  expect_null(plot(sim_model_2gamma, plot_type = "ts"))
  expect_null(plot(sim_model_2gamma, plot_type = "ll"))
  sim_model_2gamma$residuals <- NULL
  expect_warning(
    plot(sim_model_2gamma, plot_type = "pr"),
    "Residuals are not available"
  )
  sim_model_2gamma <- decode_states(sim_model_2gamma, verbose = FALSE)
  sim_model_2gamma <- compute_residuals(sim_model_2gamma, verbose = FALSE)
  expect_null(plot(sim_model_2gamma, plot_type = "pr"))
  expect_null(plot(sim_model_2gamma, plot_type = "ts"))
  expect_null(plot(sim_model_2gamma, plot_type = "sdds"))
  sim_model_2gamma$lls <- sim_model_2gamma$lls[1:5]
  expect_null(plot(sim_model_2gamma, plot_type = "ll"))
  dev.off()
})

test_that("plotting hierarchical fHMM_model works", {
  pdf(file = NULL)
  events <- list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c(
      "9/11 terrorist attack", "Bankruptcy Lehman Brothers",
      "First COVID-19 case Germany"
    )
  )
  events <- fHMM_events(events)
  expect_null(plot(dax_vw_model, plot_type = "ts"))
  expect_null(plot(dax_vw_model, plot_type = "ll"))
  dax_vw_model <- decode_states(dax_vw_model, verbose = FALSE)
  dax_vw_model <- compute_residuals(dax_vw_model, verbose = FALSE)
  expect_null(plot(dax_vw_model, plot_type = "pr"))
  expect_null(plot(dax_vw_model, plot_type = "ts"))
  expect_null(plot(dax_vw_model, plot_type = "sdds"))
  expect_null(plot(dax_vw_model, plot_type = "ll"))
  dev.off()
})

test_that("plotting relative and absolute likelihood works", {
  pdf(file = NULL)
  expect_null(plot(dax_model_3t, plot_type = "ll", ll_relative = TRUE))
  expect_null(plot(dax_model_3t, plot_type = "ll", ll_relative = FALSE))
  dev.off()
  expect_error(
    plot(dax_model_3t, plot_type = "ll", ll_relative = "FALSE"),
    "'ll_relative' must be 'TRUE' or 'FALSE'."
  )
})


