library(devtools)
load_all()

### spx unemp model 3 and 2 states

controls <- list(
  hierarchy = TRUE,
  states    = c(3, 2),
  sdds      = c("gamma", "t"),
  period    = "m",
  data      = list(
    file       = list(unemp, spx),
    date_column = c("date", "Date"),
    data_column = c("rate", "Close"),
    from       = "1955-01-01",
    to         = "2022-12-31",
    logreturns = c(FALSE, TRUE)),
  fit = list(runs = 100)
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model_long <- fit_model(spx_unemp_data, ncluster = 7)
spx_unemp_model_long <- decode_states(spx_unemp_model_long)
spx_unemp_model_long <- compute_residuals(spx_unemp_model_long)

use_data(spx_unemp_model_long, overwrite = TRUE)

### spx unemp model 2 and 2 states

controls <- list(
  hierarchy = TRUE,
  states    = c(2, 2),
  sdds      = c("gamma", "t"),
  period    = "m",
  data      = list(
    file       = list(unemp, spx),
    date_column = c("date", "Date"),
    data_column = c("rate", "Close"),
    from       = "1955-01-01",
    to         = "2022-12-31",
    logreturns = c(FALSE, TRUE)),
  fit = list(runs = 500)
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model_small_long <- fit_model(spx_unemp_data, ncluster = 7)
spx_unemp_model_small_long <- decode_states(spx_unemp_model_small_long)
spx_unemp_model_small_long <- compute_residuals(spx_unemp_model_small_long)

use_data(spx_unemp_model_small_long, overwrite = TRUE)
