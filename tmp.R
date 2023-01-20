library(devtools)
load_all()

### dax model 2n

controls <- list(
  states = 2,
  sdds   = "t(df = Inf)",
  data   = list(
    file        = dax,
    date_column = "Date",
    data_column = "Close",
    logreturns  = TRUE,
    from        = "2000-01-03",
    to          = "2022-31-12"
  ),
  fit    = list(runs = 100)
)
controls <- set_controls(controls)
dax_data <- prepare_data(controls)
dax_model_2n <- fit_model(dax_data)
dax_model_2n <- decode_states(dax_model_2n)
dax_model_2n <- compute_residuals(dax_model_2n)

use_data(dax_model_2n, overwrite = TRUE)

### dax model 3t

controls <- list(
  states = 3,
  sdds   = "t",
  data   = list(
    file        = dax,
    date_column = "Date",
    data_column = "Close",
    logreturns  = TRUE,
    from        = "2000-01-03",
    to          = "2022-31-12"
  ),
  fit    = list(runs = 100)
)
controls <- set_controls(controls)
dax_data <- prepare_data(controls)
dax_model_3t <- fit_model(dax_data)
dax_model_3t <- decode_states(dax_model_3t)
dax_model_3t <- reorder_states(dax_model_3t, c(3,2,1))
dax_model_3t <- compute_residuals(dax_model_3t)

use_data(dax_model_3t, overwrite = TRUE)

### dax vw model

controls <- list(
  hierarchy = TRUE,
  states    = c(2, 2),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file       = list(dax, vw),
    from       = "2010-01-01",
    to         = "2022-31-12",
    logreturns = c(TRUE, TRUE))
)
controls <- set_controls(controls)
dax_vw_data <- prepare_data(controls)
dax_vw_model <- fit_model(dax_vw_data)
dax_vw_model <- decode_states(dax_vw_model)
dax_vw_model <- compute_residuals(dax_vw_model)

use_data(dax_vw_model, overwrite = TRUE)

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
    from       = "2000-01-01",
    to         = "2022-12-31",
    logreturns = c(FALSE, TRUE)),
  fit = list(runs = 100)
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model <- fit_model(spx_unemp_data)
spx_unemp_model <- decode_states(spx_unemp_model)
spx_unemp_model <- compute_residuals(spx_unemp_model)

use_data(spx_unemp_model, overwrite = TRUE)

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
    from       = "2000-01-01",
    to         = "2022-12-31",
    logreturns = c(FALSE, TRUE)),
  fit = list(runs = 100)
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model_small <- fit_model(spx_unemp_data)
spx_unemp_model_small <- decode_states(spx_unemp_model_small)
spx_unemp_model_small <- compute_residuals(spx_unemp_model_small)

use_data(spx_unemp_model_small, overwrite = TRUE)
