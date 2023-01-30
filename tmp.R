library(devtools)
load_all()

### dax vw model 2

controls <- list(
  hierarchy = TRUE,
  states    = c(2, 2),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file       = list(dax, vw),
    from       = "2015-01-01",
    to         = "2022-12-31",
    logreturns = c(TRUE, TRUE)
  ),
  fit       = list(
    runs    = 500,
    iterlim = 1000
  )
)
controls <- set_controls(controls)
dax_vw_data <- prepare_data(controls)
dax_vw_model <- fit_model(dax_vw_data, ncluster = 7)
dax_vw_model <- decode_states(dax_vw_model)
dax_vw_model_2 <- compute_residuals(dax_vw_model)
use_data(dax_vw_model_2, overwrite = TRUE)

### dax vw model 3

controls <- list(
  hierarchy = TRUE,
  states    = c(3, 2),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file       = list(dax, vw),
    from       = "2015-01-01",
    to         = "2022-12-31",
    logreturns = c(TRUE, TRUE)
  ),
  fit       = list(
    runs    = 500,
    iterlim = 1000
  )
)
controls <- set_controls(controls)
dax_vw_data <- prepare_data(controls)
dax_vw_model <- fit_model(dax_vw_data, ncluster = 7)
dax_vw_model <- decode_states(dax_vw_model)
dax_vw_model_3 <- compute_residuals(dax_vw_model)
use_data(dax_vw_model_3, overwrite = TRUE)

### unemp spx model

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
    logreturns = c(FALSE, TRUE)
  ),
  fit       = list(
    runs    = 500,
    iterlim = 1000
  )
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model <- fit_model(spx_unemp_data, ncluster = 7)
spx_unemp_model <- decode_states(spx_unemp_model)
spx_unemp_model_1 <- compute_residuals(spx_unemp_model)
use_data(spx_unemp_model_1, overwrite = TRUE)

### unemp spx model

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
    logreturns = c(FALSE, TRUE)
  ),
  fit       = list(
    runs    = 500,
    iterlim = 1000
  )
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model <- fit_model(spx_unemp_data, ncluster = 7)
spx_unemp_model <- decode_states(spx_unemp_model)
spx_unemp_model_2 <- compute_residuals(spx_unemp_model)
use_data(spx_unemp_model_2, overwrite = TRUE)
