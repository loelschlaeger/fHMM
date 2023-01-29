library(devtools)
load_all()

### dax vw model 1

controls <- list(
  hierarchy = TRUE,
  states    = c(2, 2),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file       = list(dax, vw),
    from       = "2010-01-01",
    to         = "2022-12-31",
    logreturns = c(TRUE, TRUE)
  ),
  fit       = list(
    runs = 200
  )
)
controls <- set_controls(controls)
dax_vw_data <- prepare_data(controls)
dax_vw_model <- fit_model(dax_vw_data, ncluster = 4)
dax_vw_model <- decode_states(dax_vw_model)
dax_vw_model_1 <- compute_residuals(dax_vw_model)
use_data(dax_vw_model_1, overwrite = TRUE)

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
    runs = 200
  )
)
controls <- set_controls(controls)
dax_vw_data <- prepare_data(controls)
dax_vw_model <- fit_model(dax_vw_data, ncluster = 4)
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
    from       = "2010-01-01",
    to         = "2022-12-31",
    logreturns = c(TRUE, TRUE)
  ),
  fit       = list(
    runs = 200
  )
)
controls <- set_controls(controls)
dax_vw_data <- prepare_data(controls)
dax_vw_model <- fit_model(dax_vw_data, ncluster = 4)
dax_vw_model <- decode_states(dax_vw_model)
dax_vw_model_3 <- compute_residuals(dax_vw_model)
use_data(dax_vw_model_3, overwrite = TRUE)

### dax vw model 4

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
    runs = 200
  )
)
controls <- set_controls(controls)
dax_vw_data <- prepare_data(controls)
dax_vw_model <- fit_model(dax_vw_data, ncluster = 4)
dax_vw_model <- decode_states(dax_vw_model)
dax_vw_model_4 <- compute_residuals(dax_vw_model)
use_data(dax_vw_model_4, overwrite = TRUE)

