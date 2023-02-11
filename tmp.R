library(devtools)
load_all()

controls <- list(
  hierarchy = TRUE,
  states    = c(2, 2),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file       = list(unemp, spx),
    date_column = c("date", "Date"),
    data_column = c("rate_diff", "Close"),
    from       = "1970-01-01",
    to         = "2020-01-01",
    logreturns = c(FALSE, TRUE)
  ),
  fit       = list(
    runs    = 100,
    iterlim = 300
  )
)
controls <- set_controls(controls)
unemp_spx_data <- prepare_data(controls)
unemp_spx_model_2_2 <- fit_model(unemp_spx_data, ncluster = 7)
unemp_spx_model_2_2 <- decode_states(unemp_spx_model_2_2)
unemp_spx_model_2_2long <- compute_residuals(unemp_spx_model_2_2)

use_data(unemp_spx_model_2_2long, overwrite = TRUE, compress = "xz")