library(devtools)
load_all()

unemp <- fHMM::unemp
unemp$date <- substr(unemp$date, 1, 7)
unemp <- dplyr::distinct(unemp)
unemp_diff <- data.frame("date" = unemp$date[-1], "rate_diff" = diff(unemp$rate))
unemp_diff$date <- paste0(unemp_diff$date, "-01")

days <- seq(from = min(as.Date(unemp_diff$date)),
            to = ceiling_date(max(as.Date(unemp_diff$date)), "month") - days(1),
            by = "1 days")

unemp_diff <- data.frame(
  date = days,
  rate_diff = setNames(unemp_diff$rate_diff, unemp_diff$date)[format(days, format = "%Y-%m-01")]
)
unemp_diff$date <- as.character(unemp_diff$date)

### unemp spx model 2/2

controls <- list(
  hierarchy = TRUE,
  states    = c(2, 2),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file       = list(unemp_diff, spx),
    date_column = c("date", "Date"),
    data_column = c("rate_diff", "Close"),
    from       = "2000-01-01",
    to         = "2022-12-31",
    logreturns = c(FALSE, TRUE)
  ),
  fit       = list(
    runs    = 500,
    iterlim = 500
  )
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model <- fit_model(spx_unemp_data, ncluster = 6)
spx_unemp_model <- decode_states(spx_unemp_model)
spx_unemp_model_2_2 <- compute_residuals(spx_unemp_model)
use_data(spx_unemp_model_2_2, overwrite = TRUE)

### unemp spx model 3/2

controls <- list(
  hierarchy = TRUE,
  states    = c(3, 2),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file       = list(unemp_diff, spx),
    date_column = c("date", "Date"),
    data_column = c("rate_diff", "Close"),
    from       = "2000-01-01",
    to         = "2022-12-31",
    logreturns = c(FALSE, TRUE)
  ),
  fit       = list(
    runs    = 500,
    iterlim = 500
  )
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model <- fit_model(spx_unemp_data, ncluster = 6)
spx_unemp_model <- decode_states(spx_unemp_model)
spx_unemp_model_3_2 <- compute_residuals(spx_unemp_model)
use_data(spx_unemp_model_3_2, overwrite = TRUE)

### unemp spx model 2/3

controls <- list(
  hierarchy = TRUE,
  states    = c(2, 3),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file       = list(unemp_diff, spx),
    date_column = c("date", "Date"),
    data_column = c("rate_diff", "Close"),
    from       = "2000-01-01",
    to         = "2022-12-31",
    logreturns = c(FALSE, TRUE)
  ),
  fit       = list(
    runs    = 500,
    iterlim = 500
  )
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model <- fit_model(spx_unemp_data, ncluster = 6)
spx_unemp_model <- decode_states(spx_unemp_model)
spx_unemp_model_2_3 <- compute_residuals(spx_unemp_model)
use_data(spx_unemp_model_2_3, overwrite = TRUE)

### unemp spx model 2/2 long

controls <- list(
  hierarchy = TRUE,
  states    = c(2, 2),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file       = list(unemp_diff, spx),
    date_column = c("date", "Date"),
    data_column = c("rate_diff", "Close"),
    from       = "1955-02-01",
    to         = "2022-12-31",
    logreturns = c(FALSE, TRUE)
  ),
  fit       = list(
    runs    = 500,
    iterlim = 500
  )
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model <- fit_model(spx_unemp_data, ncluster = 6)
spx_unemp_model <- decode_states(spx_unemp_model)
spx_unemp_model_2_2long <- compute_residuals(spx_unemp_model)
use_data(spx_unemp_model_2_2long, overwrite = TRUE)

### unemp spx model 3/2 long

controls <- list(
  hierarchy = TRUE,
  states    = c(3, 2),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file       = list(unemp_diff, spx),
    date_column = c("date", "Date"),
    data_column = c("rate_diff", "Close"),
    from       = "1955-02-01",
    to         = "2022-12-31",
    logreturns = c(FALSE, TRUE)
  ),
  fit       = list(
    runs    = 500,
    iterlim = 500
  )
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model <- fit_model(spx_unemp_data, ncluster = 6)
spx_unemp_model <- decode_states(spx_unemp_model)
spx_unemp_model_3_2long <- compute_residuals(spx_unemp_model)
use_data(spx_unemp_model_3_2long, overwrite = TRUE)

### unemp spx model 2/3 long

controls <- list(
  hierarchy = TRUE,
  states    = c(2, 3),
  sdds      = c("t", "t"),
  period    = "m",
  data      = list(
    file       = list(unemp_diff, spx),
    date_column = c("date", "Date"),
    data_column = c("rate_diff", "Close"),
    from       = "1955-02-01",
    to         = "2022-12-31",
    logreturns = c(FALSE, TRUE)
  ),
  fit       = list(
    runs    = 500,
    iterlim = 500
  )
)
controls <- set_controls(controls)
spx_unemp_data <- prepare_data(controls)
spx_unemp_model <- fit_model(spx_unemp_data, ncluster = 6)
spx_unemp_model <- decode_states(spx_unemp_model)
spx_unemp_model_2_3long <- compute_residuals(spx_unemp_model)
use_data(spx_unemp_model_2_3long, overwrite = TRUE)

