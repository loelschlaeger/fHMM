### load code ---------------------------------------------------------------
devtools::load_all()

### download data -----------------------------------------------------------
download_data(symbol = "^GDAXI", file = "dax.csv")
download_data(symbol = "VOW3.DE", file = "vw.csv")

### simulated HMM -----------------------------------------------------------
controls = list(
  states  = 2,
  sdds    = "t(mu = 0, df = Inf)",
  horizon = 400,
  fit     = list("runs" = 50)
)
controls = set_controls(controls)
data = prepare_data(controls)
summary(data)
model = fit_model(data)
model = decode_states(model)
model = reorder_states(model, state_order = 2:1)
model = compute_residuals(model)
summary(model)
compare(model)
plot(model, type = "ll")
plot(model, type = "sdd")
plot(model, type = "pr")
predict(model, time_points = 1:10)

### empirical HMM -----------------------------------------------------------
controls = list(
  states  = 2,
  sdds    = "t",
  data    = list(file        = "dax.csv",
                 date_column = "Date",
                 data_column = "Close",
                 logreturns  = TRUE)
)
controls = set_controls(controls)
data = prepare_data(controls)
model = fit_model(data)
summary(model)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers",
            "First COVID-19 case in Germany")
)
plot(model, type = "sdd", events)

### simulated HHMM ----------------------------------------------------------
controls = list(
  hierarchy = TRUE,
  states = c(2,4),
  horizon   = c(100, 30)
)
controls = set_controls(controls)
data = prepare_data(controls)
model = fit_model(data)
summary(model)
compare(model)
plot(model, type = "ts")

### empirical HHMM ----------------------------------------------------------
controls = list(
  hierarchy = TRUE,
  horizon   = c(100, NA),
  sdds      = c("t(df = 1)", "t(df = 1)"),
  period    = "m",
  data      = list(file        = c("dax.csv","vw.csv"),
                   logreturns  = c(TRUE,TRUE))
)
controls = set_controls(controls)
data = prepare_data(controls)
model = fit_model(data)
summary(model)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers",
            "First COVID-19 case in Germany")
)
plot(model, type = "", events)


