### load code ---------------------------------------------------------------
devtools::load_all()

### download data -----------------------------------------------------------
download_data(symbol = "^GDAXI", file = "dax.csv")
download_data(symbol = "VOW3.DE", file = "vw.csv")

### simulated HMM parameters ------------------------------------------------
controls = list(
  states  = 2,
  sdds    = "t(df = Inf)",
  horizon = 400,
  fit     = list("runs" = 50)
)
controls = set_controls(controls)

### empirical HMM parameters ------------------------------------------------
controls = list(
  states  = 2,
  sdds    = "t",
  data    = list(file        = "dax.csv",
                 date_column = "Date",
                 data_column = "Close",
                 logreturns  = TRUE)
)
controls = set_controls(controls)

### simulated HHMM parameters -----------------------------------------------
controls = list(
  hierarchy = TRUE,
  horizon   = c(100, 30)
)
controls = set_controls(controls)

### empirical HHMM parameters -----------------------------------------------
controls = list(
  hierarchy = TRUE,
  horizon   = c(100, NA),
  sdds      = c("t(df = 1)", "t(df = 1)"),
  period    = "m",
  data      = list(file        = c("dax.csv","vw.csv"),
                   logreturns  = c(TRUE,TRUE))
)
controls = set_controls(controls)

### prepare data ------------------------------------------------------------
data = prepare_data(controls)

### fit model ---------------------------------------------------------------
model = fit_model(data)

### summarize model ---------------------------------------------------------
summary(model)

### plot results ------------------------------------------------------------
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers",
            "First COVID-19 case in Germany")
)
plot(model, type = "", events)



