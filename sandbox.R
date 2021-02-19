rm(list = ls()); cat("\f")

### Initialize code
source("load_code.R")

### Download data (optional)
download_data(path=".")

### Set and check controls
controls = list(
  path = ".",
  id = "test",
  sdds = c("t","gamma"),
  states = c(2,3),
  runs = 10,
  accept_codes = "all",
  time_horizon = c(100,30)
)

### Define events (optional)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### Fit (H)HMM
fit_hmm(controls,events)
