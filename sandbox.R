rm(list = ls()); cat("\f")

### Initialize code
source("init.R"); load_code()

### Download data (optional)
download_data()

### Set and check controls
controls = list(
  id = "test",
  sdds = c("t",NA),
  states = c(2,0),
  time_horizon = c(1000,NA),
)

### Define events (optional)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### Fit (H)HMM
fit_hmm(controls,events)
