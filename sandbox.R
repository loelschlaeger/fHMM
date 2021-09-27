### load code
devtools::load_all()

### download data (optional)
download_data(name = "dax", symbol = "^GDAXI", path=".")

### set controls
controls = list(
  path    = ".",
  id      = "test",
  states  = 2,
  sdds    = "t",
  horizon = 400,
  fit     = list("runs" = 50)
)

### define events (optional)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### fit (H)HMM
fit_hmm(controls,events)
