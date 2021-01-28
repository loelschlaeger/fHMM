rm(list = ls()); cat("\f")

### 1. Initialize code
source("init.R"); load_code()

### 2. Download data (optional)
download_data("dax","^GDAXI")

### 3. Set and check controls
controls = list(
  id            = "HMM_3_DAX",
  sdds          = c("t",NA),
  states        = c(3,0),
  data_source   = c("dax",NA),
  data_col      = c("Close",NA),
  truncate_data = c("2000-01-03","2020-12-30"),
  scale_par     = c(0.01,NA)
)

### 4. Define events (optional)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### 5. Fit (H)HMM
hhmmf(controls,events)
