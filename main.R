rm(list = ls()); cat("\f")

### 1. Initialize code
source("init.R"); load_code()

### 2. Download data (optional)
download_data()

### 3. Set controls
controls = list(
  #data_source   = c("dax","dax"),
  #data_col      = c("Close","Close"),
  #truncate_data = c("2000-01-03","2010-12-30"),
  states        = c(2,2),
  time_horizon  = c(100,30),
  sdds          = c("t","t"),
  runs          = 10,
  data_cs_type  = "mean",
  at_true = TRUE
)

### 4. Define events (optional)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### 5. Fit (H)HMM
hhmmf(id       = "test",
      controls = controls,
      events   = events,
      warn     = 1)
