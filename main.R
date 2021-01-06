rm(list = ls())

### 1. Initialization
source("init.R"); init()

### 2. Download financial data
downloadData()

### 3. Set and check controls
controls = list(
  id            = "test",        
  data_source   = c("dax","dax"),
  data_col      = c("Close","Close"),
  truncate_data = c("2000-01-03","2005-01-03"), 
  states        = c(2,2),
  runs          = 50,
  seed          = 1,
  time_horizon  = c(NA,30),
  overwrite     = TRUE
  )
controls = check_controls(controls)

### 4. Fit model to data
data = getData(controls)
fit = maxLikelihood(data,controls)

### 5. Decode hidden states
decoding = applyViterbi(data,fit,controls)

### 6. Visualize results
labels = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
  )
visual(data,fit,decoding,controls,labels)

### 7. Reinitialize model
reinit("test")
