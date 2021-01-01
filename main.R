rm(list = ls())

### 1. Initialization
source("init.R"); init()

### 2. Set and check controls
controls = list(
  model_name    = "HMM_3_DAX",        
  data_source   = c("dax.csv",NA),
  truncate_data = c("2000-01-03",NA), 
  states        = c(3,0),
  time_horizon  = c(NA,30),
  runs          = 200
)
controls = check_controls(controls)

### 3. Fit model to data
data = getData(controls)
fit = maxLikelihood(data,controls)

### 4. Decode hidden states
decoding = applyViterbi(data,fit,controls)

### 5. Visualize results
labels = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)
visual(data,fit,decoding,controls,labels)

### 6. Reinitialize model
reinit("HMM_DAX_3")
