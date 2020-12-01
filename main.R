### 1. Initialization
rm(list = ls())
source("init.R"); init()

### 2. Set and check controls
controls = list(
  model_name    = "test",        
  data_source   = c(NA,NA),
  truncate_data = c("2001-01-03","2020-01-30"), 
  states        = c(2,2),
  time_horizon  = c(50,5),
  fix_dfs       = c(NA,NA),
  runs          = 50,
  iterlim       = 200,
  #seed          = 1,
  overwrite     = TRUE
)
controls = check_controls(controls)

### 3. Fit model to data
data = getData(controls)
fit = maxLikelihood(data,controls)

### 4. Decode hidden states
decoding = applyViterbi(data,fit,controls)

### 5. Visualize results (currently only "states=c(3,2)" possible)
labels = list(
  dates = c("2008-9-15"),
  names = c("bankruptcy of Lehman Brothers")
)
visual(data,fit,decoding,labels,controls)

### 6. Reinitialize model
reinit("test")
