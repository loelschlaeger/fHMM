### 1. Initialization
rm(list = ls())
source("init.R"); init()

### 2. Set and check controls
controls = list(
  model_name    = "test",        
  data_source   = c("dax.csv","deutschebank.csv"),
  truncate_data = c("2000-01-01","2021-01-01"), 
  states        = c(3,2),
  time_horizon  = c(NA,30),
  runs          = 200,
  overwrite     = FALSE
)
controls = check_controls(controls)

### 3. Fit model to data
data = getData(controls)
fit = maxLikelihood(data,controls)

### 4. Decode hidden states
decoding = applyViterbi(data,fit,controls)

### 5. Visualize results
labels = list(
  dates = c("2008-09-15","2020-01-27"),
  names = c("Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)
visual(data,fit,decoding,controls,labels)

### 6. Reinitialize model
reinit("test")
