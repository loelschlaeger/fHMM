rm(list = ls())

### 1. Initialization
source("init.R"); init()

### 2. Download data
download_data()

### 3. Set and check controls
controls = list(
  id            = "test",        
  #data_source   = c("dax","dax"),
  #data_col      = c("Close","Close"),
  #truncate_data = c("2000-01-03","2020-12-30"), 
  states        = c(3,0),
  time_horizon  = c(1000,NA),
  overwrite     = TRUE,
  #data_cs_type  = "mean",
  sdds          = c("t(1)",NA),
  runs = 50
)
controls = check_controls(controls)

### 4. Fit model to data
data = get_data(controls)
fit = max_likelihood(data,controls)

### 5. Decode hidden states
decoding = apply_viterbi(data,fit,controls)

### 6. Visualize results
labels = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)
visual(data,fit,decoding,controls,labels)

### 7. Reinitialize model
reinit("test")
