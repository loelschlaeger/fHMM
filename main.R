### 1. Initialization
rm(list = ls()); source("init.R"); init()

### 2. Set control parameters
controls = list(
  modelName    = "test",        
  #dataSource   = c("data/dax.csv",NA),
  #trunc_data   = c("2000-01-03","2020-01-30"), 
  states       = c(2,0), #c(3,2),
  timeHorizon  = c(300,50),
  fix_df       = c(NA,NA),
  runs         = 50,
  iterlim      = 200,
  #seed         = 1
  overwrite    = TRUE
)
source("checks.R")
controls = check_controls(controls)

### 3. Fit model and process estimates
source("data.R")
data = getData(controls)
source("optim.R") 
est = maxLikelihood(data,controls)

### 4. Decode hidden states
source("viterbi.R")
states = applyViterbi(data,est,controls)

### 4. Visualize results (currently only "states=c(3,2)" possible)
source("plots.R")
visual(data,est,states,controls)

### 5. (Optionally) Load old model
source("init.R")
loadModel("test")

