### Clear memory, suppress warnings
rm(list = ls()); cat("\f")
options(warn=-1)

### Create path to save models
if(!dir.exists("models")){dir.create("models")}

### Load old model
load(file="models/...")

### Set model parameters for new model
controls = list(
  modelName = "test",
  fileName  = "data/sandp500.csv", 
  M         = 3,
  N         = 2,
  est_df    = "yes",
  set_df_cs = 1, 
  set_df_fs = 1,       
  T         = 200,                     
  T_star    = 30,
  t_min     = "2000-1-3", 
  t_max     = "2020-05-28",     
  runs      = 100,
  iterlim   = 1000,
  cpp       = TRUE
)

### Fit model to simulated data
source("data_sim.R")
sim = simulateHHMM(controls)
source("optimize.R") 
est_sim = maxLikelihood(sim[["observations"]],controls)
save(sim,controls,est_sim,file=paste0("models/",controls[["modelName"]]))

### Fit model to financial data
source("data.R")
data = readData(controls)
source("optimize.R") 
est = maxLikelihood(data[["observations"]],controls)
save(controls,est,file=paste0("models/",controls[["modelName"]]))

### Process estimates
source("viterbi.R")
decStates = applyViterbi(data[["observations"]],est[["thetaFull"]],controls)


