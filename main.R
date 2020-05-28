### Clear memory, suppress warnings
rm(list = ls()); cat("\f")
options(warn=-1)

### Set model parameters for new model
controls = list(
  modelName = "HHMM32",
  fileName  = "dax_data.csv",         ### only relevant for real data 
  M         = 3,
  N         = 2,
  est_df    = "no",                   ### "all" if all dfs estimated, "fscs" if one for fs and cs, "no" if deterministic
  set_df_cs = 4, set_df_fs = 4,       ### only relevant if est_df="no"
  T         = 300,                     ### only relevant for simulation 
  T_star    = 30,
  t_min     = NULL, t_max = NULL,     ### only relevant for real data 
  runs      = 100,
  iterlim   = 1000,
  outputFile= FALSE,
  cpp       = TRUE
)

### create path to save models
dir.create(paste0(getwd(),"/models/"))

### Load old model
load(file="models/HHMM32/est")

### Simulation of model
source("data_sim.R")
source("optimize.R")             
sim     = simulateHHMM(controls)
est_sim = maxLikelihood(sim[["observations"]],controls)
save(sim,controls,est_sim,file=paste0("models/",controls[["modelName"]]))
source("plotEstimationError.R")
plotEstimationError(sim[["thetaCon"]],est_sim[["thetaCon"]],controls)

### Fit model to real Data
source("readData.R")
source("optimize.R")             
data     = readData(controls)
est_data = maxLikelihood(data[["observations"]],controls)
save(controls,est_data,file=paste0("models/",controls[["modelName"]],"/est"))

source("viterbi.R")
decStates = applyViterbi(data[["observations"]],est_data[["thetaFull"]],controls)
source("graphics.R")
source("pseudos.R")
