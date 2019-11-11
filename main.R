### Set working directory
setwd("P:/HHMMFinance/RHHMMFinance")

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
  T         = 300,                    ### only relevant for simulation 
  T_star    = 100,
  t_min     = NULL, t_max = NULL,     ### only relevant for real data 
  
  runs      = 100,
  iterlim   = 1000,
  outputFile= FALSE,
  cpp       = TRUE
)

### Load old model
load(file="models/HHMM32/est")

### Simulation of model
source("simulateHHMM.R")
source("maxLikelihood.R")             ### why does this takes so much time?
source("plotEstimationError.R")
sim     = simulateHHMM(controls)
est_sim = maxLikelihood(sim[["observations"]],controls); save(sim,controls,est_sim,file=paste0("models/",controls[["modelName"]]),"/est"); plotEstimationError(sim[["thetaCon"]],est_sim[["thetaCon"]],controls)

### Fit model to real Data
source("readData.R")
source("maxLikelihood.R")             ### why does this takes so much time?
data     = readData(controls)
est_data = maxLikelihood(data[["observations"]],controls); save(controls,est_data,file=paste0("models/",controls[["modelName"]],"/est"))

source("applyViterbi.R")
decStates = applyViterbi(data[["observations"]],est_data[["thetaFull"]],controls)
#source("drawGraphics.R")

#source("pseudos.R")




