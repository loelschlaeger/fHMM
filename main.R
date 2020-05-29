### Clear memory, suppress warnings
rm(list = ls()); cat("\f")
options(warn=-1)

### Create path to save models
if(!dir.exists("models")){dir.create("models")}

### Load old model
load(file="models/HHMM32")

### Set model parameters for new model
controls = list(
  modelName = "HHMM_DAX_32",
  fileName  = "dax_new.csv", 
  M         = 3,
  N         = 2,
  est_df    = "all",                   
  ### set est_df to "all" if all dfs are estimated, "fscs" if one for fs and cs, "no" if deterministic
  set_df_cs = 4, 
  set_df_fs = 4,       
  T         = 200,                     
  T_star    = 30,
  t_min     = "2000-1-3", 
  t_max     = "2020-05-28",     
  runs      = 50,
  iterlim   = 1000,
  cpp       = TRUE
)

### Fit model to simulated data
source("data_sim.R")
sim = simulateHHMM(controls)
source("optimize.R") 
est_sim = maxLikelihood(sim[["observations"]],controls)
save(sim,controls,est_sim,file=paste0("models/",controls[["modelName"]]))

### Fit model to DAX data
source("data_dax.R")
data = readData(controls)
source("optimize.R") 
est_dax = maxLikelihood(data[["observations"]],controls)
save(controls,est_dax,file=paste0("models/",controls[["modelName"]]))

### Process estimates
source("viterbi.R")
decStates = applyViterbi(data[["observations"]],est_dax[["thetaFull"]],controls)
source("pseudos.R")
plotPseudos()
source("graphics.R")
plotTimeseries(data,est_dax,decStates)

