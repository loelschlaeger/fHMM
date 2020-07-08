### 1. Clear memory
rm(list = ls()); cat("\f")

### 2. Create path to save models
if(!dir.exists("models")){dir.create("models")}

### 3. Load old model
path = "models/HHMM_sandp500_32_longrun"
if(file.exists(path)){load(file = path)}

### 4. Set model parameters for new model
controls = list(
  modelName = "HHMM_sandp500_32_longrun",
  fileName  = "data/sandp500.csv", 
  M         = 3,
  N         = 2,
  est_df    = "yes",
  set_df_cs = 1, 
  set_df_fs = 1,       
  T         = 100,                     
  T_star    = 30,
  t_min     = "2000-01-03", 
  t_max     = "2020-01-30",     
  runs      = 1000,
  iterlim   = 500
)

### 5a. Fit model to simulated data
source("data_sim.R")
sim = simulateHHMM(controls)
source("optim.R") 
est_sim = maxLikelihood(sim[["observations"]], controls)
save(sim, controls, est_sim, file=paste0("models/", controls[["modelName"]]))

### 5b. Fit model to empirical data and process estimates
source("data.R")
data = readData(controls)
source("optim.R") 
est = maxLikelihood(data[["observations"]], controls)
sink(file = paste0("models/",controls$modelName,"_estimates.txtf")); controls; est; sink()
source("viterbi.R")
states = applyViterbi(data[["observations"]], est[["thetaFull"]], controls)
save(data, controls, est, states, file = paste0("models/", controls[["modelName"]]))

### 6. Plot graphics (only M=3 and N=2)
source("plots.R")
hhmm_visual(data, est, states, controls)

