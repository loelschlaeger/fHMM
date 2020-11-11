### 1. Clear memory and create path to save models
rm(list = ls()); cat("\f")
if(!dir.exists("models")){dir.create("models")}

### 2. Set control parameters
controls = list(
  modelName    = "HHMM_DAX_32_longrun",        
  dataSource   = "data/dax.csv",
  trunc_data   = c("2000-01-03","2020-01-30"), 
  states       = c(3,2),
  timeHorizon  = c(100,30),
  fix_df       = c(1,1),
  runs         = 700,
  iterlim      = 500
)

### 3. (Optionally) Load old model
path = "models/HHMM_sandp500_32_longrun"
if(file.exists(path)){load(file=path)}

### 4a. Fit model to simulated data
source("sim.R")
sim = simulateHHMM(controls)
source("optim.R") 
est_sim = maxLikelihood(sim[["observations"]], controls)
save(sim, controls, est_sim, file=paste0("models/", controls[["modelName"]]))

### 4b. Fit model to empirical data and process estimates
source("data.R")
data = readData(controls)
source("optim.R") 
est = maxLikelihood(data[["observations"]], controls)
sink(file = paste0("models/",controls$modelName,"_estimates.txtf")); controls; est; sink()
source("viterbi.R")
states = applyViterbi(data[["observations"]], est[["thetaFull"]], controls)
save(data, controls, est, states, file = paste0("models/", controls[["modelName"]]))

### 5. Plot graphics (only M=3 and N=2)
source("plots.R")
hhmm_visual(data, est, states, controls)

