### Set working directory
setwd("P:/HHMMFinance/R")

### Clear memory, suppress warnings
rm(list = ls()); cat("\f")
options(warn=-1)

### Load functions
source("simulateHHMM.R")
source("readData.R")
source("maxLikelihood.R")
source("plotEstimationError.R")

### Set model parameters
controls = list(
  M         = 3,
  N         = 2,
  
  est_df    = "no",              ### estimation of degrees of freedom: "all", "fscs" if only one for fs and cs, "no" if deterministic
  set_df_cs = 4, set_df_fs = 4,  ### only relevant if degrees of freedom are not estimated
  
  modelName = "HHMM32",
  fileName  = "dax_data.csv",

  T         = 500, ### only relevant for simulation of data 
  T_star    = 50,
  t_min     = NULL, t_max     = NULL, ### only relevant for real data 
  
  runs      = 500,
  iterlim   = 1000,
  output    = FALSE,
  cpp       = TRUE
)

### SIMULATED DATA
sim          = simulateHHMM(controls)
observations = sim[["observations"]] #simulated observations 
states       = sim[["states"]]       #true states
theta        = sim[["theta"]]        #true parameter vector

#library(foreach)
#library(doParallel)
#cores=detectCores()
#cl <- makeCluster(cores[1]-1) #not to overload your computer
#registerDoParallel(cl)
#start = initializeEstimation(controls)

#t1 <- Sys.time()
#logL_hhmm(start,observations,controls)
#Sys.time() - t1

est = maxLikelihood(observations,controls); plotEstimationError(theta,est,controls)

#stopCluster(cl)

### REAL DATA

observations = readData(controls)
est = maxLikelihood(observations,controls)

### VITERBI

#source("applyViterbi.R")
#decStates = applyViteri(observations,est,controls)

### PSEUDOS

### GRAPHICS



