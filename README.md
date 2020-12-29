# HHMM_Finance

This repository provides R and C++ code for fitting hierarchical hidden Markov models (HHMMs) to financial data. 

## Files

- `checks.R` provides several validation functions for [objects](#objects).
- `data.R` processes empirical data. The data has to be provided in a csv-file, containing columns "Date" and "Close" (closing prices).
- `data_sim.R` simulates data from a HHMM.
- `init.R` initializes the estimation routine (randomly).
- `loglike.R` computes the models' log-likelihood.
- `loglike_cpp.cpp` is the C++-version of computing the models' log-likelihood.
- `main.R` is the master file.
- `optim.R` maximizes the log-likelihood function using the standard R function `nlm` for non-linear optimization.
- `plots.R` generates visualisations of the model results, only for M=3 and N=2.
- `trans.R` provides helper functions for required parameter transformations.
- `viterbi.R` is an implementation of the Viterbi algorithm.

## Data
The code is designed for closing prices of financial time series provided by https://finance.yahoo.com/.

## Objects

## Getting Started

1. Go to `main.R`.
2. Optionally run code chunk 1 to clear your memory.
3. Run code chunk 2 to create subdirection `"\models"` to save estimated models.
4. Optionally run code chunk 3 to load estimates from a previously saved model.
5. Set model parameters in code chunk 4 and run afterwards.
    + `modelName` sets a model reference.
    + `fileName` is the source of the empirical data. Not required for fitting the HHMM to simulated data.
    + `M` sets the number of coarse-scale states.
    + `N` sets the number of fine-scale states.
    + Set `est_df="yes"` to estimate degrees of freedom for the state-dependent distributions, set `est_df="no"` to fix the degrees of freedom.
    + Fix degrees of freedom for the coarse scale state-dependent distributions by assigning an integer to `set_df_cs`. Only required if `est_df="no"`.
    + Fix degrees of freedom for the fine scale state-dependent distributions by assigning an integer to `set_df_fs`. Only required if `est_df="no"`.
    + `T` sets the number of coarse scale observations. Only required for fitting the HHMM to simulated data. 
    + `T_star` sets the length of the fine scale process.
    + `t_min` defines the start of the empirical time series. Only required for fitting the HHMM to empirical data.
    + `t_max` defines the end of the empirical time series. Only required for fitting the HHMM to empirical data.
    + `runs` sets the number of runs for the numerical likelihood maximization routine.
    + `iterlim` bounds the number of iterations in the numerical maximization routine.
6. Optionally run code chunk 5a to fit the HHMM to simulated data and to save estimates afterwards at location determined by `modelName`.
7. Optionally run code chunk 5b to fit the HHMM to empirical data determined by variable `fileName`, to perform state decoding and to save estimates and decoded states afterwards at location determined by `modelName`.
8. Optionally run code chunk 6 to create graphics of the model results. This works only for M=3 and N=2.

## Running examples

### Fit HHMM to simulated data
```
### 2. Create path to save models
if(!dir.exists("models")){dir.create("models")}

### 4. Set model parameters for new model
controls = list(
  modelName = "HHMM_sim_32",
  fileName  = "", 
  M         = 3,
  N         = 2,
  est_df    = "no",
  set_df_cs = 1, 
  set_df_fs = 1,       
  T         = 200,                     
  T_star    = 30,
  t_min     = "", 
  t_max     = "",     
  runs      = 100,
  iterlim   = 1000
)

### 5a. Fit model to simulated data
source("data_sim.R")
sim = simulateHHMM(controls)
source("optimize.R") 
est_sim = maxLikelihood(sim[["observations"]],controls)
save(sim,controls,est_sim,file=paste0("models/",controls[["modelName"]]))
```

### Fit HHMM to empirical data
```
### 2. Create path to save models
if(!dir.exists("models")){dir.create("models")}

### 4. Set model parameters for new model
controls = list(
  modelName = "HHMM_DAX_32",
  fileName  = "data/dax.csv", 
  M         = 3,
  N         = 2,
  est_df    = "yes",
  set_df_cs = NULL, 
  set_df_fs = NULL,       
  T         = 200,                     
  T_star    = 30,
  t_min     = "2000-1-3", 
  t_max     = "2020-05-28",     
  runs      = 100,
  iterlim   = 1000
)

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
```
