# HHMM_Finance
This repository provides R and C++ code for fitting (hierarchical) hidden Markov models (H)HMMs to financial data. The code is intended to be used on closing prices of financial time series provided by https://finance.yahoo.com/. The data must be in csv-format, containing columns named "Date" and "Close", and must be saved in the folder `"./data"`. Additionally, data can be simulated.

## Contained files
- `checks.R` contains validation functions.
- `data.R` processes or simulates data.
- `init.R` initializes the code and the estimation routine.
- `loglike.cpp` computes the model's log-likelihood.
- `main.R` presents the code's [workflow](#getting-started).
- `optim.R` maximizes the log-likelihood function using [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html).
- `trans.R` contains helper functions for parameter transformations.
- `visual.R` generates visualisations of the [model results](#outputs).
- `viterbi.R` performs state decoding based on the [Viterbi algorithm](https://en.wikipedia.org/wiki/Viterbi_algorithm).

## Getting started
0. Go to `main.R`.
1. Run code chunk 1 to initialize the code.
2. Run code chunk 2 to set and check the model's [controls](#specifying-controls).
3. Run code chunk 3 to fit the model to the data.
4. Run code chunk 4 to decode the hidden states.
5. Run code chunk 5 to visualize the [results](#outputs). 
6. Run code chunk 6 to reinitialize an old model.

See below for some [examples](#examples).

## Specifying controls
The model is specified by defining the named list `controls`. The following parameters of `controls` are mandatory:
- `model_name`: a character, identifying the model
- `states`: a numeric vector of length 2, determining the model type and the number of states:
   - if `states = c(x,0)`, a HMM with `x` states is estimated
   - if `states = c(x,y)`, a HHMM with `x` coarse-scale and `y` fine-scale states is estimated
- `time_horizon`: a numeric vector of length 2, determining the length of the time horizion(s)

The following parameters of `controls` are optional and set to default values if not specified:
- `accept_codes`: a numeric vector, containing acceptable exit codes of the optimization, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `data_source`: a numeric vector of length 2, containing the file names of the financial data:
   - if `data_source = c(NA,NA)`, data is simulated
   - if `data_source = c("x.csv",NA)`, model `"./data/x.csv"` by a HMM
   - if `data_source = c("x.csv","y.csv")`, model averages of `"./data/x.csv"` (size determined by the second entry of `time_horizon`) on the coarse scale and pure data `"./data/y.csv"` on the fine scale, respectively
   - `data_source = c(NA,"y.csv")` is interpreted as `data_source = c("y.csv","y.csv")`
- `fix_dfs`: a numeric vector of length 2, fixing the degrees of freedom of the state-dependent t-distributions
   - if `fix_dfs = c(NA,NA)`, degrees of freedom are not fixed
   - if `fix_dfs = c(x,NA)`, degrees of freedom of a HMM or the coarse scale of a HHMM are fixed to `x`
   - if `fix_dfs = c(NA,y)`, degrees of freedom of the fine scale of a HHMM are fixed to `y`
   - if `fix_dfs = c(x,y)`, degrees of freedom of the coarse scale and the fine scale of a HHMM are fixed to `x` and `y`, respectively 
- `hessian`: a boolean, determining whether the Hessian is computed
- `iterlim`: an integer, specifying the maximum number of optimization iterations to be performed before termination, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `overwrite`: a boolean, determining whether existing results (on the same `model_name`) can be overwritten
- `print.level`: an integer, determining the level of printing during the optimization, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `runs`: an integer, setting the number of optimization runs
- `seed`: an integer, setting a seed for the simulation and the initialization
- `steptol`: an integer, setting the minimum allowable relative step length during the optimization, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `truncate_data`: a vector of length 2, containing dates or `NA` and specifying a subset of the empirical data

## Outputs
The following model results are saved in the folder `./models`:
- `estimates.txt`, containing estimates, gradient, Hessian, likelihood value, AIC and BIC values etc.
- `controls`, `data`, `decoding` and `fit`, .RData-files that can be reinitialized
- `lls.pdf`, a visualization of the log-likelihood values in the different estimation runs
- `pseudos.pdf`, a visualization of the pseudo-residuals
- `sdd.pdf`, a visualization of the state-dependent distributions
- `ts.pdf`, a visualization of the decoded time series (only for empirical data)

## Examples
### Fitting a HMM to the 21st century DAX
```R
### 1. Initialization
source("init.R"); init()

### 2. Set and check controls
controls = list(
  model_name    = "HMM_DAX_3",        
  data_source   = c("dax.csv",NA),
  truncate_data = c("2001-01-03",NA), 
  states        = c(3,0),
  time_horizon  = c(NA,NA),
  runs          = 200
)
controls = check_controls(controls)

### 3. Fit model to data
data = getData(controls)
fit = maxLikelihood(data,controls)

### 4. Decode hidden states
decoding = applyViterbi(data,fit,controls)

### 5. Visualize results
labels = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)
visual(data,fit,decoding,controls,labels)
```
[Results](#https://github.com/loelschlaeger/HHMM_Finance/tree/master/models/HMM_DAX_3)
