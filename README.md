# HHMM_Finance
This repository provides R and C++ code for fitting (hierarchical) hidden Markov models (H)HMMs to financial time series. The code is intended to be used on share prices provided by https://finance.yahoo.com/. The data must be in csv-format, must contain a column named "Date" and must be saved in the folder `"./data"`. Additionally, data can be simulated.

## Contained files
- `checks.R` provides validation functions.
- `data.R` processes or simulates data.
- `init.R` initializes the code and the estimation routine.
- `loglike.cpp` computes the model's log-likelihood.
- `loop.R` loops over multiple [model specifications](#specifying-controls).
- `main.R` presents the code's [workflow](#getting-started).
- `optim.R` maximizes the log-likelihood function using [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html).
- `trans.R` contains helper functions for parameter transformations.
- `ttest.R` tests for significant differences between parameters.
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
A model is specified by setting parameters of the named list `controls`. The following parameters are mandatory:
- `id`: a character, identifying the model
- `states`: a numeric vector of length 2, determining the model type and the number of states:
   - if `states = c(x,0)`, a HMM with `x` states is estimated
   - if `states = c(x,y)`, a HHMM with `x` coarse-scale and `y` fine-scale states is estimated
- And either `data_source` along with `data_col` (for empirical data) or `time_horizon` (for simulated data) has to be specified, see below.

The following parameters are optional and set to [default values](#default-values) if not specified:
- `accept_codes`: either a numeric vector (containing acceptable exit codes of the optimization) or the character `all` (accepting all codes), see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `at_true`: a boolean, determining whether the optimization is initialised at the true parameter values (only for simulated data, sets `runs=1`, ignores `accept_codes`)
- `data_col`: a character vector of length 2, containing names of the desired columns of `data_source`
- `data_source`: a character vector of length 2, containing the file names of the empirical data:
   - if `data_source = c(NA,NA)`, data is simulated
   - if `data_source = c("x",NA)`, data `"./data/x.csv"` is modeled by a HMM
   - if `data_source = c("x","y")`, averages of data `"./data/x.csv"` (average size determined by the second entry of `time_horizon`) on the coarse scale and data `"./data/y.csv"` on the fine scale is modeled by a HHMM
   - `data_source = c(NA,"y")` is interpreted as `data_source = c("y","y")`
- `fix_dfs`: a numeric vector of length 2, fixing the degrees of freedom of the state-dependent t-distributions
   - if `fix_dfs = c(NA,NA)`, degrees of freedom are not fixed
   - if `fix_dfs = c(x,NA)`, degrees of freedom of a HMM or the coarse scale of a HHMM are fixed to `x`
   - if `fix_dfs = c(NA,y)`, degrees of freedom of the fine scale of a HHMM are fixed to `y`
   - if `fix_dfs = c(x,y)`, degrees of freedom of the coarse scale and the fine scale of a HHMM are fixed to `x` and `y`, respectively 
- `iterlim`: an integer, specifying the maximum number of optimization iterations to be performed before termination, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `overwrite`: a boolean, determining whether existing results (on the same `id`) can be overwritten
- `print.level`: an integer, determining the level of printing during the optimization, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `runs`: an integer, setting the number of optimization runs
- `seed`: an integer, setting a seed for the simulation and the initialization
- `steptol`: an integer, setting the minimum allowable relative step length during the optimization, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `time_horizon`: a numeric vector of length 2, determining the length of the time horizion(s) (If the model is a HHMM, its second entry must always be specified to set the length of the fine-scale chunks.)
- `truncate_data`: a vector of length 2, containing a lower and upper date limit (in format YYYY-MM-DD) to select a subset of the empirical data (neither, one or both limits can be specified)

### Default values
- `accept_codes = 1` (relative gradient is close to zero, current iterate is probably solution)
- `at_true = FALSE`
- `data_col = c(NA,NA)`
- `data_source = c(NA,NA)` 
- `fix_dfs = c(NA,NA)`
- `iterlim = 500`
- `overwrite = FALSE`
- `print.level = 0` (no printing occurs)
- `runs = 200`
- `seed` is not set
- `steptol = 1e-6`
- `time_horizon = c(NA,NA)`
- `truncate_data = c(NA,NA)`

## Outputs
The following model results are saved in the folder `./models`:
- `estimates.txt`, containing likelihood value, AIC and BIC values, exit code, number of iterations, computation time, true parameter values, estimates, gradient, Hessian
- `states.txt`, containing frequencies of the decoded states and a comparison between the true states and the predicted states
- `controls`, `data`, `decoding` and `fit`, .RData-files that can be reinitialized
- `lls.pdf`, a visualization of the log-likelihood values in the different estimation runs
- `pseudos.pdf`, a visualization of the pseudo-residuals
- `sdd.pdf`, a visualization of the state-dependent distributions
- `ts.pdf`, a visualization of the decoded time series

## Examples
### Fitting a 3-state HMM to the DAX closing prices from 2000 to 2020
Click [here](https://github.com/loelschlaeger/HHMM_Finance/tree/master/models/HMM_3_DAX) for the results.
```R
### 1. Initialization
source("init.R"); init()

### 2. Set and check controls
controls = list(
  id            = "HMM_DAX_3",        
  data_source   = c("dax",NA),
  data_col      = c("Close",NA)
  truncate_data = c("2000-01-03","2020-12-28"), 
  states        = c(3,0)
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
