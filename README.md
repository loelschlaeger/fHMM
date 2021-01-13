# HHMM_Finance
This repository provides R and C++ code for fitting (hierarchical) hidden Markov models (H)HMMs to financial data.

## Contained files
- `checks.R` provides validation functions.
- `conf.R` computes confidence intervals for the estimates.
- `data.R` processes, simulates and downloads [data](#data).
- `init.R` initializes the code and the estimation routine.
- `loglike.cpp` computes the model's log-likelihood.
- `loop.R` loops over multiple [model specifications](#specifying-controls).
- `main.R` presents the code's [workflow](#getting-started).
- `optim.R` maximizes the log-likelihood function using [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html).
- `trans.R` contains helper functions for parameter transformations.
- `visual.R` generates visualisations of the [model results](#outputs).
- `viterbi.R` performs state decoding based on the [Viterbi algorithm](https://en.wikipedia.org/wiki/Viterbi_algorithm).

## Getting started
0. Go to `main.R`.
1. Run code chunk 1 to initialize the code.
2. Run code chunk 2 to download [data](#data). (optional)
3. Run code chunk 3 to set and check the model's [controls](#specifying-controls).
4. Run code chunk 4 to fit the model.
5. Run code chunk 5 to decode the hidden states.
6. Run code chunk 6 to visualize the [results](#outputs). 
7. Run code chunk 7 to reinitialize an old model. (optional)

See below for some [examples](#examples).

## Data
The code is intended to be used on daily share prices provided by https://finance.yahoo.com/. The data must be in csv-format and must contain a column named "Date". Data can be saved in the folder `"./data"` or downloaded automatically via the function `download_data(name,symbol,from,to)`, where
- `name` is a personal identifier,
- `symbol` is the stock's symbol,
- `from` and `to` define the time interval (in format `"YYYY-MM-DD"`).

Additionally, data can be simulated.

## Specifying controls
A model is specified by setting parameters of the named list `controls`. The following parameters are mandatory:
- `id`: a character, identifying the model
- `states`: a numeric vector of length 2, determining the model type and the number of states:
   - if `states = c(x,0)`, a HMM with `x` states is estimated
   - if `states = c(x,y)`, a HHMM with `x` coarse-scale and `y` fine-scale states is estimated
- `sdds`: a character vector of length 2, specifying the state-dependent distributions:
   - `"t"`, the t-distribution
   - `"t(x)"`, the t-distribution with `x` fixed degrees of freedom
   - `norm`, the normal distribution (which internally is treated as `t(Inf)`)
   - `gamma`, the gamma distribution (not supported yet)
- either `data_source` along with `data_col` (for empirical data) or `time_horizon` (for simulated data), see below

The following parameters are optional and set to [default values](#default-values) if not specified:
- `accept_codes`: either a numeric vector (containing acceptable exit codes of the optimization) or the character `"all"` (accepting all codes), see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `at_true`: a boolean, determining whether the optimization is initialised at the true parameter values (only for simulated data, sets `runs = 1` and `accept_codes = "all"`)
- `data_col`: a character vector of length 2, containing names of the desired columns of `data_source`
- `data_cs_type`: a character, determining the type of the coarse scale data:
   - if `data_cs_type = mean`, means of the fine scale data are chosen
   - if `data_cs_type = mean_abs`, means of the fine scale data in absolute value are chosen
   - if `data_cs_type = sum_abs`, sums of fine scale data in absolute value are chosen
- `data_source`: a character vector of length 2, containing the file names of the empirical data:
   - if `data_source = c(NA,NA)`, data is simulated
   - if `data_source = c("x",NA)`, data `"./data/x.csv"` is modeled by a HMM
   - if `data_source = c("x","y")`, data `"./data/x.csv"` (type determined by `data_cs_type`) on the coarse scale and data `"./data/y.csv"` on the fine scale is modeled by a HHMM
   - `data_source = c(NA,"y")` is interpreted as `data_source = c("y","y")`
- `iterlim`: an integer, specifying the maximum number of optimization iterations to be performed before termination, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `overwrite`: a boolean, determining whether overwriting of existing results (on the same `id`) is allowed
- `print_level`: an integer, determining the level of printing during the optimization, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `runs`: an integer, setting the number of optimization runs
- `seed`: an integer, setting a seed for the simulation and the optimization
- `steptol`: an integer, setting the minimum allowable relative step length during the optimization, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `time_horizon`: a numeric vector of length 2, determining the length of the time horizion(s) (second entry is mandatory if the model is a HHMM)
- `truncate_data`: a vector of length 2, containing lower and upper date limits (each in format `"YYYY-MM-DD"`) to select a subset of the empirical data (neither, one or both limits can be specified)

### Default values
- `accept_codes = 1` (relative gradient is close to zero)
- `at_true = FALSE`
- `data_cs_type = mean_abs`
- `data_col = c(NA,NA)`
- `data_source = c(NA,NA)` 
- `iterlim = 500`
- `overwrite = FALSE`
- `print_level = 0` (no printing)
- `runs = 200`
- `seed` is not set
- `steptol = 1e-6`
- `time_horizon = c(NA,NA)`
- `truncate_data = c(NA,NA)`

## Outputs
The following model results are saved in the folder `./models`:
- `estimates.txt`, containing likelihood value, AIC and BIC values, exit code, number of iterations, computation time, true parameter values, estimates, gradient, Hessian
- `states.txt`, containing frequencies of the decoded states and (in case of simulated data) a comparison between the true states and the predicted states
- `controls`, `data`, `decoding` and `fit`, .RData-files that can be reinitialized
- `lls.pdf`, a visualization of the log-likelihood values in the different estimation runs
- `pseudos.pdf`, a visualization of the pseudo-residuals together with a test result on their normality
- `sdd.pdf`, a visualization of the estimated state-dependent distributions
- `ts.pdf`, a visualization of the decoded time series

## Examples
### Fitting a 3-state HMM to the DAX closing prices from 2000 to 2020
Click [here](https://github.com/loelschlaeger/HHMM_Finance/tree/master/models/HMM_3_DAX) for the results.
```R
### 1. Initialization
source("init.R"); init()

### 2. Download data
download_data("dax","^GDAXI")

### 3. Set and check controls
controls = list(
  id            = "HMM_3_DAX", 
  sdds          = c("t",NA),
  states        = c(3,0),
  data_source   = c("dax",NA),
  data_col      = c("Close",NA),
  truncate_data = c("2000-01-03","2020-12-30"), 
  )
controls = check_controls(controls)

### 4. Fit model to data
data = get_data(controls)
fit = max_likelihood(data,controls)

### 5. Decode hidden states
decoding = apply_viterbi(data,fit,controls)

### 6. Visualize results
labels = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
  )
visual(data,fit,decoding,controls,labels)
```
