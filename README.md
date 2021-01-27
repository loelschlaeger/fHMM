# HHMM_Finance
This repository provides R and C++ code for fitting (hierarchical) hidden Markov models (H)HMMs to financial data.

## Contained files
- `checks.R` provides validation functions.
- `conf.R` computes confidence intervals for the estimates.
- `data.R` processes, simulates and downloads [data](#data).
- `init.R` initializes the code.
- `loglike.cpp` computes the model's log-likelihood.
- `main.R` presents the code's [workflow](#getting-started).
- `optim.R` maximizes the log-likelihood function using [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html).
- `trans.R` contains helper functions for parameter transformations between different [parameter structures](#parameter-structures).
- `visual.R` generates visualisations of the [model results](#outputs).
- `viterbi.R` performs state decoding based on the [Viterbi algorithm](https://en.wikipedia.org/wiki/Viterbi_algorithm).

## Getting started
0. Go to `main.R`.
1. Run code chunk 1 to initialize the code.
2. Run code chunk 2 to download [data](#data). (optional)
3. Run code chunk 3 to set the model's [controls](#specifying-controls).
4. Run code chunk 4 to define [events](#events). (optional)
5. Execute `hhmmf(id,controls,events,warn,scale_par,sim_par)`, where
   - `id` is a character identifying the model (default is `id = "test"`),
   - `controls` is the list of controls defined in step 3,
   - `events` is the list of events defined in step 4,
   - `warn` sets the handling of warning messages, see the [R options manual](https://stat.ethz.ch/R-manual/R-devel/library/base/html/options.html) (default is `warn = 0`),
   - `scale_par` is a float scaling the model parameters (default is `scale_par = 0.1`),
   - `sim_par` is a [thetaUncon](#parameter-structures)-object specifying model parameters for a simulation (optional).

See below for [examples](#examples).

## Data
The code is intended to be used on daily share prices provided by https://finance.yahoo.com/. The data must be in csv-format and must contain a column named "Date". Data can be saved in the folder `"./data"` or downloaded automatically via the function `download_data(name,symbol,from,to,show_symbols)`, where
- `name` is a personal identifier,
- `symbol` is the stock's symbol,
- `from` and `to` define the time interval (in format `"YYYY-MM-DD"`),
- `show_symbols = TRUE` prints all saved symbols.

Additionally, data can be simulated.

### Events
Events can be highlighted in the visualization of the decoded time series by passing a named list with elements `dates` (a vector of dates) and `names` (a vector of names for the events) to `hhmmf`.

## Specifying controls
A model is specified by setting parameters of the named list `controls`. The following parameters are mandatory:
- `states`: a numeric vector of length 2, determining the model type and the number of states:
   - if `states = c(x,0)`, a HMM with `x` states is estimated
   - if `states = c(x,y)`, a HHMM with `x` coarse-scale and `y` fine-scale states is estimated
- `sdds`: a character vector of length 2, specifying the state-dependent distributions:
   - `"t"`, the t-distribution
   - `"t(x)"`, the t-distribution with `x` fixed degrees of freedom (normal distribution is obtained by setting `x = Inf`)
   - `"gamma"`, the gamma distribution
- `data_cs_type` (only for a HHMM and empirical data): a character, determining the type of the coarse scale data:
   - if `data_cs_type = "mean"`, means of the fine scale data are chosen
   - if `data_cs_type = "mean_abs"`, means of the fine scale data in absolute value are chosen
   - if `data_cs_type = "sum_abs"`, sums of fine scale data in absolute value are chosen
- either `data_source` along with `data_col` (for empirical data) or `time_horizon` (for simulated data), see below

The following parameters are optional and set to [default values](#default-values) if not specified:
- `accept_codes`: either a numeric vector (containing acceptable exit codes of the optimization) or the character `"all"` (accepting all codes), see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `at_true`: a boolean, determining whether the optimization is initialised at the true parameter values (only for simulated data, sets `runs = 1` and `accept_codes = "all"`)
- `data_col`: a character vector of length 2, containing names of the desired columns of `data_source`
- `data_source`: a character vector of length 2, containing the file names of the empirical data:
   - if `data_source = c(NA,NA)`, data is simulated
   - if `data_source = c("x",NA)`, data `"./data/x.csv"` is modeled by a HMM
   - if `data_source = c("x","y")`, data `"./data/x.csv"` (type determined by `data_cs_type`) on the coarse scale and data `"./data/y.csv"` on the fine scale is modeled by a HHMM
- `gradtol`: a positive scalar, giving the tolerance at which the scaled gradient is considered close enough to zero , see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `iterlim`: an integer, specifying the maximum number of optimization iterations to be performed before termination, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `overwrite`: a boolean, determining whether overwriting of existing results (on the same `id`) is allowed, set to `TRUE` if `id = "test"`
- `print_level`: an integer, determining the level of printing during the optimization, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `runs`: an integer, setting the number of optimization runs
- `seed`: an integer, setting a seed for the simulation and the optimization
- `steptol`: an integer, setting the minimum allowable relative step length during the optimization, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `time_horizon`: a vector of length 2, determining the length of the time horizion(s), first entry is mandatory if data is simulated, second entry is mandatory if the model is a HHMM and can be one of
   - `x`, where `x` is an integer
   - `"w"` for weekly coarse-scale observations
   - `"m"` for weekly coarse-scale observations
   - `"q"` for weekly coarse-scale observations
   - `"y"` for weekly coarse-scale observations
- `truncate_data`: a vector of length 2, containing lower and upper date limits (each in format `"YYYY-MM-DD"`) to select a subset of the empirical data (neither, one or both limits can be specified)

### Default values
- `accept_codes = 1` (relative gradient is close to zero)
- `at_true = FALSE`
- `data_col = c(NA,NA)`
- `data_source = c(NA,NA)` 
- `gradtol = 1e-5`
- `iterlim = 500`
- `overwrite = FALSE`
- `print_level = 0` (no printing)
- `runs = 100`
- `seed` is not set
- `steptol = 1e-5`
- `time_horizon = c(NA,NA)`
- `truncate_data = c(NA,NA)`

## Parameter structures
Internally, model parameters are processed using three structures:
- `thetaFull`, a named list of all model parameters
- `thetaUncon`: a vector of all unconstrained model parameters to be estimated 
- `thetaCon`: a vector of all constrained model parameters to be estimated 

## Outputs
The following model results are saved in the folder `./models/id`:
- txt-files:
   - `comparison.txt` (only for simulated data), comparing true and estimated parameters
   - `estimates.txt`, containing likelihood value, AIC and BIC values, exit code, number of iterations, computation time, true parameter values, estimates, gradient, Hessian
   - `protocol.txt`, containing a protocol of the estimation,
   - `states.txt`, containing frequencies of the decoded states and (in case of simulated data) a comparison between the true states and the predicted states
- pdf-files:
   - `lls.pdf`, a visualization of the log-likelihood values in the different estimation runs
   - `prs.pdf`, a visualization of the pseudo-residuals together with a test result on their normality
   - `sdds.pdf`, a visualization of the estimated state-dependent distributions and (in case of simulated data) the true state-dependent distributions
   - `ts.pdf`, a visualization of the decoded time series with (in case of empirical data) markings for the entries in `events`
- rds-files:
   - `controls`, `data`, `decoding`, `events`, `fit` and `pseudos` (to analyse and further process the model results)

## Examples
### Fitting a 3-state HMM to the DAX closing prices from 2000 to 2020 using t-distributions
Click [here](https://github.com/loelschlaeger/HHMM_Finance/tree/master/models/HMM_3_DAX) for the results.
```R
### 1. Initialize code
source("init.R"); load_code()

### 2. Download data (optional)
download_data("dax","^GDAXI")

### 3. Set controls
controls = list(
  sdds          = c("t",NA),
  states        = c(3,0),
  data_source   = c("dax",NA),
  data_col      = c("Close",NA),
  truncate_data = c("2000-01-03","2020-12-30")
)

### 4. Define events (optional)
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### 5. Fit (H)HMM
hhmmf(id       = "HMM_3_DAX",
      controls = controls,
      events   = events)
```
