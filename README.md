# fHMM <img src='sticker/StickerShadesOfBlue.png' align="right" height="136" />
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/fHMM)](https://www.r-pkg.org/badges/version-last-release/fHMM)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)

Fitting (hierarchical) hidden Markov models (H)HMMs to financial data

Found a bug? Please fill an issue.

## Table of contents
1. [Getting started](#getting-started)
2. [Data](#data)
3. [Specifying controls](#specifying-controls)
4. [Parameter structures](#parameter-structures)
5. [Outputs](#outputs)
6. [Debugging](#debugging)
7. [Examples](#examples)

## Getting started
Set the model's [controls](#specifying-controls) and execute
```r 
fit_hmm(controls,events=NULL,sim_par=NULL)
``` 
where
- `controls` is the list of controls defined in step 2,
- `events` is a list of [events](#events) (optional),
- `sim_par` is a [thetaList](#parameter-structures)-object specifying model parameters for a simulation (optional).
   
See below for [examples](#examples).

## Data
The code is intended to be used on daily share prices provided by https://finance.yahoo.com/ or simulated data. 

### Empirical data
Empirical data must be in csv-format and must contain a column named "Date" and a named column of daily share prices. Such data files can be saved in the folder `"path/data"` (where `path` is specified in `controls`) or downloaded automatically via the function 
```r 
download_data(name=NULL,symbol=NULL,from=as.Date("1902-01-01"),to=Sys.Date(),show_symbols=FALSE,path)
```
where
- `name` is a personal identifier (optional),
- `symbol` is the stock's symbol (optional),
- `from` and `to` define the time interval (in format `"YYYY-MM-DD"`, optional),
- `show_symbols = TRUE` prints all saved symbols (optional),
- `path` is the data saving path.

### Simulated data
Data can be simulated, e.g. for bootstrapping. The model parameters can be either
1. specified by passing `sim_par` to `fit_hmm` or 
2. randomly drawn. In this case, model paramters are drawn from the ranges -1 to 1 for expected values of a t-distribution, 0 to 1 for expected values of a gamma-distribution and 0 to 1 for standard deviations. Setting `scale_par(x,y)` in `controls` scales these values by `x` and `y` on the coarse scale and on the fine scale, respectively.

### Events
Historical events can be highlighted in the visualization of the decoded, empirical time series by passing a named list with elements `dates` (a vector of dates) and `names` (a vector of names for the events) to `fit_hmm`.

## Specifying controls
A model is specified by setting parameters of the named list `controls` and passing it to `fit_hmm`. The following parameters are mandatory:
- `id`: a character, identifying the model
- `path`: a character, setting the path of the data and the model results
- `states`: a numeric vector of length 2, determining the model type and the number of states:
   - if `states = c(x,0)`, a HMM with `x` states is estimated
   - if `states = c(x,y)`, a HHMM with `x` coarse-scale and `y` fine-scale states is estimated
- `sdds`: a character vector of length 2, specifying the state-dependent distributions for both scales:
   - `"t"`, the t-distribution
   - `"t(x)"`, the t-distribution with `x` fixed degrees of freedom (normal distribution is obtained by setting `x = Inf`)
   - `"gamma"`, the gamma distribution
- either `data_source` along with `data_col` (for empirical data) or `time_horizon` (for simulated data), see below

The following parameters are optional and set to [default values](#default-values) if not specified:
- `accept_codes`: either a numeric vector (containing acceptable exit codes of the optimization) or the character `"all"` (accepting all codes), see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `at_true`: a boolean, determining whether the optimization is initialised at the true parameter values (only for simulated data, sets `runs = 1` and `accept_codes = "all"`)
- `ci_level`: a scalar between 0 and 1, setting the confidence interval level
- `data_col`: a character vector of length 2, containing the name of the desired column of `data_source` for both scales
- `data_cs_type` (only for a HHMM and empirical data): a character, determining the type of coarse-scale data:
   - `data_cs_type = "mean"`: means of the fine-scale data
   - `data_cs_type = "mean_abs"`: means of the fine-scale data in absolute value
   - `data_cs_type = "sum_abs"`: sums of fine-scale data in absolute value
- `data_source`: a character vector of length 2, containing the file names of the empirical data:
   - if `data_source = c(NA,NA)`, data is simulated
   - if `data_source = c("x",NA)`, data `"./data/x.csv"` is modeled by a HMM
   - if `data_source = c("x","y")`, data `"./data/x.csv"` (type determined by `data_cs_type`) on the coarse scale and data `"./data/y.csv"` on the fine scale is modeled by a HHMM
- `gradtol`: a positive scalar, giving the tolerance at which the scaled gradient is considered close enough to zero , see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `iterlim`: an integer, specifying the maximum number of optimization iterations to be performed before termination, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `overwrite`: a boolean, determining whether overwriting of existing results (on the same `id`) is allowed, set to `TRUE` if `id = "test"`
- `print_level`: an integer, determining the level of printing during the optimization, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `runs`: an integer, setting the number of optimization runs
- `scale_par`: a positive numeric vector of length two, [scaling the model parameters](#simulated-data) in a simulation on the coarse and fine scale, respectively
- `seed`: an integer, setting a seed for the simulation and the optimization
- `steptol`: a positive scalar, setting the minimum allowable relative step length during the optimization, see the [nlm manual](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `time_horizon`: a vector of length 2 (first entry is mandatory if data is simulated, second entry is mandatory if the model is a HHMM), determining the length of the time horizion(s) and can be one of
   - `x`, where `x` is an integer
   - `"w"` for weekly coarse-scale observations
   - `"m"` for monthly coarse-scale observations
   - `"q"` for quarterly coarse-scale observations
   - `"y"` for yearly coarse-scale observations
- `truncate_data`: a vector of length 2, containing lower and upper date limits (each in format `"YYYY-MM-DD"`) to select a subset of the empirical data (neither, one or both limits can be specified)

### Default values
- `accept_codes = c(1,2)` 
- `at_true = FALSE`
- `ci_level = 0.95`
- `data_col = c(NA,NA)`
- `data_cs_type = mean_abs` 
- `data_source = c(NA,NA)` 
- `gradtol = 1e-4`
- `iterlim = 500`
- `overwrite = FALSE`
- `print_level = 0`
- `runs = 100`
- `scale_par = c(1,1)`
- `seed` is not set
- `steptol = 1e-2`
- `time_horizon = c(NA,NA)`
- `truncate_data = c(NA,NA)`

## Parameter structures
The code estimates four types of model parameters:
1. non-diagonal elements (column-wise) `gammas` of transition probability matrices `Gamma`,
2. expected values `mus`,
3. standard deviations `sigmas`,
4. degrees of freedom `dfs`.

All of these parameters have to fulfill constraints. Constrained parameters get the suffix `Con`, unconstrained parameters the suffix `Uncon`. Fine-scale parameters additionally get the suffix `_star`. Internally, collections of model parameters are processed using the following structures:
- `thetaFull`: a named list of all unconstrained model parameters,
- `thetaUncon`: a vector of all unconstrained model parameters to be estimated (in the above order),
- `thetaCon`: constrained elements of `thetaUncon`,
- `thetaUnconSplit`: splitted `thetaUncon` by fine-scale models,
- `thetaListOrdered`: `thetaList` in ordered form with respect to estimated expected values.

The code provides functions to transform structures. Their names follow the logic `x2y`, where `x` and `y` are two structures.  

## Outputs
The following model results are saved in the folder `path/models/id` (`path` and `id` specified in `controls`):
- `estimates.txt`: containing the model's likelihood value, AIC and BIC values, exit code, number of iterations, estimated and true parameters (only for simulated data), relaltive bias (only for simulated data) and confidence intervals
- `protocol.txt`: containing a protocol of the estimation
- `states.txt`: containing frequencies of the decoded states and (only for simulated data) a comparison between the true states and the predicted states
- `log_likelihoods.pdf`: a visualization of the log-likelihood values in the different estimation runs
- `pseudo_residuals.pdf`: a visualization of the pseudo-residuals along with a [Jarque–Bera test](https://en.wikipedia.org/wiki/Jarque%E2%80%93Bera_test) result on their normality
- `state_dependent_distributions.pdf`: a visualization of the estimated state-dependent distributions and (in case of simulated data) the true state-dependent distributions
- `decoded_time_series.pdf`: a visualization of the decoded time series with (in case of empirical data) markings for the entries in `events`
- `controls.rds`, `data.rds`, `decoding.rds`, `events.rds`, `fit.rds` and `pseudos.rds` (to analyse and further process the model results)
   
## Debugging
Some error or warning messages provide exception codes. Calling `exception(code)` yields suggestions for debugging.

## Examples
### Fitting a 3-state HMM to the DAX closing prices from 2000 to 2020 using t-distributions
Click [here](https://github.com/loelschlaeger/fHMM/tree/master/models/HMM_3_DAX) for the results.
```R
### Initialize code
source("load_code.R")

### Download data 
download_data("dax","^GDAXI",path=".")

### Set and check controls
controls = list(
  path          = ".",
  id            = "HMM_3_DAX",
  sdds          = c("t",NA),
  states        = c(3,0),
  data_source   = c("dax",NA),
  data_col      = c("Close",NA),
  truncate_data = c("2000-01-03","2020-12-30")
)

### Define events 
events = list(
  dates = c("2001-09-11","2008-09-15","2020-01-27"),
  names = c("9/11 terrorist attack","Bankruptcy of Lehman Brothers","First COVID-19 case in Germany")
)

### Fit (H)HMM
fit_hmm(controls,events)
```
### Fitting a 2-state HMM to simulated data using gamma-distributions
Click [here](https://github.com/loelschlaeger/fHMM/tree/master/models/HMM_2_sim_gamma) for the results.
```R
### Initialize code
source("load_code.R")

### Set and check controls
controls = list(
  path          = ".",
  id            = "HMM_2_sim_gamma",
  sdds          = c("gamma",NA),
  states        = c(2,0),
  time_horizon  = c(5000,NA),
  seed          = 1
)

### Fit (H)HMM
fit_hmm(controls)
```
