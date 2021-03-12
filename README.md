# fHMM <img src='sticker/StickerShadesOfBlue.png' align="right" height="136" />
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/fHMM)](https://www.r-pkg.org/badges/version-last-release/fHMM)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)

👉 Fitting (hierarchical) hidden Markov models (H)HMMs to financial data.

💬 Found a bug? Request for a feature? Please [tell us](https://github.com/loelschlaeger/fHMM/issues)!

## Table of contents
1. [Getting started](#getting-started)
2. [Data](#data)
3. [Specifying controls](#specifying-controls)
4. [Parameter structures](#parameter-structures)
5. [Outputs](#outputs)
6. [Debugging](#debugging)
7. [Examples](#examples)

## Getting started
Set the model's [controls](#specifying-controls) and execute `fit_hmm(controls)`. See below for [examples](#examples).

## Data
Download daily prices of your preferred stock from https://finance.yahoo.com/ via
```r 
download_data(name,symbol,from,to,path)
```
where
- `name` is your personal identifier,
- `symbol` is the stock's symbol,
- `from` and `to` define the time interval (in format `"YYYY-MM-DD"`),
- `path` is the path where the data gets saved.

Historical events can be highlighted in the visualization of the decoded, empirical time series by passing a named list `events` with elements `dates` (a vector of dates) and `names` (a vector of names for the events) to `fit_hmm`.

If you do not specify the `source` parameter in the model's `controls`, data is simulated. You can specify the model coefficients by passing the list `sim_par` in [thetaList](#parameter-structures) format to `fit_hmm`. Otherwise, the parameters are randomly drawn from the ranges -1 to 1 for expected values of a t-distribution, 0 to 1 for expected values of a gamma-distribution and 0 to 1 for standard deviations. Setting `scale_par(x,y)` in `controls` scales these values by `x` and `y` on the coarse scale and on the fine scale, respectively.

## Specifying controls
Specify your model by setting the following parameters of the named list `controls` and passing it to `fit_hmm`:
- `path`: A character, setting the path of the data and the model results.
- `id`: A character, identifying the model.
- `states`: A numeric vector of length 2, determining the model type and the number of states:
   - If `states = c(x,0)`, a HMM with `x` states is estimated.
   - If `states = c(x,y)`, a HHMM with `x` coarse-scale and `y` fine-scale states is estimated.
- `sdds`: A character vector of length 2, specifying the state-dependent distributions for both scales:
   - `"t"`, the t-distribution
   - `"t(x)"`, the t-distribution with `x` fixed degrees of freedom (`x = Inf` is allowed)
   - `"gamma"`, the gamma distribution
- `horizon`: A vector of length 2, determining the length of the time horizion(s). The first entry is numeric and mandatory if data is simulated. The second entry is mandatory if the model is a HHMM and can be either numeric or one of
   - `"w"` for weekly fine-scale chunks,
   - `"m"` for monthly fine-scale chunks,
   - `"q"` for quarterly fine-scale chunks,
   - `"y"` for yearly fine-scale chunks.

The following parameters are optional and set to [default values](#default-values) if you do not specify them:
- `data`: A list, containing
   - `soure`: A character vector of length 2, containing the file names of the empirical data:
      - If `source = c(NA,NA)`, data is simulated.
      - If `source = c("x",NA)`, data "x.csv" in folder "`path`/data" is modeled by a HMM.
      - If `source = c("x","y")`, data "x.csv" (type determined by `cs_type`) on the coarse scale and data "y.csv" in folder "`path`/data" on the fine scale is modeled by a HHMM.
   - `col`: A character vector of length 2, containing the names of the desired column of `source` for both scales.
   - `truncate`: A vector of length 2, containing lower and upper date limits (each in format `"YYYY-MM-DD"`) to select a subset of the empirical data (neither, one or both limits can be specified).
   - `cs_type`: A character, determining the type of empirical coarse-scale data in HHMMs, one of
      - `"mean"`: means of the fine-scale data,
      - `"mean_abs"`: means of the fine-scale data in absolute value,
      - `"sum_abs"`: sums of fine-scale data in absolute value.
- `fit`: A list, containing
   - `runs`: A numeric, setting the number of optimization runs.
   - `at_true`: A boolean, determining whether the optimization is initialised at the true parameter values. Only for simulated data, sets `runs = 1` and `accept = "all"`.
   - `seed`: A numeric, setting a seed for the simulation and the optimization.
   - `accept`: Either a numeric vector (containing acceptable exit codes of the [nlm optimization](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)) or the character `"all"` (accepting all codes).
   - `print.level`: Passed on to [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html).
   - `gradtol`: Passed on to [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html).
   - `stepmax`: Passed on to [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html).
   - `steptol`: Passed on to [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html).
   - `iterlim`: Passed on to [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html).
   - `scale_par`: A positive numeric vector of length two, scaling the model parameters in a simulation on the coarse and fine scale, respectively.
- `results`: A list, containing
   - `overwrite`: A boolean, determining whether overwriting of existing results (on the same `id`) is allowed. Set to `TRUE` if `id = "test"`.
   - `ci_level`: A numeric value between 0 and 1, setting the confidence interval level.

### Default values
- `accept = c(1,2)` 
- `at_true = FALSE`
- `ci_level = 0.95`
- `col = c(NA,NA)`
- `cs_type = NA` 
- `gradtol = 1e-3`
- `iterlim = 200`
- `overwrite = FALSE`
- `print_level = 0`
- `runs = 100`
- `scale_par = c(1,1)`
- `seed` is not set
- `source = c(NA,NA)`
- `stepmax = 1` 
- `steptol = 1e-3`
- `truncate = c(NA,NA)`

## Parameter structures
Four types of model parameters are estimated:
1. non-diagonal elements (column-wise) `gammas` of transition probability matrices `Gamma`,
2. expected values `mus`,
3. standard deviations `sigmas`,
4. degrees of freedom `dfs`.

All of these parameters have to fulfill constraints. Constrained parameters get the suffix `Con`, unconstrained parameters the suffix `Uncon`. Fine-scale parameters additionally get the suffix `_star`. Internally, collections of model parameters are processed using the following structures:
- `thetaFull`: A named list of all unconstrained model parameters.
- `thetaUncon`: A vector of all unconstrained model parameters to be estimated (in the above order).
- `thetaCon`: Constrained elements of `thetaUncon`.
- `thetaUnconSplit`: Splitted `thetaUncon` by fine-scale models.
- `thetaListOrdered`: `thetaList` in ordered form with respect to estimated expected values.

The code provides functions to transform these structures. Their names follow the logic `x2y`, where `x` and `y` are two structures.  

## Outputs
The following model results are saved in the folder `path/models/id` (`path` and `id` specified in `controls`):
- `estimates.txt`: Containing the model's likelihood value, AIC and BIC values, exit code, number of iterations, estimated and true parameters (only for simulated data), relaltive bias (only for simulated data) and confidence intervals.
- `protocol.txt`: Containing a protocol of the estimation.
- `states.txt`: Containing frequencies of the decoded states and (only for simulated data) a comparison between the true states and the predicted states.
- `log_likelihoods.pdf`: A visualization of the log-likelihood values in the different estimation runs.
- `pseudo_residuals.pdf`: A visualization of the pseudo-residuals along with a [Jarque–Bera test](https://en.wikipedia.org/wiki/Jarque%E2%80%93Bera_test) result on their normality.
- `state_dependent_distributions.pdf`: A visualization of the estimated state-dependent distributions along with (in case of simulated data) the true state-dependent distributions.
- `decoded_time_series.pdf`: A visualization of the decoded time series with (in case of empirical data) markings for the entries in `events`.
- `controls.rds`, `data.rds`, `decoding.rds`, `events.rds`, `fit.rds` and `pseudos.rds`: Restore the fitting steps.
   
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
