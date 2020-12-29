# HHMM_Finance
This repository provides R and C++ code for fitting (hierarchical) hidden Markov models (H)HMMs to [financial data](#data). 

## Data
The code is designed for closing prices of financial time series provided by https://finance.yahoo.com/. The data must be in csv-format, containing columns named "Date" and "Close", and must be saved in the folder `"./data/"`. Additionally, data can be simulated.

## Getting started
The file `main.R` presents the workflow:
1. Run code chunk 1 to initialize the code. Two paths are printed, first where you have to provide the [data](#data) and second where the [results](#results) are saved.
2. Run code chunk 2 to set and check the model's [controls](#controls) `controls`. You can only proceed if `check_controls(controls)` is successfully called.
3. Run code chunk 3 to fit the model to the [data](#data).
4. Run code chunk 4 to decode the hidden states.
5. Run code chunk 5 to visualize the [results](#results). 
6. Optionally run code chunk 6 to reinitialize an old model.

## Controls
The model is specified by defining a named list `controls`. The following parameters of `controls` are mandatory:
- `model_name`: character, identifying the model
- `states`: numeric vector of length 2, determining the model and the number of states:
   - if `states = c(x,0)`, a HMM with `x` states is estimated
   - if `states = c(x,y)`, a HHMM with `x` coarse-scale and `y` fine-scale states is estimated
- `time_horizon`: numeric vector of length 2, first and second entry determining the length of the coarse-scale and fine-scale time horizion, respectively

The following parameters of `controls` are optional and set to default values, if not specified:
- `accept_codes`: numeric vector, containing exit codes of the optimization to accept, see [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `data_source`: numeric vector of length 2, containing the file names of the financial data:
   - if `data_source = c(NA,NA)`, data is simulated
   - if `data_source = c("x.csv",NA)`, use `"./data/x.csv"` for a HMM
   - if `data_source = c("x.csv","y.csv")`, use averages of data `"./data/x.csv"` (size determined by the second entry of `time_horizon`) for the coarse scale and data `"./data/y.csv"` for the fine scale, respectively, of a HHMM
   - if `data_source = c(NA,"y.csv")`, this is interpreted as `data_source = c("y.csv","y.csv")`
- `fix_dfs`: numeric vector of length 2, fixing the degrees of freedom of the state-dependent t-distributions
   - if `fix_dfs = c(NA,NA)`, degrees of freedom are estimated
   - if `fix_dfs = c(x,NA)`, degrees of freedom of a HMM or the coarse scale of a HHMM are fixed to `x`
   - if `fix_dfs = c(NA,y)`, degrees of freedom of the fine scale of a HHMM are fixed to `y`
   - if `fix_dfs = c(x,y)`, degrees of freedom of the coarse scale and the fine scale of a HHMM are fixed to `x` and `y`, respectively 
- `hessian`: boolean, determining wheter the Hessian should be computed
- `iterlim`: integer, specifying the maximum number of optimization iterations to be performed before termination, see [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `overwrite`: boolean, determining wheter existing results can be overwritten
- `print.level`: integer, determining the level of printing which is done during the optimization process, see [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `runs`: integer, number of runs for the optimization
- `seed`: integer, setting a seed for the simulation and initialization
- `steptol`: integer, providing the minimum allowable relative step length during the optimization process, see [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `truncate_data`: vector of length 2 with dates or `NA`, specifying truncation points when working with empirical data

## Files
- `checks.R` contains several validation functions.
- `data.R` processes or simulates [data](#data).
- `init.R` initializes the code and the estimation routine.
- `loglike.cpp` computes the model's log-likelihood.
- `main.R` presents the code's workflow.
- `optim.R` maximizes the log-likelihood function using the [R function nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html).
- `trans.R` contains helper functions for parameter transformations.
- `visual.R` generates visualisations of the [model results](#results).
- `viterbi.R` performes state decoding based on the Viterbi algorithm.

## Results
The following model results are saved in `./models/model_name/`:
- `estimates.txt` containes estimates, gradient, Hessian, likelihood value, AIC and BIC values etc.
- `controls`, `data`, `decoding` and `fit` are .RData-files and can be reinitialized
- `pseudos.pdf` is a visualization of the pseudo-residuals
- `sdd.pdf` is a visualization of the state-dependent distributions
- `ts.pdf` is a visualization of the decoded time series (only for empirical data)
