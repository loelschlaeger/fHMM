# HHMM_Finance
This repository provides R and C++ code for fitting (hierarchical) hidden Markov models (H)HMMs to [financial data](#data). 

## Data
The code is designed for closing prices of financial time series provided by https://finance.yahoo.com/. The data must be in csv-format, containing columns named "Date" and "Close". Additionally, data can be simulated.

## Getting started
The file `main.R` presents the workflow:
1. Run code chunk 1 to initialize the code. Two paths are printed where you have to provide the [data](#data) and where the [results](#results) are saved.
2. Run code chunk 2 to set and check the model's [controls](#controls).
3. Run code chunk 3 to fit the model to the [data](#data).
4. Run code chunk 4 to decode the hidden states.
5. Run code chunk 5 to visualize the [results](#results). 
6. Optionally run code chunk 6 to reinitialize an old model.

## Controls
The model is specified by defining a named list `controls`. The following parameters of `controls` are mandatory:
- `model_name`: character, identifying the model
- `states`: numeric vector of length 2, determining the number of states:
   - if `states = c(x,0)`, estimate HMM with `x`states
   - if `states = c(x,y)`, estimate HHMM with `x` coarse-scale and `y` fine-scale states
- `time_horizon`: numeric vector of length 2, first and second entry determining the length of the coarse-scale and fine-scale time horizion, respectively

The following parameters of `controls` are optional:
- `accept_codes`: numeric vector, containing exit codes of the optimization to accept, see [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `data_source`: 
- `fix_dfs`:
- `hessian`: boolean, determining wheter the Hessian should be computed
- `iterlim`: integer, specifying the maximum number of optimization iterations to be performed before termination, see [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `overwrite`: boolean, determining wheter existing results can be overwritten
- `print.level`: integer, determining the level of printing which is done during the optimization process, see [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `runs`: integer, number of runs for the optimization
- `seed`: integer, setting a seed for the simulation and initialization
- `steptol`: integer, providing the minimum allowable relative step length during the optimization process, see [nlm](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/nlm.html)
- `truncate_data`: 

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

## Example
