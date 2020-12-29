# HHMM_Finance
This repository provides R and C++ code for fitting (hierarchical) hidden Markov models (H)HMMs to [financial data](#data). 

## Data
The code is designed for closing prices of financial time series provided by https://finance.yahoo.com/. The data must be in csv-format, containing columns named "Date" and "Close". Additionally, data can be simulated.

## Getting started
The file `main.R` presents the workflow:
1. Run code chunk 1 to initialize the code. It prints the paths where you have to provide the [data](#data) and where the model results are saved.
2. Run code chunk 2 to set and check the model's [controls](#controls).
3. Run code chunk 3 to fit the model to the [data](#data).
4. Run code chunk 4 to decode the hidden states.
5. Run code chunk 5 to visualize the results. 
6. Optionally run code chunk 6 to reinitialize an old model.

## Controls
`controls` is a list, containing parameters for the model specification. The following parameters are mandatory:
- `model_name`
- `data_source`
The following parameters are optional:
- `data_source`

## Files
- `checks.R` contains several validation functions.
- `data.R` processes empirical data or simulates data.
- `init.R` initializes the code and the estimation routine.
- `loglike.cpp` computes the model's log-likelihood.
- `main.R` presents the code's workflow.
- `optim.R` maximizes the log-likelihood function using the R function `nlm`.
- `trans.R` contains helper functions for parameter transformations.
- `visual.R` generates visualisations of the model results.
- `viterbi.R` performes state decoding based on the Viterbi algorithm.

## Examples
