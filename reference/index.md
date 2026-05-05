# Package index

## Data preparation

Use these functions to prepare or simulate financial data.

- [`set_controls()`](https://loelschlaeger.de/fHMM/reference/set_controls.md)
  [`validate_controls()`](https://loelschlaeger.de/fHMM/reference/set_controls.md)
  [`print(`*`<fHMM_controls>`*`)`](https://loelschlaeger.de/fHMM/reference/set_controls.md)
  [`summary(`*`<fHMM_controls>`*`)`](https://loelschlaeger.de/fHMM/reference/set_controls.md)
  : Define and validate model specifications

- [`download_data()`](https://loelschlaeger.de/fHMM/reference/download_data.md)
  : Download financial data from Yahoo Finance

- [`simulate_hmm()`](https://loelschlaeger.de/fHMM/reference/simulate_hmm.md)
  : Simulate data

- [`prepare_data()`](https://loelschlaeger.de/fHMM/reference/prepare_data.md)
  : Prepare data

- [`fHMM_data()`](https://loelschlaeger.de/fHMM/reference/fHMM_data.md)
  [`print(`*`<fHMM_data>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_data.md)
  [`summary(`*`<fHMM_data>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_data.md)
  :

  Constructor of an `fHMM_data` object

- [`plot(`*`<fHMM_data>`*`)`](https://loelschlaeger.de/fHMM/reference/plot.fHMM_data.md)
  :

  Plot method for an object of class `fHMM_data`

## Model parameters

Use these functions to define and transform model parameters.

- [`fHMM_parameters()`](https://loelschlaeger.de/fHMM/reference/fHMM_parameters.md)
  [`print(`*`<fHMM_parameters>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_parameters.md)
  : Set and check model parameters
- [`par2parUncon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`parUncon2parCon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`parCon2par()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`par2parCon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`parCon2parUncon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`parUncon2par()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`muCon2muUncon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`muUncon2muCon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`sigmaCon2sigmaUncon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`sigmaUncon2sigmaCon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`dfCon2dfUncon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`dfUncon2dfCon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`Gamma2gammasCon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`Gamma2gammasUncon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`gammasCon2Gamma()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`gammasCon2gammasUncon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`gammasUncon2Gamma()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  [`gammasUncon2gammasCon()`](https://loelschlaeger.de/fHMM/reference/parameter_transformations.md)
  : Parameter transformations

## Model estimation

Use these function for model estimation and state decoding.

- [`ll_hmm()`](https://loelschlaeger.de/fHMM/reference/ll_hmm.md) :
  Log-likelihood function of an (H)HMM
- [`fit_model()`](https://loelschlaeger.de/fHMM/reference/fit_model.md)
  : Model fitting
- [`fHMM_model()`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`print(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`residuals(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`summary(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`coef(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`AIC(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`BIC(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`nobs(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`logLik(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`npar()`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`predict(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  : Constructor of a model object
- [`decode_states()`](https://loelschlaeger.de/fHMM/reference/decode_states.md)
  [`viterbi()`](https://loelschlaeger.de/fHMM/reference/decode_states.md)
  : Decode the underlying hidden state sequence
- [`reorder_states()`](https://loelschlaeger.de/fHMM/reference/reorder_states.md)
  : Reorder estimated states

## Model evaluation

Use these functions to evaluate a fitted model.

- [`fHMM_model()`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`print(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`residuals(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`summary(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`coef(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`AIC(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`BIC(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`nobs(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`logLik(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`npar()`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  [`predict(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
  : Constructor of a model object

- [`compare_models()`](https://loelschlaeger.de/fHMM/reference/compare_models.md)
  : Compare multiple models

- [`compute_residuals()`](https://loelschlaeger.de/fHMM/reference/compute_residuals.md)
  : Compute (pseudo-) residuals

- [`plot(`*`<fHMM_model>`*`)`](https://loelschlaeger.de/fHMM/reference/plot.fHMM_model.md)
  :

  Plot method for an object of class `fHMM_model`

- [`fHMM_colors()`](https://loelschlaeger.de/fHMM/reference/fHMM_colors.md)
  : Set color scheme for visualizations

- [`fHMM_events()`](https://loelschlaeger.de/fHMM/reference/fHMM_events.md)
  [`print(`*`<fHMM_events>`*`)`](https://loelschlaeger.de/fHMM/reference/fHMM_events.md)
  : Checking events

## Data

The following data sets are included in the package.

- [`dax`](https://loelschlaeger.de/fHMM/reference/dax.md) : Deutscher
  Aktienindex (DAX) index data
- [`spx`](https://loelschlaeger.de/fHMM/reference/spx.md) : Standard &
  Poor’s 500 (S&P 500) index data
- [`unemp`](https://loelschlaeger.de/fHMM/reference/unemp.md) :
  Unemployment rate data USA
- [`vw`](https://loelschlaeger.de/fHMM/reference/vw.md) : Volkswagen AG
  (VW) stock data

## Models

The following pre-computed models are included in the package.

- [`dax_model_2n`](https://loelschlaeger.de/fHMM/reference/dax_model_2n.md)
  : DAX 2-state HMM with normal distributions
- [`dax_model_3t`](https://loelschlaeger.de/fHMM/reference/dax_model_3t.md)
  : DAX 3-state HMM with t-distributions
- [`dax_vw_model`](https://loelschlaeger.de/fHMM/reference/dax_vw_model.md)
  : DAX/VW hierarchical HMM with t-distributions
- [`sim_model_2gamma`](https://loelschlaeger.de/fHMM/reference/sim_model_2gamma.md)
  : Simulated 2-state HMM with gamma distributions
- [`unemp_spx_model_3_2`](https://loelschlaeger.de/fHMM/reference/unemp_spx_model_3_2.md)
  : Unemployment rate and S&P 500 hierarchical HMM
