# DAX 3-state HMM with t-distributions

A pre-computed HMM on closing prices of the DAX from 2000 to 2022 with
three hidden states and state-dependent t-distributions for
demonstration purposes.

## Usage

``` r
data("dax_model_3t")
```

## Format

An object of class
[`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md).

## Details

The model was estimated via:


    controls <- set_controls(
      states = 3,
      sdds   = "t",
      data   = list(
        file        = dax,
        date_column = "Date",
        data_column = "Close",
        logreturns  = TRUE,
        from        = "2000-01-03",
        to          = "2022-12-31"
      ),
      fit    = list(
        runs        = 100,
        iterlim     = 300,
        gradtol     = 1e-6,
        steptol     = 1e-6
      )
    )
    dax_data <- prepare_data(controls)
    dax_model_3t <- fit_model(dax_data, seed = 1, ncluster = 10)
    dax_model_3t <- decode_states(dax_model_3t)
    dax_model_3t <- compute_residuals(dax_model_3t)
    summary(dax_model_3t)
