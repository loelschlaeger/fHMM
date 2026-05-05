# DAX 2-state HMM with normal distributions

A pre-computed HMM on closing prices of the DAX from 2000 to 2022 with
two hidden states and normal state-dependent distributions for
demonstration purposes.

## Usage

``` r
data("dax_model_2n")
```

## Format

An object of class
[`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md).

## Details

The model was estimated via:


    controls <- set_controls(
      states = 2,
      sdds   = "normal",
      data   = list(
        file        = dax,
        date_column = "Date",
        data_column = "Close",
        logreturns  = TRUE,
        from        = "2000-01-03",
        to          = "2022-12-31"
      ),
      fit    = list("runs" = 10, "gradtol" = 1e-6, "steptol" = 1e-6)
    )
    dax_data <- prepare_data(controls)
    dax_model_2n <- fit_model(dax_data, seed = 1)
    dax_model_2n <- decode_states(dax_model_2n)
    dax_model_2n <- compute_residuals(dax_model_2n)
    summary(dax_model_2n)
