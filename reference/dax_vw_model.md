# DAX/VW hierarchical HMM with t-distributions

A pre-computed HHMM with monthly averaged closing prices of the DAX from
2010 to 2022 on the coarse scale, Volkswagen AG stock data on the fine
scale, two hidden fine-scale and coarse-scale states, respectively, and
state-dependent t-distributions for demonstration purposes.

## Usage

``` r
data("dax_vw_model")
```

## Format

An object of class
[`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md).

## Details

The model was estimated via:


    controls <- set_controls(
      hierarchy = TRUE,
      states    = c(2, 2),
      sdds      = c("t", "t"),
      period    = "m",
      data      = list(
        file       = list(dax, vw),
        from       = "2010-01-01",
        to         = "2022-12-31",
        logreturns = c(TRUE, TRUE)
      ),
      fit       = list(
        runs       = 200,
        iterlim    = 300,
        gradtol    = 1e-6,
        steptol    = 1e-6
      )
    )
    dax_vw_data <- prepare_data(controls)
    dax_vw_model <- fit_model(dax_vw_data, seed = 1, ncluster = 10)
    dax_vw_model <- decode_states(dax_vw_model)
    dax_vw_model <- compute_residuals(dax_vw_model)
    summary(dax_vw_model)
