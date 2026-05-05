# Compare multiple models

This function performs model comparison by comparing multiple
[`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
objects with respect to

- the number of model parameters,

- the log-likelihood value,

- the AIC value,

- the BIC value.

## Usage

``` r
compare_models(...)
```

## Arguments

- ...:

  A list of one or more objects of class
  [`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md).

## Value

A `data.frame` with models in rows and comparison criteria in columns.

## Examples

``` r
### 3-state HMM with t-distributions is preferred over 2-state HMM with
### normal distributions for the DAX data based on AIC and BIC
compare_models(dax_model_2n, dax_model_3t)
#>              parameters loglikelihood       AIC       BIC
#> dax_model_2n          6      17403.61 -34795.21 -34755.13
#> dax_model_3t         15      17650.02 -35270.05 -35169.85
```
