# Prepare data

This function simulates or reads financial data for the {fHMM} package.

## Usage

``` r
prepare_data(controls, true_parameters = NULL, seed = NULL)
```

## Arguments

- controls:

  \[`fHMM_controls`\]  
  An object of class `fHMM_controls`.

- true_parameters:

  \[`NULL` \| `fHMM_parameters`\]  
  An object of class `fHMM_parameters`, used as simulation parameters.
  By default, `true_parameters = NULL`, i.e., sampled true parameters.

- seed:

  \[`NULL` \| `integer(1)`\]  
  Set a seed for the data simulation. No seed by default.

## Value

An object of class
[`fHMM_data`](https://loelschlaeger.de/fHMM/reference/fHMM_data.md).

## Examples

``` r
controls <- set_controls()
data <- prepare_data(controls)
class(data)
#> [1] "fHMM_data"
summary(data)
#> Summary of fHMM simulated data
#> * number of observations: 100 
```
