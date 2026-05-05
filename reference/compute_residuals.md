# Compute (pseudo-) residuals

This function computes (pseudo-) residuals of an
[`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
object.

## Usage

``` r
compute_residuals(x, verbose = TRUE)
```

## Arguments

- x:

  \[`fHMM_model`\]  
  An object of class
  [`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md).

- verbose:

  \[`logical(1)`\]  
  Set to `TRUE` (default) to print progress messages.

## Value

An object of class
[`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
with residuals included.

## Examples

``` r
compute_residuals(dax_model_3t)
#> Computed residuals
#> fHMM fitted model:
#> * total estimation time: 3 mins 
#> * accepted runs: 100 of 100 
#> * log-likelihood: 17650.02 
summary(residuals(dax_model_3t))
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -3.517900 -0.664018  0.012170 -0.003262  0.673180  3.693568 
```
