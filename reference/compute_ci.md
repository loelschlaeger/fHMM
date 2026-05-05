# Compute confidence intervals

This helper function computes confidence intervals for the estimates of
an [`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
object using the inverse Fisher information.

## Usage

``` r
compute_ci(x, alpha = 0.05)
```

## Arguments

- x:

  \[`fHMM_model`\]  
  An object of class
  [`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md).

- alpha:

  \[`numeric(1)`\]  
  The alpha level for the confidence interval. Must be between 0 and 1.
  By default, `alpha = 0.05`, which computes a 95 percent confidence
  interval.

## Value

A `list` containing the following elements:

- `lb`: lower bound of confidence

- `estimate`: point estimate

- `ub`: upper bound of confidence
