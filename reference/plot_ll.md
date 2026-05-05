# Visualization of log-likelihood values

This function plots the log-likelihood values of the different
optimization runs.

## Usage

``` r
plot_ll(lls, ll_relative = TRUE)
```

## Arguments

- lls:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A `numeric` vector of log-likelihood values.

- ll_relative:

  \[`logical(1)`\]  
  A `logical`, set to `TRUE` (default) to plot the differences from the
  best log-likelihood value. Set to `FALSE` to plot the absolute values.

## Value

No return value. Draws a plot to the current device.
