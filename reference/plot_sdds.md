# Visualization of estimated state-dependent distributions

This function plots the estimated state-dependent distributions.

## Usage

``` r
plot_sdds(est, true = NULL, controls, colors)
```

## Arguments

- est:

  \[`fHMM_parameters`\]  
  An object of class `fHMM_parameters` with estimated parameters.

- true:

  \[`NULL` \| `fHMM_parameters`\]  
  Either `NULL` or an object of class `fHMM_parameters` with true
  parameters.

- controls:

  \[`fHMM_controls`\]  
  An object of class `fHMM_controls`.

- colors:

  \[`NULL` \| [`character()`](https://rdrr.io/r/base/character.html)\]  
  Either `NULL` (default) or a `character` vector of color names or
  hexadecimal RGB triplets.

## Value

No return value. Draws a plot to the current device.
