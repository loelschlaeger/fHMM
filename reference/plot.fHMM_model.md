# Plot method for an object of class [`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)

This function is the plot method for an object of class
[`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md).

## Usage

``` r
# S3 method for class 'fHMM_model'
plot(
  x,
  plot_type = "ts",
  events = NULL,
  colors = NULL,
  ll_relative = TRUE,
  title = NULL,
  from = NULL,
  to = NULL,
  ...
)
```

## Arguments

- x:

  \[`fHMM_model`\]  
  An object of class
  [`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md).

- plot_type:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  A character (vector), specifying the type of plot and can be one (or
  more) of

  - `"ll"` for a visualization of the likelihood values in the different
    optimization runs,

  - `"sdds"` for a visualization of the estimated state-dependent
    distributions,

  - `"pr"` for a visualization of the model's (pseudo-) residuals,

  - `"ts"` for a visualization of the financial time series.

- events:

  \[`NULL` \| `fHMM_events`\]  
  An object of class
  [`fHMM_events`](https://loelschlaeger.de/fHMM/reference/fHMM_events.md).

- colors:

  \[`NULL` \| [`character()`](https://rdrr.io/r/base/character.html)\]  
  Either `NULL` (default) or a `character` vector of color names or
  hexadecimal RGB triplets.

- ll_relative:

  \[`logical(1)`\]  
  A `logical`, set to `TRUE` (default) to plot the differences from the
  best log-likelihood value. Set to `FALSE` to plot the absolute values.

- title:

  \[`NULL` \| `character(1)`\]  
  Optionally a `character` for a custom title.

- from:

  \[`NULL` \| `character(1)`\]  
  Optionally a `character`, a date in format `"YYYY-MM-DD"`, setting the
  lower date bound for plotting. By default, `from = NULL`, i.e. no
  lower bound.

- to:

  \[`NULL` \| `character(1)`\]  
  Optionally a `character`, a date in format `"YYYY-MM-DD"`, setting the
  upper date bound for plotting. By default, `to = NULL`, i.e. no upper
  bound.

- ...:

  Currently not used.

## Value

No return value. Draws a plot to the current device.
