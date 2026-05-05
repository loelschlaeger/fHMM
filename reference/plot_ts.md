# Visualize time series

This function visualizes the data time series.

## Usage

``` r
plot_ts(
  data,
  decoding,
  colors,
  events = NULL,
  title = NULL,
  from = NULL,
  to = NULL
)
```

## Arguments

- data:

  \[`fHMM_data`\]  
  An object of class `fHMM_data`.

- decoding:

  \[`NULL` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  [`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  Either `NULL` or an object of class `fHMM_decoding`.

- colors:

  \[`NULL` \| [`character()`](https://rdrr.io/r/base/character.html)\]  
  Either `NULL` (default) or a `character` vector of color names or
  hexadecimal RGB triplets.

- events:

  \[`NULL` \| `fHMM_events`\]  
  An object of class
  [`fHMM_events`](https://loelschlaeger.de/fHMM/reference/fHMM_events.md).

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

## Value

No return value. Draws a plot to the current device.
