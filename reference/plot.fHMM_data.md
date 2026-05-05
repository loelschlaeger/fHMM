# Plot method for an object of class `fHMM_data`

This function is the plot method for an object of class `fHMM_data`.

## Usage

``` r
# S3 method for class 'fHMM_data'
plot(x, events = NULL, title = NULL, from = NULL, to = NULL, ...)
```

## Arguments

- x:

  \[`fHMM_data`\]  
  An object of class `fHMM_data`.

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

- ...:

  Currently not used.

## Value

No return value. Draws a plot to the current device.

## Examples

``` r
plot(dax_model_3t$data, title = "DAX time series")

```
