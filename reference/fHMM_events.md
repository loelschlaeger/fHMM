# Checking events

This function checks the input `events`.

## Usage

``` r
fHMM_events(events)

# S3 method for class 'fHMM_events'
print(x, ...)
```

## Arguments

- events:

  \[[`list()`](https://rdrr.io/r/base/list.html)\]  
  A `list` of two elements.

  - The first element is named `"dates"` and contains a `character`
    vector in format `"YYYY-MM-DD"`.

  - The second element is named `"labels"` and is a `character` vector
    of the same length as `"dates"`.

- x:

  \[`fHMM_events`\]  
  An object of class `fHMM_events`.

- ...:

  Currently not used.

## Value

An object of class `fHMM_events`.

## Examples

``` r
events <- list(
  dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
  labels = c(
    "9/11 terrorist attack", "Bankruptcy Lehman Brothers",
    "First COVID-19 case Germany"
  )
)
events <- fHMM_events(events)
```
