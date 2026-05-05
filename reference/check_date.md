# Check date format

This function checks if the input `date` has the format `"YYYY-MM-DD"`.

## Usage

``` r
check_date(date)
```

## Arguments

- date:

  \[`character(1)`\]  
  The date in format `"YYYY-MM-DD"`.

## Value

`as.Date(date)` if `date` has the format `"YYYY-MM-DD"`. Otherwise, the
function throws an error.
