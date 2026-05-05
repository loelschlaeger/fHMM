# Download financial data from Yahoo Finance

This function downloads financial data from <https://finance.yahoo.com/>
and returns it as a `data.frame`.

## Usage

``` r
download_data(
  symbol,
  from = "1902-01-01",
  to = Sys.Date(),
  fill_dates = FALSE,
  columns = c("Date", "Open", "High", "Low", "Close", "Adj.Close", "Volume")
)
```

## Arguments

- symbol:

  \[`character(1)`\]  
  A `character`, the stock's symbol.

  It must match the identifier on <https://finance.yahoo.com/>.

- from:

  \[`character(1)`\]  
  A `character` in the format `"YYYY-MM-DD"`, setting the lower data
  bound.

  Must not be earlier than `"1902-01-01"` (default).

- to:

  \[`character(1)`\]  
  A `character` in the format `"YYYY-MM-DD"`, setting the upper data
  bound.

  Default is the current date `Sys.date()`.

- fill_dates:

  \[`logical(1)`\]  
  Set to `TRUE` to fill missing dates (e.g., days at which the stock
  market is closed) with `NA`'s.

  By default, `fill_dates = FALSE`.

- columns:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  A `character` of requested data columns, see the details.

  By default, all columns are returned.

## Value

A `data.frame`.

## Details

Yahoo Finance provides historical daily data for stocks or indices. The
following data columns are available:

- `Date`: The date.

- `Open`: Opening price.

- `High`: Highest price.

- `Low`: Lowest price.

- `Close`: Close price adjusted for splits.

- `Adj.Close`: Close price adjusted for dividends and splits.

- `Volume`: Trade volume.

## Examples

``` r
### 21st century DAX closing prices
data <- download_data(
  symbol = "^GDAXI", from = "2000-01-01", columns = c("Date", "Close"),
  fill_dates = TRUE
)
head(data)
#>         Date   Close
#> 1 2000-01-01      NA
#> 2 2000-01-02      NA
#> 3 2000-01-03 6750.76
#> 4 2000-01-04 6586.95
#> 5 2000-01-05 6502.07
#> 6 2000-01-06 6474.92
```
