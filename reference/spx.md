# Standard & Poor’s 500 (S&P 500) index data

Standard & Poor’s 500 (S&P 500) index data from 1928 to 2022 from Yahoo
Finance.

## Usage

``` r
spx
```

## Format

A `data.frame` with 23864 rows and the following 7 columns:

- `Date`: The date.

- `Open`: Opening price.

- `High`: Highest price.

- `Low`: Lowest price.

- `Close`: Close price adjusted for splits.

- `Adj.Close`: Close price adjusted for dividends and splits.

- `Volume`: Trade volume.

## Details

The data was obtained via:


    spx <- download_data(
      symbol = "^GSPC",    # S&P 500 identifier on Yahoo Finance
      from = "1928-01-01", # first observation
      to = "2022-12-31"    # last observation
    )
