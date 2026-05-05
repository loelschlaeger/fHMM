# Deutscher Aktienindex (DAX) index data

Deutscher Aktienindex (DAX) index data from 1988 to 2022 from Yahoo
Finance.

## Usage

``` r
dax
```

## Format

A `data.frame` with 9012 rows and the following 7 columns:

- `Date`: The date.

- `Open`: Opening price.

- `High`: Highest price.

- `Low`: Lowest price.

- `Close`: Close price adjusted for splits.

- `Adj.Close`: Close price adjusted for dividends and splits.

- `Volume`: Trade volume.

## Details

The data was obtained via:


    dax <- download_data(
      symbol = "^GDAXI",   # DAX identifier on Yahoo Finance
      from = "1988-01-01", # first observation
      to = "2022-12-31"    # last observation
    )
