# Volkswagen AG (VW) stock data

Volkswagen AG (VW) stock data from 1998 to 2022 from Yahoo Finance.

## Usage

``` r
vw
```

## Format

A `data.frame` with 6260 rows and the following 7 columns:

- `Date`: The date.

- `Open`: Opening price.

- `High`: Highest price.

- `Low`: Lowest price.

- `Close`: Close price adjusted for splits.

- `Adj.Close`: Close price adjusted for dividends and splits.

- `Volume`: Trade volume.

## Details

The data was obtained via:


    vw <- download_data(
      symbol = "VOW3.DE",  # Volkswagen AG identifier on Yahoo Finance
      from = "1988-07-22", # first observation
      to = "2022-12-31"    # last observation
    )
