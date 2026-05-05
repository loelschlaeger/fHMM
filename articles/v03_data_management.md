# Data management

This vignette[^1] explains how to prepare or simulate data in
[fHMM](https://loelschlaeger.de/fHMM/) for estimation.

## Empirical data

Empirical data can be provided either as a `data.frame` or as a
comma-separated values (.csv) file, see [the vignette on specifying the
controls](https://loelschlaeger.de/fHMM/articles/v02_controls.html) for
details.[^2] The [fHMM](https://loelschlaeger.de/fHMM/) package comes
with a dataset of the Deutscher Aktienindex for demonstration purposes
that can be accessed as follows:

``` r

system.file("extdata", "dax.csv", package = "fHMM")
```

The
[`prepare_data()`](https://loelschlaeger.de/fHMM/reference/prepare_data.md)
function prepares the data based on the `data` controls specifications
and returns an `fHMM_data` object that can be passed to the
[`fit_model()`](https://loelschlaeger.de/fHMM/reference/fit_model.md)
function for model fitting.

``` r

controls <- list(
  states = 3,
  sdds   = "t",
  data   = list(
    file        = system.file("extdata", "dax.csv", package = "fHMM"),
    date_column = "Date",
    data_column = "Close",
    logreturns  = TRUE
  )
)
controls <- set_controls(controls)
data <- prepare_data(controls)
summary(data)
#> Summary of fHMM empirical data
#> * number of observations: 9012 
#> * data source: dax.csv 
#> * date column: Date 
#> * log returns: TRUE
```

## Download stock data

Daily stock prices listed on <https://finance.yahoo.com/> can be
downloaded directly via

``` r

download_data(symbol, from, to, file)
```

where

- `symbol` is the stock’s symbol that has to match the official symbol
  on <https://finance.yahoo.com/>,[^3]

- `from` and `to` define the time interval (in format `"YYYY-MM-DD"`),

- `file` is the name of the file where the .csv-file is saved. If
  `file = NULL` (default), the data is not saved but returned as a
  `data.frame`.

For example, the following call downloads the 21st century daily data of
the DAX:

``` r

dax <- download_data(symbol = "^GDAXI", from = "2000-01-01", to = Sys.Date())
head(dax)
#>         Date    Open    High     Low   Close Adj.Close   Volume
#> 1 2000-01-03 6961.72 7159.33 6720.87 6750.76   6750.76 43072500
#> 2 2000-01-04 6747.24 6755.36 6510.46 6586.95   6586.95 46678400
#> 3 2000-01-05 6585.85 6585.85 6388.91 6502.07   6502.07 52682800
#> 4 2000-01-06 6501.45 6539.31 6402.63 6474.92   6474.92 41180600
#> 5 2000-01-07 6489.94 6791.53 6470.14 6780.96   6780.96 56058900
#> 6 2000-01-10 6785.47 6975.26 6785.47 6925.52   6925.52 42006200
```

## Highlighting events

Historical events can be highlighted by specifying a named list `events`
with elements `dates` (a vector of dates) and `labels` (a vector of
labels for the events) and passing it to the plot method, for example:

``` r

events <- fHMM:::fHMM_events(
  list(
    dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
    labels = c(
      "9/11 terrorist attack",
      "Bankruptcy of Lehman Brothers",
      "First COVID-19 case in Germany"
    )
  )
)
print(events)
#>        dates                         labels
#> 1 2001-09-11          9/11 terrorist attack
#> 2 2008-09-15  Bankruptcy of Lehman Brothers
#> 3 2020-01-27 First COVID-19 case in Germany
plot(data, events = events)
```

![](fHMM-events-1.png)

## Simulated data

If the `data` parameter in the model’s `controls` is unspecified, the
model is fitted to simulated data from the model specification. This can
be useful for testing the functionality or conducting simulation
experiments.

True model parameters can be specified by defining an
`fHMM_parameters`-object via the
[`fHMM_parameters()`](https://loelschlaeger.de/fHMM/reference/fHMM_parameters.md)
function and passing it to
[`prepare_data()`](https://loelschlaeger.de/fHMM/reference/prepare_data.md).

[^1]: This vignette was built using R 4.6.0 with the
    [fHMM](https://loelschlaeger.de/fHMM/) 1.4.3 package.

[^2]: The
    [`download_data()`](https://loelschlaeger.de/fHMM/reference/download_data.md)
    function explained below provides a convenient tool for downloading
    stock data from <https://finance.yahoo.com/>.

[^3]: For example, `"^GDAXI"` is the symbol of the DAX and `"^GSPC"` the
    one of the S&P 500.
