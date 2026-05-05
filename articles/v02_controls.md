# Controls

The [fHMM](https://loelschlaeger.de/fHMM/) package allows for multiple
hidden Markov model specifications, including different data
transformations, state-dependent distributions, and a hierarchical model
structure. This vignette[^1] outlines what and how specifications are
possible.

We first load the package via the familiar
[`library()`](https://rdrr.io/r/base/library.html) call:

``` r

library("fHMM")
```

## The `set_controls` function

The [fHMM](https://loelschlaeger.de/fHMM/) philosophy is to start the
modeling process by setting all data, model, and estimation
specifications. This is done by defining a named `list` of controls and
passing it to the
[`set_controls()`](https://loelschlaeger.de/fHMM/reference/set_controls.md)
function. The function checks the specifications and returns an
`fHMM_controls` object which stores all specifications and thereby
provides required information for other
[fHMM](https://loelschlaeger.de/fHMM/) functionalities.

## Example specifications

For demonstration, we list example specifications using data from the
Deutscher Aktienindex DAX[^2] ([Janßen and Rudolph 1992](#ref-jan92)):

``` r

dax <- download_data(symbol = "^GDAXI")
head(dax)
#>         Date    Open    High     Low   Close Adj.Close Volume
#> 1 1987-12-30 1005.19 1005.19 1005.19 1005.19   1005.19      0
#> 2 1987-12-31      NA      NA      NA      NA        NA     NA
#> 3 1988-01-01      NA      NA      NA      NA        NA     NA
#> 4 1988-01-04  956.49  956.49  956.49  956.49    956.49      0
#> 5 1988-01-05  996.10  996.10  996.10  996.10    996.10      0
#> 6 1988-01-06 1006.01 1006.01 1006.01 1006.01   1006.01      0
```

### HMMs for empirical data

The following lines of code specify a 3-state HMM with state-dependent
t-distributions on the data in the file dax.csv. The dates are provided
in the column called Date and the data in the column called Close. The
`logreturns = TRUE` line transforms the index data to log-returns. The
`runs = 50` line sets the number of numerical optimization runs to 50.

``` r

controls <- list(
  states = 3,
  sdds   = "t",
  data   = list(file        = dax,
                date_column = "Date",
                data_column = "Close",
                logreturns  = TRUE),
  fit    = list(runs        = 50)
)
set_controls(controls)
#> fHMM controls:
#> * hierarchy: FALSE 
#> * data type: empirical 
#> * number of states: 3 
#> * sdds: t() 
#> * number of runs: 50
```

### Simulated HMM data

The following specifies a 2-state HMM with state-dependent Gamma
distributions, where the expectation values for state 1 and 2 are fixed
to 0.5 and 2, respectively. The model will be fitted to 500 data points
(`horizon = 500`), that are going to be simulated from this model
specification.

``` r

controls <- list(
  states  = 2,
  sdds    = "gamma(mu = 0.5|2)",
  horizon = 500
)
set_controls(controls)
#> fHMM controls:
#> * hierarchy: FALSE 
#> * data type: simulated 
#> * number of states: 2 
#> * sdds: gamma(mu = 0.5|2) 
#> * number of runs: 10
```

### Hierarchical HMMs

Specifying hierarchical HMMs is analogously, except that new parameters
can be specified (for example `period`, see below) and some parameters
now can be specified for both hierarchies.

``` r

controls <- list(
  hierarchy = TRUE,
  horizon   = c(100, 10),
  sdds      = c("t(df = 1)", "t(df = Inf)"),
  period    = "m"
)
set_controls(controls)
#> fHMM controls:
#> * hierarchy: TRUE 
#> * data type: simulated 
#> * number of states: 2 2 
#> * sdds: t(df = 1) t(df = Inf) 
#> * number of runs: 10
```

The help page of the
[`set_controls()`](https://loelschlaeger.de/fHMM/reference/set_controls.md)
function provides an overview of all possible specifications, it can be
accessed via
[`help("set_controls", package = "fHMM")`](https://loelschlaeger.de/fHMM/reference/set_controls.md).

## References

Janßen, B., and B. Rudolph. 1992. “Der Deutsche Aktienindex DAX.” *Knapp
Verlag*.

[^1]: This vignette was built using R 4.6.0 with the
    [fHMM](https://loelschlaeger.de/fHMM/) 1.4.3 package.

[^2]: The
    [`download_data()`](https://loelschlaeger.de/fHMM/reference/download_data.md)
    function is explained in [the vignette on data
    management](https://loelschlaeger.de/fHMM/articles/v03_data_management.html).
