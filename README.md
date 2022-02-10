
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fHMM <img src="man/figures/sticker.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/loelschlaeger/fHMM/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/fHMM/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/fHMM)](https://www.r-pkg.org/badges/version-last-release/fHMM)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)
[![codecov](https://codecov.io/gh/loelschlaeger/fHMM/branch/master/graph/badge.svg?token=OYU22T74DV)](https://codecov.io/gh/loelschlaeger/fHMM)
<!-- badges: end -->

With fHMM you can detect bearish and bullish markets in financial time
series by applying *Hidden Markov Models (HMMs)*. The model and the
package functionality [is documented in detail
here](https://loelschlaeger.de/fHMM/articles/). Below, you can find a
first application to the German stock index
[DAX](https://en.wikipedia.org/wiki/DAX).

## Installation

You can install the released version of fHMM from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fHMM")
```

## Example: Fitting an HMM to the DAX

``` r
library(fHMM)
#> Thanks for using fHMM version 1.0.0!
#> See https://loelschlaeger.de/fHMM for help.
#> Type 'citation("fHMM")' for citing this R package.
```

We fit a 2-state HMM with state-dependent t-distributions to the DAX
log-returns from 2010 to 2020. The states can be interpreted as proxies
for bearish and bullish markets.

The package has a build-in function to download the data from [Yahoo
Finance](https://finance.yahoo.com/):

``` r
path <- paste0(tempdir(),"/dax.csv")
download_data(symbol = "^GDAXI", file = path, verbose = FALSE)
```

We first need to define the model by setting some `controls`:

``` r
controls <- list(
  states = 2,
  sdds   = "t",
  data   = list(file        = path,
                date_column = "Date",
                data_column = "Close",
                logreturns  = TRUE,
                from        = "2010-01-01",
                to          = "2020-12-31")
)
controls <- set_controls(controls)
```

The function `prepare_data()` prepares the data for estimation:

``` r
data <- prepare_data(controls)
summary(data)
#> Summary of fHMM empirical data
#> * number of observations: 2791 
#> * data source: dax.csv 
#> * date column: Date 
#> * log returns: TRUE
```

We now fit the model and subsequently decode the hidden states:

``` r
set.seed(1)
model <- fit_model(data, ncluster = 7)
#> Checking start values
#> Maximizing likelihood
#> Computing Hessian
#> Fitting completed
model <- decode_states(model)
#> Decoded states
summary(model)
#> Summary of fHMM model
#> 
#>   simulated hierarchy       LL       AIC       BIC
#> 1     FALSE     FALSE 8577.094 -17138.19 -17090.71
#> 
#> State-dependent distributions:
#> t() 
#> 
#> Estimates:
#>                   lb   estimate        ub
#> Gamma_2.1  0.0177990  0.0319495 5.670e-02
#> Gamma_1.2  0.0054805  0.0109757 2.186e-02
#> mu_1       0.0005605  0.0009467 1.333e-03
#> mu_2      -0.0024864 -0.0009720 5.425e-04
#> sigma_1    0.0067460  0.0072860 7.869e-03
#> sigma_2    0.0148162  0.0168710 1.921e-02
#> df_1       4.0858880  5.3999003 7.136e+00
#> df_2       4.1958554  6.6527687 1.055e+01
#> 
#> States:
#> decoded
#>    1    2 
#> 2208  583
```

Having estimated the model, we can visualize the state-dependent
distributions and the decoded time series to interpret bearish and
bullish markets:

``` r
plot(model, plot_type = c("sdds","ts"))
```

<img src="man/figures/README-plots-1.png" width="100%" /><img src="man/figures/README-plots-2.png" width="100%" />
