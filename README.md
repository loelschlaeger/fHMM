
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

The goal of fHMM is to detect bearish and bullish markets in financial
time series applying (hierarchical) hidden Markov models.

## Installation

You can install the released version of fHMM from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fHMM")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("loelschlaeger/fHMM")
```

## Example: Fitting an HMM to the DAX

``` r
library(fHMM)
#> Thanks for using fHMM version 1.0.0!
#> See https://loelschlaeger.de/fHMM for help.
#> Type 'citation("fHMM")' for citing this R package.
library(magrittr)
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

We first need to define the model by setting the `controls`:

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
(controls %<>% set_controls)
#> fHMM controls:
#> * hierarchy: FALSE 
#> * data type: empirical 
#> * number of states: 2 
#> * sdds: t() 
#> * number of runs: 100
```

The function `prepare_data` prepares the data for estimation:

``` r
data <- prepare_data(controls)
summary(data)
#> Summary of fHMM empirical data
#> * number of observations: 2791 
#> * data source: dax.csv 
#> * date column: Date 
#> * log returns: TRUE
```

We now fit the model and subsequentially decode the hidden states:

``` r
set.seed(1)
model <- fit_model(data, ncluster = 7) %>% decode_states 
#> Checking start values
#> Maximizing likelihood
#> Computing Hessian
#> Fitting completed
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
#>                   lb   estimate         ub
#> Gamma_2.1  0.0313615  0.0319495  0.0325481
#> Gamma_1.2  0.0107352  0.0109757  0.0112215
#> mu_1       0.0009344  0.0009467  0.0009591
#> mu_2      -0.0010204 -0.0009720 -0.0009235
#> sigma_1    0.0072680  0.0072860  0.0073039
#> sigma_2    0.0168010  0.0168710  0.0169412
#> df_1       5.3519409  5.3999003  5.4482894
#> df_2       6.5553795  6.6527687  6.7516048
#> 
#> States:
#> decoded
#>    1    2 
#> 2208  583
```

Let’s visualize the estimated state-dependent distributions and the
decoded time series:

``` r
model %>% plot("sdds")
```

<img src="man/figures/README-plots-1.png" width="100%" />

``` r
model %>% plot("ts")
```

<img src="man/figures/README-plots-2.png" width="100%" />
