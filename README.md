
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fHMM <img src="man/figures/sticker.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/loelschlaeger/fHMM/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/fHMM/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version-last-release/fHMM)](https://www.r-pkg.org/badges/version-last-release/fHMM)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)](https://cranlogs.r-pkg.org/badges/grand-total/fHMM)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/fHMM/branch/main/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/fHMM?branch=main)
<!-- badges: end -->

The goal of fHMM is to detect bearish and bullish markets in financial
time series aplying (hierarchical) hidden Markov models.

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
#> Thanks for using fHMM version 0.3.0.9000, have fun!
#> See https://loelschlaeger.github.io/fHMM for help.
#> Type 'citation("fHMM")' for citing this R package.
library(magrittr)
```

We fit a 3-state HMM with state-dependent t-distributions to the DAX
log-returns from 2010 to 2020.

The package has a build-in function to download the data from [Yahoo
Finance](https://finance.yahoo.com/):

``` r
download_data(symbol = "^GDAXI", file = "dax.csv", verbose = FALSE)
```

We first need to define the model by setting the `controls`:

``` r
controls = list(
  states = 3,
  sdds   = "t",
  data   = list(file        = "dax.csv",
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
#> * number of states: 3 
#> * sdds: t() 
#> * number of runs: 100
```

The function `prepare_data` prepares the data for estimation:

``` r
data = prepare_data(controls)
summary(data)
#> Summary of fHMM empirical data
#> * number of observations: 2791 
#> * data source: dax.csv 
#> * date column: Date 
#> * log returns: TRUE
```

We now fit the model and subsequentially decode the hidden states:

``` r
model = fit_model(data, ncluster = 7) %>% decode_states 
#> Checking start values
#> Maximizing likelihood
#> Computing Hessian
#> Fitting completed
#> Decoded states
summary(model)
#> Summary of fHMM model
#> 
#>   simulated hierarchy       LL       AIC       BIC
#> 1     FALSE     FALSE 8632.274 -17234.55 -17145.54
#> 
#> State-dependent distributions:
#> t() 
#> 
#> Estimates:
#>                   lb   estimate         ub
#> Gamma_2.1         NA  5.017e-02         NA
#> Gamma_3.1  2.779e-02  2.846e-02  2.913e-02
#> Gamma_1.2  3.930e-02  4.000e-02  4.071e-02
#> Gamma_3.2  5.069e-53  5.066e-53  5.062e-53
#> Gamma_1.3  6.818e-03  6.990e-03  7.166e-03
#> Gamma_2.3         NA  1.565e-18         NA
#> mu_1              NA -1.194e-05         NA
#> mu_2       1.211e-03  1.224e-03  1.237e-03
#> mu_3      -1.490e-03 -1.405e-03 -1.320e-03
#> sigma_1    1.215e-02  1.217e-02  1.220e-02
#> sigma_2    5.082e-03  5.102e-03  5.123e-03
#> sigma_3    2.129e-02  2.139e-02  2.148e-02
#> df_1       1.549e+17  1.549e+17  1.549e+17
#> df_2       4.387e+00  4.436e+00  4.486e+00
#> df_3       7.457e+00  7.628e+00  7.802e+00
#> 
#> States:
#> decoded
#>    1    2    3 
#> 1448 1028  315
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
