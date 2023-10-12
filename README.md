
<!-- README.md is generated from README.Rmd. Please edit that file -->

# HMMs for Finance <a href="https://loelschlaeger.de/fHMM/"><img src="man/figures/logo.png" align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/fHMM)](https://CRAN.R-project.org/package=fHMM)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/last-month/fHMM)](https://cran.r-project.org/package=fHMM)
[![R-CMD-check](https://github.com/loelschlaeger/fHMM/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/loelschlaeger/fHMM/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The `{fHMM}` R package allows for the detection and characterization of
financial market regimes in time series data by applying hidden Markov
Models (HMMs). The [detailed
documentation](https://loelschlaeger.de/fHMM/articles/) outlines the
functionality and the model formulation. Below, we provide an initial
application to the German stock index
[DAX](https://en.wikipedia.org/wiki/DAX).

## Installation

You can install the released version of `{fHMM}` from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("fHMM")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("loelschlaeger/fHMM")
```

## Contributing

We are open to contributions and would appreciate your input! If you
encounter any issues, please [submit bug reports as
issues](https://github.com/loelschlaeger/fHMM/issues/new?assignees=&labels=bug&template=bug.md).
If you have any ideas for new features, please submit them as [feature
requests](https://github.com/loelschlaeger/fHMM/issues/new?assignees=&labels=future&template=suggestion.md).
If you would like to add extensions to the package, please fork the
“master” branch and submit merge requests. We look forward to your
contributions!

## Example: Fitting an HMM to the DAX

We fit a 3-state HMM with state-dependent t-distributions to the DAX
log-returns from 2000 to 2022. The states can be interpreted as proxies
for bearish (green below) and bullish markets (red) and an “in-between”
market state (yellow).

``` r
library("fHMM")
```

The package has a build-in function to download financial data from
[Yahoo Finance](https://finance.yahoo.com/):

``` r
dax <- download_data(symbol = "^GDAXI", file = NULL, verbose = FALSE)
```

We first need to define the model:

``` r
controls <- list(
  states = 3,
  sdds   = "t",
  data   = list(file        = dax,
                date_column = "Date",
                data_column = "Close",
                logreturns  = TRUE,
                from        = "2000-01-01",
                to          = "2022-12-31")
)
controls <- set_controls(controls)
```

The function `prepare_data()` then prepares the data for estimation:

``` r
data <- prepare_data(controls)
```

The `summary()` method gives an overview:

``` r
summary(data)
#> Summary of fHMM empirical data
#> * number of observations: 5882 
#> * data source: data.frame 
#> * date column: Date 
#> * log returns: TRUE
```

We fit the model and subsequently decode the hidden states and compute
(pseudo-) residuals:

``` r
model <- fit_model(data)
model <- decode_states(model)
model <- compute_residuals(model)
```

The `summary()` method gives an overview of the model fit:

``` r
summary(model)
#> Summary of fHMM model
#> 
#>   simulated hierarchy       LL       AIC       BIC
#> 1     FALSE     FALSE 17649.52 -35269.03 -35168.84
#> 
#> State-dependent distributions:
#> t() 
#> 
#> Estimates:
#>                   lb   estimate        ub
#> Gamma_2.1  1.286e-02  2.007e-02 3.113e-02
#> Gamma_3.1  1.208e-06  1.198e-06 1.180e-06
#> Gamma_1.2  1.557e-02  2.489e-02 3.959e-02
#> Gamma_3.2  1.036e-02  1.877e-02 3.378e-02
#> Gamma_1.3  4.119e-07  4.080e-07 4.019e-07
#> Gamma_2.3  2.935e-03  5.275e-03 9.422e-03
#> mu_1       9.655e-04  1.271e-03 1.576e-03
#> mu_2      -8.483e-04 -3.102e-04 2.278e-04
#> mu_3      -3.813e-03 -1.760e-03 2.932e-04
#> sigma_1    5.417e-03  5.853e-03 6.324e-03
#> sigma_2    1.278e-02  1.330e-02 1.384e-02
#> sigma_3    2.348e-02  2.579e-02 2.832e-02
#> df_1       3.957e+00  5.198e+00 6.828e+00
#> df_2       3.870e+08  3.870e+08 3.870e+08
#> df_3       5.549e+00  1.078e+01 2.095e+01
#> 
#> States:
#> decoded
#>    1    2    3 
#> 2278 2900  704 
#> 
#> Residuals:
#>      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#> -3.519694 -0.658831  0.009613 -0.002206  0.669598  3.905726
```

Having estimated the model, we can visualize the state-dependent
distributions and the decoded time series:

``` r
events <- fHMM_events(
  list(dates = c("2001-09-11", "2008-09-15", "2020-01-27"),
       labels = c("9/11 terrorist attack", "Bankruptcy Lehman Brothers", "First COVID-19 case Germany"))
)
plot(model, plot_type = c("sdds","ts"), events = events)
```

![](man/figures/README-plot_model_fit-1.png)<!-- -->![](man/figures/README-plot_model_fit-2.png)<!-- -->

The (pseudo-) residuals help to evaluate the model fit:

``` r
plot(model, plot_type = "pr")
```

![](man/figures/README-plot_model_residuals-1.png)<!-- -->
