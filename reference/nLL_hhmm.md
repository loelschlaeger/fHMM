# Negative log-likelihood function of an HHMM

This function computes the negative log-likelihood of an HHMM.

## Usage

``` r
nLL_hhmm(parUncon, observations, controls)
```

## Arguments

- parUncon:

  \[`parUncon`\]  
  An object of class `parUncon`.

- observations:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  The matrix of the simulated or empirical data used for estimation.

- controls:

  \[`fHMM_controls`\]  
  An object of class `fHMM_controls`.

## Value

The negative log-likelihood value.
