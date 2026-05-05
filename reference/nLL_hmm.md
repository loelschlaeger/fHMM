# Negative log-likelihood function of an HMM

This function computes the negative log-likelihood of an HMM.

## Usage

``` r
nLL_hmm(parUncon, observations, controls)
```

## Arguments

- parUncon:

  \[`parUncon`\]  
  An object of class `parUncon`.

- observations:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The vector of the simulated or empirical data used for estimation.

- controls:

  \[`fHMM_controls`\]  
  An object of class `fHMM_controls`.

## Value

The negative log-likelihood value.
