# Simulate state-dependent observations

This function simulates state-dependent observations.

## Usage

``` r
simulate_observations(
  markov_chain,
  sdd,
  mu,
  sigma = NULL,
  df = NULL,
  seed = NULL,
  total_length = length(markov_chain)
)
```

## Arguments

- markov_chain:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  A `numeric` vector of states of a Markov chain.

- sdd:

  \[`character(1)`\]  
  A `character`, the name of the state-dependent distribution.

- mu:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A `numeric` vector of expected values.

- sigma:

  \[`NULL` \| [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A `numeric` vector of standard deviations (if any).

- df:

  \[`NULL` \| [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A `numeric` vector of degrees of freedom (if any).

- seed:

  \[`NULL` \| `integer(1)`\]  
  Sets a seed for the observation sampling.

- total_length:

  \[`integer(1)`\]  
  An `integer`, the total length of the output vector. Must be greater
  or equal than `length(markov_chain)`.

## Value

A `numeric` vector of length `total_length`, where the first
`length(markov_chain)` elements are numeric values and the last
`total_length - length(markov_chain)` elements are `NA_real_`.
