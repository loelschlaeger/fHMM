# Decode the underlying hidden state sequence

This function decodes the (most likely) underlying hidden state sequence
by applying the Viterbi algorithm for global decoding.

## Usage

``` r
decode_states(x, verbose = TRUE)

viterbi(observations, nstates, sdd, Gamma, mu, sigma = NULL, df = NULL)
```

## Arguments

- x:

  \[`fHMM_model`\]  
  An object of class
  [`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md).

- verbose:

  \[`logical(1)`\]  
  Set to `TRUE` to print progress messages.

- observations:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A `numeric` `vector` of state-dependent observations.

- nstates:

  \[`integer(1)`\]  
  The number of states.

- sdd:

  \[`character(1)`\]  
  A `character`, specifying the state-dependent distribution. One of

  - `"normal"` (the normal distribution),

  - `"lognormal"` (the log-normal distribution),

  - `"t"` (the t-distribution),

  - `"gamma"` (the gamma distribution),

  - `"poisson"` (the Poisson distribution).

- Gamma:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  A transition probability `matrix` of dimension `nstates`.

- mu:

  \[`numeric(nstates)`\]  
  A `numeric` vector of expected values for the state-dependent
  distribution in the different states of length `nstates`.

  For the gamma- or Poisson-distribution, `mu` must be positive.

- sigma:

  \[`NULL` \| `numeric(nstates)`\]  
  A positive `numeric` vector of standard deviations for the
  state-dependent distribution in the different states of length
  `nstates`.

  Not relevant in case of a state-dependent Poisson distribution.

- df:

  \[`NULL` \| `numeric(nstates)`\]  
  A positive `numeric` vector of degrees of freedom for the
  state-dependent distribution in the different states of length
  `nstates`.

  Only relevant in case of a state-dependent t-distribution.

## Value

An object of class
[`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md)
with decoded state sequence included.

## References

<https://en.wikipedia.org/wiki/Viterbi_algorithm>

## Examples

``` r
decode_states(dax_model_3t)
#> Decoded states
#> fHMM fitted model:
#> * total estimation time: 3 mins 
#> * accepted runs: 100 of 100 
#> * log-likelihood: 17650.02 
plot(dax_model_3t, type = "ts")

viterbi(
  observations = c(1, 1, 1, 10, 10, 10),
  nstates      = 2,
  sdd          = "poisson",
  Gamma        = matrix(0.5, 2, 2),
  mu           = c(1, 10)
)
#> [1] 1 1 1 2 2 2
```
