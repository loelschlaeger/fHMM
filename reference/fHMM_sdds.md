# Define state-dependent distributions

This helper function defines state-dependent distributions.

## Usage

``` r
fHMM_sdds(sdds, states)

# S3 method for class 'fHMM_sdds'
print(x, ...)
```

## Arguments

- sdds:

  \[`character(1)` \| `character(2)`\]  
  A `character`, specifying the state-dependent distribution. One of

  - `"normal"` (the normal distribution),

  - `"lognormal"` (the log-normal distribution),

  - `"t"` (the t-distribution),

  - `"gamma"` (the gamma distribution),

  - `"poisson"` (the Poisson distribution).

  The distribution parameters, i.e. the

  - mean `mu`,

  - standard deviation `sigma` (not for the Poisson distribution),

  - degrees of freedom `df` (only for the t-distribution),

  can be fixed via, e.g., `"t(df = 1)"` or `"gamma(mu = 0, sigma = 1)"`.
  To fix different values of a parameter for different states, separate
  by "\|", e.g. `"poisson(mu = 1|2|3)"`.

  If `hierarchy = TRUE`, `sdds` must be a `vector` of length 2. The
  first entry corresponds to the coarse-scale layer, while the second
  entry corresponds to the fine-scale layer.

  By default, `sdds = "normal"` if `hierarchy = FALSE` and
  `sdds = c("normal", "normal")` if `hierarchy = TRUE`.

- states:

  \[`integer(1)` \| `integer(2)`\]  
  An `integer`, the number of states of the underlying Markov chain.

  If `hierarchy = TRUE`, `states` must be a `vector` of length 2. The
  first entry corresponds to the coarse-scale layer, while the second
  entry corresponds to the fine-scale layer.

  By default, `states = 2` if `hierarchy = FALSE` and `states = c(2, 2)`
  if `hierarchy = TRUE`.

- ...:

  Currently not used.

## Value

A `list` of length `1` (or `2` in the hierarchical case). Each element
again is a `list`, containing

- the `"name"` of the distribution

- and a list `"pars"` of its parameters, where unknown parameters are
  set to `NULL`.
