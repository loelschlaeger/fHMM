# Simulate data

This helper function simulates HMM data.

## Usage

``` r
simulate_hmm(
  controls = list(),
  hierarchy = FALSE,
  states = if (!hierarchy) 2 else c(2, 2),
  sdds = if (!hierarchy) "normal" else c("normal", "normal"),
  horizon = if (!hierarchy) 100 else c(100, 30),
  period = NA,
  true_parameters = fHMM_parameters(controls = controls, hierarchy = hierarchy, states =
    states, sdds = sdds),
  seed = NULL
)
```

## Arguments

- controls:

  \[[`list()`](https://rdrr.io/r/base/list.html) \| `fHMM_controls`\]  
  Either a `list` or an object of class `fHMM_controls`.

  The `list` can contain the following elements, which are described in
  more detail below:

  - `hierarchy`, defines a hierarchical HMM,

  - `states`, defines the number of states,

  - `sdds`, defines the state-dependent distributions,

  - `horizon`, defines the time horizon,

  - `period`, defines a flexible, periodic fine-scale time horizon,

  - `data`, a `list` of controls that define the data,

  - `fit`, a `list` of controls that define the model fitting

  Either none, all, or selected elements can be specified.

  Unspecified parameters are set to their default values.

  Important: Specifications in `controls` always override individual
  specifications.

- hierarchy:

  \[`logical(1)`\]  
  A `logical`, set to `TRUE` for a hierarchical HMM.

  If `hierarchy = TRUE`, some of the other controls must be specified
  for the coarse-scale and the fine-scale layer.

  By default, `hierarchy = FALSE`.

- states:

  \[`integer(1)` \| `integer(2)`\]  
  An `integer`, the number of states of the underlying Markov chain.

  If `hierarchy = TRUE`, `states` must be a `vector` of length 2. The
  first entry corresponds to the coarse-scale layer, while the second
  entry corresponds to the fine-scale layer.

  By default, `states = 2` if `hierarchy = FALSE` and `states = c(2, 2)`
  if `hierarchy = TRUE`.

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

- horizon:

  \[`integer(1)` \| `integer(2)`\]  
  A `numeric`, specifying the length of the time horizon.

  If `hierarchy = TRUE`, `horizon` must be a `vector` of length 2. The
  first entry corresponds to the coarse-scale layer, while the second
  entry corresponds to the fine-scale layer.

  By default, `horizon = 100` if `hierarchy = FALSE` and
  `horizon = c(100, 30)` if `hierarchy = TRUE`.

  If `data` is specified (i.e., not `NA`), the first entry of `horizon`
  is ignored and the (coarse-scale) time horizon is defined by available
  data.

- period:

  \[`NA` \| `character(1)`\]  
  Only relevant if `hierarchy = TRUE`.

  In this case, a `character` which specifies a flexible, periodic
  fine-scale time horizon and can be one of

  - `"w"` for a week,

  - `"m"` for a month,

  - `"q"` for a quarter,

  - `"y"` for a year.

  By default, `period = NA`. If `period` is not `NA`, it overrules
  `horizon[2]`.

- true_parameters:

  \[`NULL` \| `fHMM_parameters`\]  
  An object of class `fHMM_parameters`, used as simulation parameters.
  By default, `true_parameters = NULL`, i.e., sampled true parameters.

- seed:

  \[`NULL` \| `integer(1)`\]  
  Set a seed for the data simulation. No seed by default.

## Value

A `list` containing the following elements:

- `time_points`, the `vector` (or `matrix` in the hierarchical case) of
  time points,

- `markov_chain`, the `vector` (or `matrix` in the hierarchical case) of
  the simulated states,

- `data`, the `vector` (or `matrix` in the hierarchical case) of the
  simulated state-dependent observations,

- `T_star`, the `numeric` vector of fine-scale chunk sizes in the
  hierarchical case

## Examples

``` r
simulate_hmm(states = 2, sdds = "normal", horizon = 10)
#> $time_points
#>  [1]  1  2  3  4  5  6  7  8  9 10
#> 
#> $markov_chain
#>  [1] 2 2 2 1 1 1 1 1 1 1
#> 
#> $data
#>  [1] -0.6585950  1.0541351 -0.3196731 -0.2888282 -0.1801663 -0.3192822
#>  [7] -0.1275880 -0.2889544 -0.3983131 -0.1707813
#> 
#> $T_star
#> NULL
#> 
```
