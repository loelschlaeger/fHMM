# Set and check model parameters

This function sets and checks model parameters. Unspecified parameters
are sampled.

## Usage

``` r
fHMM_parameters(
  controls = list(),
  hierarchy = FALSE,
  states = if (!hierarchy) 2 else c(2, 2),
  sdds = if (!hierarchy) "normal" else c("normal", "normal"),
  Gamma = NULL,
  mu = NULL,
  sigma = NULL,
  df = NULL,
  Gamma_star = NULL,
  mu_star = NULL,
  sigma_star = NULL,
  df_star = NULL,
  scale_par = c(1, 1),
  seed = NULL,
  check_controls = TRUE
)

# S3 method for class 'fHMM_parameters'
print(x, ...)
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

- Gamma, Gamma_star:

  \[`NULL` \| [`matrix()`](https://rdrr.io/r/base/matrix.html) \|
  [`list()`](https://rdrr.io/r/base/list.html)\]  
  A transition probability `matrix`.

  It should have dimension `states[1]`.

  `Gamma_star` is a `list` of fine-scale transition probability
  matrices. The `list` must be of length `states[1]`. Each transition
  probability matrix must be of dimension `states[2]`.

- mu, mu_star:

  \[`NULL` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  [`list()`](https://rdrr.io/r/base/list.html)\]  
  A `numeric` vector of expected values for the state-dependent
  distribution in the different states.

  For the gamma- or Poisson-distribution, `mu` must be positive.

  It should have length `states[1]`.

  `mu_star` is a `list` of `vectors` with fine-scale expectations. The
  `list` must be of length `states[1]`. Each `vector` must be of length
  `states[2]`.

- sigma, sigma_star:

  \[`NULL` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  [`list()`](https://rdrr.io/r/base/list.html)\]  
  A positive `numeric` vector of standard deviations for the
  state-dependent distribution in the different states.

  It should have length `states[1]`.

  `sigma_star` is a `list` of `vectors` with fine-scale standard
  deviations. The `list` must be of length `states[1]`. Each vector must
  be of length `states[2]`.

- df, df_star:

  \[`NULL` \| [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  [`list()`](https://rdrr.io/r/base/list.html)\]  
  A positive `numeric` vector of degrees of freedom for the
  state-dependent distribution in the different states.

  It should have length `states[1]`.

  Only relevant in case of a state-dependent t-distribution.

  `df_star` is a `list` of `vectors` with fine-scale degrees of freedom.
  The `list` must be of length `states[1]`. Each vector must be of
  length `states[2]`. Only relevant in case of a fine-scale
  state-dependent t-distribution.

- scale_par:

  \[`numeric(2)`\]  
  A positive `numeric` vector of length two, containing scales for
  sampled expectations and standard deviations.

  The first entry is the scale for `mu` and `sigma`, the second entry is
  the scale for `mu_star` and `sigma_star` (if any).

- seed:

  \[`NULL` \| `integer(1)`\]  
  Sets a seed for the sampling of parameters.

- check_controls:

  \[`logical(1)`\]  
  Either `TRUE` to check the defined controls or `FALSE` to not check
  them (which saves computation time), else.

- x:

  \[`fHMM_parameters`\]  
  An object of class `fHMM_parameters`.

- ...:

  Currently not used.

## Value

An object of class `fHMM_parameters`.

## Details

See the package vignettes at <https://loelschlaeger.de/fHMM/articles/>
for details.

## Examples

``` r
parameters <- fHMM_parameters(states = 2, sdds = "normal")
parameters$Gamma
#>           state_1   state_2
#> state_1 0.7265505 0.2734495
#> state_2 0.1083762 0.8916238
```
