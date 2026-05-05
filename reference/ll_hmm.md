# Log-likelihood function of an (H)HMM

This function computes the log-likelihood value of a (hierarchical)
hidden Markov model for given observations and parameter values.

## Usage

``` r
ll_hmm(
  parUncon,
  observations,
  controls = list(),
  hierarchy = FALSE,
  states = if (!hierarchy) 2 else c(2, 2),
  sdds = if (!hierarchy) "normal" else c("normal", "normal"),
  negative = FALSE,
  check_controls = TRUE
)
```

## Arguments

- parUncon:

  \[`parUncon`\]  
  An object of class `parUncon`, which is a `numeric` `vector` with
  identified and unconstrained model parameters in the following order:

  1.  non-diagonal transition probabilities `gammasUncon`

  2.  expectations `muUncon`

  3.  standard deviations `sigmaUncon` (if any)

  4.  degrees of freedom `dfUncon` (if any)

  5.  fine-scale parameters for each coarse-scale state, in the same
      order (if any)

- observations:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  [`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  A `numeric` `vector` of time-series data.

  In the hierarchical case (`hierarchy = TRUE`), a `matrix` with
  coarse-scale data in the first column and corresponding fine-scale
  data in the rows.

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

- negative:

  \[`logical(1)`\]  
  Either `TRUE` to return the negative log-likelihood value (useful for
  optimization) or `FALSE` (default), else.

- check_controls:

  \[`logical(1)`\]  
  Either `TRUE` to check the defined controls or `FALSE` to not check
  them (which saves computation time), else.

## Value

The (negative) log-likelihood value.

## Examples

``` r
### HMM log-likelihood 
controls <- set_controls(states = 2, sdds = "normal")
parameters <- fHMM_parameters(controls)
parUncon <- par2parUncon(parameters, controls)
observations <- 1:10
ll_hmm(parUncon, observations, controls)
#> [1] -268.9179

### HHMM log-likelihood 
controls <- set_controls(
  hierarchy = TRUE, states = c(2, 2), sdds = c("normal", "normal")
)
parameters <- fHMM_parameters(controls)
parUncon <- par2parUncon(parameters, controls)
observations <- matrix(dnorm(110), ncol = 11, nrow = 10)
ll_hmm(parUncon, observations, controls)
#> [1] -70.26436
```
