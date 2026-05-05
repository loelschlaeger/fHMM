# Parameter transformations

These helper functions transform model parameters between

- constrained spaces (suffix `*Con`)

- and unconstrained spaces (suffix `*Uncon`).

The former is useful for interpretation, the latter for unconstrained
optimization.

## Usage

``` r
par2parUncon(par, controls, use_parameter_labels = TRUE)

parUncon2parCon(
  parUncon,
  controls,
  use_parameter_labels = TRUE,
  numerical_safeguard = FALSE
)

parCon2par(parCon, controls, use_parameter_labels = TRUE)

par2parCon(par, controls, use_parameter_labels = TRUE)

parCon2parUncon(parCon, controls, use_parameter_labels = TRUE)

parUncon2par(
  parUncon,
  controls,
  use_parameter_labels = TRUE,
  numerical_safeguard = FALSE
)

muCon2muUncon(muCon, link, prefix = "muUncon_", use_parameter_labels = TRUE)

muUncon2muCon(muUncon, link, prefix = "muCon_", use_parameter_labels = TRUE)

sigmaCon2sigmaUncon(
  sigmaCon,
  prefix = "sigmaUncon_",
  use_parameter_labels = TRUE
)

sigmaUncon2sigmaCon(
  sigmaUncon,
  prefix = "sigmaCon_",
  use_parameter_labels = TRUE,
  numerical_safeguard = FALSE
)

dfCon2dfUncon(dfCon, prefix = "dfUncon_", use_parameter_labels = TRUE)

dfUncon2dfCon(
  dfUncon,
  prefix = "dfCon_",
  use_parameter_labels = TRUE,
  numerical_safeguard = FALSE
)

Gamma2gammasCon(
  Gamma,
  prefix = "gammasCon_",
  use_parameter_labels = TRUE,
  numerical_safeguard = FALSE
)

Gamma2gammasUncon(Gamma, prefix = "gammasUncon_", use_parameter_labels = TRUE)

gammasCon2Gamma(gammasCon, dim, prefix = "state_", use_parameter_labels = TRUE)

gammasCon2gammasUncon(
  gammasCon,
  dim,
  prefix = "gammasUncon_",
  use_parameter_labels = TRUE
)

gammasUncon2Gamma(
  gammasUncon,
  dim,
  prefix = "state_",
  use_parameter_labels = TRUE,
  numerical_safeguard = FALSE
)

gammasUncon2gammasCon(
  gammasUncon,
  dim,
  prefix = "gammasCon_",
  use_parameter_labels = TRUE,
  numerical_safeguard = FALSE
)
```

## Arguments

- par:

  \[`fHMM_parameters`\]  
  An object of class
  [`fHMM_parameters`](https://loelschlaeger.de/fHMM/reference/fHMM_parameters.md),
  which is a `list` of model parameters.

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

- use_parameter_labels:

  \[`logical(1)`\]  
  Either `TRUE` to label the parameters or `FALSE`, if not (this can
  save computation time).

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

- numerical_safeguard:

  \[`logical(1)`\]  
  Either `TRUE` or `FALSE`, determining whether to apply the following
  small corrections to boundary parameters to improve numerical
  performance when calculating and optimizing the likelihood function:

  - transition probabilities equal to 0 or 1 are shifted towards the
    center by `1e-3`

  - standard deviations and degrees of freedom are bounded above by
    `100`

- parCon:

  \[`parCon`\]  
  An object of class `parCon`, which is a `numeric` `vector` with
  identified (and constrained) model parameters in the following order:

  1.  non-diagonal transition probabilities `gammasCon`

  2.  expectations `muCon`

  3.  standard deviations `sigmaCon` (if any)

  4.  degrees of freedom `dfCon` (if any)

  5.  fine-scale parameters for each coarse-scale state, in the same
      order (if any)

- muCon, muUncon:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A vector of (un-) constrained expected values.

- link:

  \[`logical(1)`\]  
  Either `TRUE` or `FALSE`, determining whether to apply the link
  function.

- prefix:

  \[`character(1)`\]  
  A `character` prefix for labeling the parameters.

- sigmaCon, sigmaUncon:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A vector of (un-) constrained standard deviations.

- dfCon, dfUncon:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A vector of (un-) constrained degrees of freedom.

- gammasCon, gammasUncon:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A vector of (un-) constrained non-diagonal transition probabilities.

- dim:

  \[`integer(1)`\]  
  An `integer`, the dimension of the transition probability matrix.

## Value

For `par2parUncon`: a vector of unconstrained model parameters.

For `parUncon2parCon`: a vector of constrained model parameters.

For `parCon2par`: an object of class
[`fHMM_parameters`](https://loelschlaeger.de/fHMM/reference/fHMM_parameters.md).

For `par2parCon`: a vector of constrained model parameters.

For `parCon2parUncon`: a vector of unconstrained model parameters.

For `parUncon2par`: an object of class `fHMM_parameters`.

For `muCon2muUncon`: a vector of unconstrained expected values.

For `muUncon2muCon`: a vector of constrained expected values.

For `sigmaCon2sigmaUncon`: a vector of unconstrained standard
deviations.

For `sigmaUncon2sigmaCon`: a vector of constrained standard deviations.

For `dfCon2dfUncon`: a vector of unconstrained degrees of freedom.

For `dfUncon2dfCon`: a vector of constrained degrees of freedom.

For `Gamma2gammasCon`: a vector of constrained non-diagonal matrix
elements (column-wise).

For `Gamma2gammasUncon`: a vector of unconstrained non-diagonal matrix
elements (column-wise).

For `gammasCon2Gamma`: a transition probability matrix.

For `gammasCon2gammasUncon`: a vector of unconstrained non-diagonal
elements of the transition probability matrix.

For `gammasUncon2Gamma`: a transition probability matrix.

For `gammasUncon2gammasCon`: a vector of constrained non-diagonal
elements of a transition probability matrix.
