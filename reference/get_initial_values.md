# Initialization of numerical likelihood optimization

This helper function generates a set of initial values for the numerical
optimization of the model likelihood function.

## Usage

``` r
get_initial_values(
  data,
  ncluster = 1,
  seed = NULL,
  verbose = TRUE,
  initial_estimate = NULL
)
```

## Arguments

- data:

  \[`fHMM_data`\]  
  An object of class
  [`fHMM_data`](https://loelschlaeger.de/fHMM/reference/fHMM_data.md).

- ncluster:

  \[`integer(1)`\]  
  Set the number of clusters for parallel optimization runs to reduce
  optimization time. By default, `ncluster = 1` (no clustering).

- seed:

  \[`NULL` \| `integer(1)`\]  
  Set a seed for the generation of initial values. No seed by default.

- verbose:

  \[`logical(1)`\]  
  Set to `TRUE` to print progress messages.

- initial_estimate:

  \[`NULL` \| `parUncon`\]  
  Optionally defines an initial estimate for the numerical likelihood
  optimization. Good initial estimates can improve the optimization
  process. Can be:

  - `NULL` (the default), in this case

    - applies a heuristic to calculate a good initial estimate

    - or uses the true parameter values (if available and
      `data$controls$origin` is `TRUE`)

  - or an object of class `parUncon` (i.e., a `numeric` of unconstrained
    model parameters), for example the estimate of a previously fitted
    model (i.e. the element `model$estimate`).

## Value

A `list`, where each element is an object of class `parUncon`.
