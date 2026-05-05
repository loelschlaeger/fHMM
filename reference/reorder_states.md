# Reorder estimated states

This function reorders the estimated states, which can be useful for a
comparison to true parameters or the interpretation of states.

## Usage

``` r
reorder_states(x, state_order = "mean")
```

## Arguments

- x:

  \[`fHMM_model`\]  
  An object of class
  [`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md).

- state_order:

  \[`character(1)` \| [`numeric()`](https://rdrr.io/r/base/numeric.html)
  \| [`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  Either

  - `"mean"`, in which case the states are ordered according to the
    means of the state-dependent distributions,

  - or a vector (or a matrix) which determines the new ordering:

    - If `x$data$controls$hierarchy = FALSE`, `state_order` must be a
      vector of length `x$data$controls$states` with integer values from
      `1` to `x$data$controls$states`. If the old state number `x`
      should be the new state number `y`, put the value `x` at the
      position `y` of `state_order`. E.g. for a 2-state HMM, specifying
      `state_order = c(2, 1)` swaps the states.

    - If `x$data$controls$hierarchy = TRUE`, `state_order` must be a
      matrix of dimension `x$data$controls$states[1]` x
      `x$data$controls$states[2] + 1`. The first column orders the
      coarse-scale states with the logic as described above. For each
      row, the elements from second to last position order the
      fine-scale states of the coarse-scale state specified by the first
      element. E.g. for an HHMM with 2 coarse-scale and 2 fine-scale
      states, specifying
      `state_order = matrix(c(2, 1, 2, 1, 1, 2), 2, 3)` swaps the
      coarse-scale states and the fine-scale states connected to
      coarse-scale state 2.

## Value

An object of class
[`fHMM_model`](https://loelschlaeger.de/fHMM/reference/fHMM_model.md),
in which states are reordered.

## Examples

``` r
dax_model_3t_reordered <- reorder_states(dax_model_3t, state_order = 3:1)
```
