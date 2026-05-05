# Create labels for estimated parameters

This helper function creates labels for the estimated HMM parameters.

## Usage

``` r
parameter_labels(controls, expected_length = NULL)
```

## Arguments

- controls:

  \[`fHMM_controls`\]  
  An object of class `fHMM_controls`.

- expected_length:

  \[`NULL` \| `integer(1)`\]  
  The expected output length. If `NULL` (default), this is not checked.

## Value

A `character` vector of parameter labels.
