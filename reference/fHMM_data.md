# Constructor of an `fHMM_data` object

This function constructs an object of class `fHMM_data`, which contains
the financial data for modeling.

## Usage

``` r
fHMM_data(
  dates,
  time_points,
  markov_chain,
  data,
  time_series,
  T_star,
  controls,
  true_parameters
)

# S3 method for class 'fHMM_data'
print(x, ...)

# S3 method for class 'fHMM_data'
summary(object, ...)
```

## Arguments

- dates:

  \[`any`\]  
  The dates in the empirical case.

- time_points:

  \[`any`\]  
  The time points in the simulated case.

- markov_chain:

  \[`any`\]  
  The states in the simulated case.

- data:

  \[`any`\]  
  The data for modeling.

- time_series:

  \[`any`\]  
  The data before transformation.

- T_star:

  \[`NULL` \| [`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The fine-scale chunk sizes.

- controls:

  \[`fHMM_controls`\]  
  The `fHMM_controls` object.

- true_parameters:

  \[`NULL` \| `fHMM_parameters`\]  
  The `fHMM_parameters` object in the simulated case.

- x:

  \[`fHMM_data`\]  
  An object of class `fHMM_data`.

- ...:

  Currently not used.

- object:

  \[`fHMM_data`\]  
  An object of class `fHMM_data`.

## Value

An object of class `fHMM_data`, which is a `list` containing the
following elements:

- The `matrix` of the `dates` if `simulated = FALSE` and
  `controls$data$data_column` is specified,

- the `matrix` of the `time_points` if `simulated = TRUE` or
  `controls$data$data_column` is not specified,

- the `matrix` of the simulated `markov_chain` if `simulated = TRUE`,

- the `matrix` of the simulated or empirical `data` used for estimation,

- the `matrix` `time_series` of empirical data before the transformation
  to log-returns if `simulated = FALSE`,

- the `vector` of fine-scale chunk sizes `T_star` if
  `controls$hierarchy = TRUE`,

- the input `controls`,

- the `true_parameters`.
