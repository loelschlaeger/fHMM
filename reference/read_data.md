# Read data

This helper function reads financial data for the {fHMM} package.

## Usage

``` r
read_data(controls)
```

## Arguments

- controls:

  \[`fHMM_controls`\]  
  An object of class `fHMM_controls`.

## Value

A `list` containing the following elements:

- the `matrix` of the `dates` if `controls$simulated = FALSE` and
  `controls$data$data_column` is specified,

- the `matrix` of the `time_points` if `controls$simulated = TRUE` or
  `controls$data$data_column` is not specified,

- the `matrix` of the empirical `data` used for estimation,

- the `matrix` named `time_series` of empirical data before the
  transformation to log-returns,

- the `vector` of fine-scale chunk sizes `T_star` if
  `controls$hierarchy = TRUE`.
