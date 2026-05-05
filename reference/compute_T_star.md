# Compute lengths of fine-scale chunks

This helper function computes lengths of fine-scale chunks in the
hierarchical case.

## Usage

``` r
compute_T_star(horizon, period, dates = NULL, seed = NULL)
```

## Arguments

- horizon:

  \[`integer(2)`\]  
  The element `controls$horizon`, i.e., an integer vector of length 2,
  where the second entry can be `NA_integer_`.

- period:

  \[`character(1)`\]  
  The element `controls$period`, i.e. one of `"w"`, `"m"`, `"q"`, or
  `"y"`.

- dates:

  \[`NULL` \| [`character()`](https://rdrr.io/r/base/character.html)\]  
  A `character` vector of dates of empirical fine-scale data (if any).
  By default, `dates = NULL`.

- seed:

  \[`NULL` \| `integer(1)`\]  
  Set a seed for the simulation of flexible chunk lengths. By default,
  `seed = NULL` (i.e., no seed).

## Value

An `integer` vector of fine-scale chunk sizes.
