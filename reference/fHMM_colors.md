# Set color scheme for visualizations

This helper function defines a color scheme for visualizations in the
{fHMM} package.

## Usage

``` r
fHMM_colors(controls, colors = NULL)
```

## Arguments

- controls:

  \[`fHMM_controls`\]  
  An object of class `fHMM_controls`. It can be created with
  [`set_controls`](https://loelschlaeger.de/fHMM/reference/set_controls.md).

- colors:

  \[`NULL` \| [`character()`](https://rdrr.io/r/base/character.html)\]  
  Either `NULL` (default) or a `character` vector of color names or
  hexadecimal RGB triplets.

## Value

An object of class `fHMM_colors`, which is:

- for `controls$hierarchy == FALSE` a `character` vector of length
  `controls$states` of color codes,

- for `controls$hierarchy == TRUE` a `list` of

  - a `character` vector of length `controls$states[1]` and

  - a `character` matrix of dimensions `controls$states`

  with color codes.

## Examples

``` r
if (FALSE) { # \dontrun{
controls <- set_controls()
fHMM_colors(controls, colors = c("red", "blue"))
} # }
```
